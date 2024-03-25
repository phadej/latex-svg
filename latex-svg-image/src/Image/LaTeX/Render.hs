{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Image.LaTeX.Render (
    -- * Rendering Formulas
    imageForFormula,
    renderToFile,
    Formula, SVG,
    -- * BaseLine
    BaseLine, getBaseline, alterForHTML,
    -- * Errors
    RenderError (..),
    -- * Options
    -- ** Environment Options
    EnvironmentOptions (..),
    defaultEnv,
    TempDirectoryHandling (..),
    -- ** Formula Options
    FormulaOptions (..),
    displaymath,
    math, tikz,
    defaultFormulaOptions,
    ) where

import Control.Applicative        (some, (<|>))
import Control.DeepSeq            (NFData (..), ($!!))
import Control.Monad              (when)
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE, withExceptT)
import Data.Bitraversable         (bimapM)
import Data.Char                  (isSpace)
import Data.List                  (foldl', isPrefixOf, sortOn, stripPrefix)
import Data.Maybe                 (fromMaybe, maybeToList)
import Numeric                    (showFFloat)
import System.Exit                (ExitCode (..))
import System.FilePath            ((<.>), (</>))
import System.IO.Temp             (withSystemTempDirectory, withTempDirectory)

-- import System.IO

import qualified Control.Exception          as E
import qualified Crypto.Hash.SHA256         as SHA256
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as BS8
import qualified System.Directory           as Dir
import qualified System.Process             as Proc
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as P

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A LaTeX formula, e.g @x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}@ for the quadratic formula. Do not include any @$@s to denote the environment, just
--   specify the environment in the 'FormulaOptions' instead.
type Formula = String

-- | Number of points (@pt@) from the bottom of the image to the typesetting baseline. Useful for setting your formulae inline with text
type BaseLine = Double

-- | A source of 'SVG' image.
type SVG = String

-- | This type contains all possible errors than can happen while rendering an equation.
--   It includes all IO errors that can happen as well as more specific errors.
data RenderError
    = LaTeXFailure String        -- ^ @latex@ returned a nonzero error code
    | DVISVGMFailure String      -- ^ @dvisvgm@ returned a nonzero error code
    | IOException E.IOException  -- ^ An 'IOException' occurred while managing the temporary files used to convert the equation
  deriving (Show, Eq)

data TempDirectoryHandling
    = UseSystemTempDir String  -- ^ A temporary directory with a name based on the given template will be created in the system temporary files location
    | UseCurrentDir    String  -- ^ A temporary directory with a name based on the given template will be created in the current directory
  deriving (Eq, Show, Read, Ord)

data EnvironmentOptions = EnvironmentOptions
    { latexCommand     :: String                  -- ^ Command to use for @latex@, default is @latex@
    , dvisvgmCommand   :: String                  -- ^ Command to use for @dvisvgm@, default is @dvisvgm@
    , latexArgs        :: [String]                -- ^ Any additional arguments for @latex@
    , dvisvgmArgs      :: [String]                -- ^ Any additional arguments for @dvisvgm@
    , latexFontSize    :: Int                     -- ^ Document font size, one of @8,9,10,11,12,14,17,20@. If not @10,11,12@ then @extarticle@ document size is used.
    , tempDir          :: TempDirectoryHandling   -- ^ How to handle temporary files
    , tempFileBaseName :: String                  -- ^ The base name to use for the temporary files.
    , globalCache      :: Bool                    -- ^ Cache outputs globally in @XDG_CACHE/latex-svg@
    }
  deriving (Eq, Show, Read, Ord)

data FormulaOptions = FormulaOptions
    { preamble    :: String        -- ^ LaTeX preamble to use. Put your @\usepackage@ commands here.@ commands here.
    , environment :: Maybe String  -- ^ LaTeX environment in which the equation will be typeset, usually 'math' or 'displaymath'
    }
  deriving (Eq, Show, Read, Ord)

-------------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------------

-- | Use the @amsmath@, @amsfonts@ and @lmodern@ packages.
defaultFormulaOptions :: FormulaOptions
defaultFormulaOptions = FormulaOptions
    { environment = Nothing
    , preamble = concat
        [ "\\usepackage{amsmath}"
        , "\\usepackage{amsfonts}"
        , "\\usepackage[T1]{fontenc}"
        , "\\usepackage{lmodern}"
        ]
    }

-- | Use the @amsmath@, @amsfonts@ and @lmodern@ package, the @displaymath@ environment.
displaymath :: FormulaOptions
displaymath = defaultFormulaOptions { environment = Just "displaymath" }

-- | Use the @amsmath@, @amsfonts@ and @lmodern@ package, the @math@ environment.
math :: FormulaOptions
math = defaultFormulaOptions { environment = Just "math" }

-- | Use the @tikzpicture@ environment, optionally providing more tikz libraries
-- to add to the preamble.
tikz :: [String] -> FormulaOptions
tikz libraries = defaultFormulaOptions {
    environment = Just "tikzpicture"
  , preamble = preamble defaultFormulaOptions
            ++ "\\usepackage{tikz}"
            ++ mconcat [ "\\usetikzlibrary{" ++ l ++ "}" | l <- libraries ]
  }

-- | Sensible defaults for system environments. Works if @dvisvgm@ and @latex@ are recent enough and in your @$PATH@.
defaultEnv :: EnvironmentOptions
defaultEnv = EnvironmentOptions
    { latexCommand     = "latex"
    , dvisvgmCommand   = "dvisvgm"
    , latexArgs        = []
    -- "--exact-bbox" is good idea if you have recent dvisvgm
    , dvisvgmArgs      = ["--no-fonts=1", "--clipjoin", "--bbox=min", "--exact"]
    , latexFontSize    = 12
    , tempDir          = UseSystemTempDir "latex-eqn-temp"
    , tempFileBaseName = "working"
    , globalCache      = False
    }

-------------------------------------------------------------------------------
-- Image for formula
-------------------------------------------------------------------------------

-- | Convert a formula into a SVG image.
imageForFormula :: EnvironmentOptions -> FormulaOptions -> Formula -> IO (Either RenderError SVG)
imageForFormula EnvironmentOptions {..} FormulaOptions {..} eqn =
    withTemp $ \temp -> runExceptT $ do
        let doc :: String
            doc = unlines $
                [ "% " ++ latexCommand ++ " " ++ show latexArgs
                , "% " ++ dvisvgmCommand ++ " " ++ show dvisvgmArgs
                , "\\nonstopmode"
                , documentClassDeclaration
                , "\\pagestyle{empty}"
                , preamble
                , "\\begin{document}"
                ] ++
                [ "\\begin{" ++ e ++ "}" | e <- maybeToList environment ] ++
                filter (not . all isSpace) (lines eqn) ++
                [ "\\end{" ++ e ++ "}"  | e <- maybeToList environment ] ++
                [ "\\end{document}"
                ]

        cached doc $ do
            -- io $ hPutStrLn stderr doc
            io $ writeFile (temp </> tempFileBaseName <.> "tex") doc

            (c,o,e) <- io $ readProcessWithCWD temp latexCommand $ latexArgs ++ [tempFileBaseName <.> "tex"]
            when (c /= ExitSuccess) $ throwE $ LaTeXFailure (o ++ "\n" ++ e)

            (c',o',e') <- io $ readProcessWithCWD temp dvisvgmCommand $ dvisvgmArgs ++ ["-o", tempFileBaseName <.> "svg", tempFileBaseName <.> "dvi"]
            when (c' /= ExitSuccess) $ throwE $ DVISVGMFailure (o' ++ "\n" ++ e')

            svg <- io $ readFile (temp </> tempFileBaseName <.> "svg")

            return $ addTitle eqn svg
  where
    documentClassDeclaration = mconcat
      [ "\\documentclass["
      , show latexFontSize'
      , "pt,preview,dvisvgm"
      , if isTikz then ",tikz" else ""
      , "]{"
      , documentClass
      , "}"
      ]

    latexFontSize'
        | latexFontSize < 8  = 8
        | latexFontSize > 20 = 20
        | otherwise          = head $ sortOn (\s -> abs (s - latexFontSize)) sizes

    sizes :: [Int]
    sizes = [8,9,10,11,12,14,17,20]

    isTikz = environment == Just "tikzpicture"

    documentClass
        | isTikz                           = "standalone"
        | latexFontSize' `elem` [10,11,12] = "article"
        | otherwise                        = "extarticle"

    io :: NFData a => IO a -> ExceptT RenderError IO a
    io = withExceptT IOException . tryIO

    withTemp a = case tempDir of
        UseSystemTempDir f -> withSystemTempDirectory f a
        UseCurrentDir f    -> withTempDirectory "." f a

    cached :: String -> ExceptT RenderError IO String -> ExceptT RenderError IO String
    cached doc action
        | not globalCache = action
        | otherwise       = do
            let key :: String
                key = filter (/= '=')
                    $ BS8.unpack
                    $ Base64.encode
                    $ SHA256.hashlazy
                    $ B.toLazyByteString
                    $ B.stringUtf8 doc

            -- cache directory
            xdgCache <- io $ Dir.getXdgDirectory Dir.XdgCache "latex-svg"
            io $ Dir.createDirectoryIfMissing True xdgCache
            let path = xdgCache </> key <.> "svg"

            readFile path `orElse` do
                result <- action
                io $ writeFile path result
                return result

    orElse :: IO a -> ExceptT e IO a -> ExceptT e IO a
    orElse lft rgt = ExceptT $ fmap Right lft `E.catch` handler rgt

    handler :: ExceptT e IO a -> E.IOException -> IO (Either e a)
    handler rgt _ = runExceptT rgt

-- | Render a formula and produce a file at the given filepath.
renderToFile :: FilePath
             -> EnvironmentOptions
             -> FormulaOptions
             -> Formula
             -> IO (Either RenderError ())
renderToFile fp env opts formula =
  bimapM pure (writeFile fp) =<< imageForFormula env opts formula

-------------------------------------------------------------------------------
-- Baseline and other postprocessing
-------------------------------------------------------------------------------

addTitle :: Formula -> String -> String
addTitle eqn svg =
    let (x0,x1) = spanL "<svg" svg
        (y1,y2) = spanR '>' x1
    in x0 ++ y1 ++ "\n<title>" ++ processAltString eqn ++ "</title>" ++ y2

spanL :: Eq a => [a] -> [a] -> ([a], [a])
spanL sep = go where
    go str@[]                  = (str, str)
    go str@(c:sfx)
        | sep `isPrefixOf` str = ([], str)
        | otherwise            = (c:xs , ys)
      where
        ~(xs,ys) = go sfx

spanR :: Eq a => a -> [a] -> ([a], [a])
spanR sep = go where
    go  str@[]      = (str, str)
    go _str@(c:sfx)
        | sep == c  = ([c], sfx)
        | otherwise = (c:xs , ys)
      where
        ~(xs,ys) = go sfx

processAltString :: String -> String
processAltString = concatMap $ \c -> case c of
    '<'  -> "&lt;"
    '>'  -> "&gt;"
    '&'  -> "&amp;"
    '"'  -> "&quot;"
    '\'' -> "&39;"
    '\n' -> " "
    '\r' -> " "
    '\t' -> " "
    x    -> [x]

getBaseline :: SVG -> Double
getBaseline str = getBaseline' sfx
  where
    (_pfx, sfx) = spanL viewboxMarker str

-- | Alter 'SVG' image to be embeddable in HTML page, i.e.align baseline.
--
-- * Add @style="vertical-align: baseline-correction"@
-- * Remove @id="page1"@
--
alterForHTML :: SVG -> SVG
alterForHTML xml =
    pfx ++ " style='vertical-align: " ++ showFFloat (Just 6) baseline "" ++ "pt'" ++ stripId sfx
  where
    (_,  svg)   = spanL "<svg" xml
    (pfx, sfx) = spanL viewboxMarker svg
    baseline   = getBaseline' sfx

viewboxMarker :: String
viewboxMarker = " viewBox='"

stripId :: String -> String
stripId str = pfx ++ sfx'
  where
    (pfx, sfx) = spanL needle str
    sfx'       = fromMaybe sfx (stripPrefix needle sfx)

    needle = "id='page1'"

getBaseline' :: String -> Double
getBaseline' sfx = case P.parse parser "<input>" sfx of
    Left err -> error $ show (err, sfx)
    Right x  -> negate x
  where
    parser :: P.Parser Double
    parser = do
        _ <- P.string viewboxMarker
        _ <- lexeme double
        _ <- P.spaces
        x <- lexeme double
        _ <- lexeme double
        y <- lexeme double
        return (y + x)

    double :: P.Parser Double
    double = sign <*> (float1 <|> float2)

    float1 :: P.Parser Double
    float1 = do
        d <- decimal
        f <- P.option 0 (P.char '.' *> fraction)
        return (d + f)

    float2 :: P.Parser Double
    float2 = P.char '.' *> fraction

    decimal :: P.Parser Double
    decimal = foldl' (\x d ->  10*x + digitToInt d) 0
        <$> digits1

    fraction :: P.Parser Double
    fraction = uncurry (/) . foldl' (\(x,n) d -> (10*x + digitToInt d,n*10)) (0,1)
        <$> digits1

    digits1 = some P.digit

    digitToInt '0' = 0
    digitToInt '1' = 1
    digitToInt '2' = 2
    digitToInt '3' = 3
    digitToInt '4' = 4
    digitToInt '5' = 5
    digitToInt '6' = 6
    digitToInt '7' = 7
    digitToInt '8' = 8
    digitToInt '9' = 9
    digitToInt _   = 0

    sign :: P.Parser (Double -> Double)
    sign = P.option id (negate <$ P.char '-')

    lexeme :: P.Parser a -> P.Parser a
    lexeme p = p <* P.spaces

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

readProcessWithCWD
    :: FilePath                     -- ^ working  directory
    -> FilePath                     -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                     -- ^ any arguments
    -> IO (ExitCode,String,String)  -- ^ exitcode, stdout, stderr
readProcessWithCWD cwd cmd args = Proc.readCreateProcessWithExitCode
    ((Proc.proc cmd args) { Proc.cwd = Just cwd })
    ""

-- | Catch 'IOException's and convert them to the 'ExceptT' monad
tryIO :: (MonadIO m, NFData a) => IO a -> ExceptT E.IOException m a
tryIO action = ExceptT $ liftIO $ E.try $ evaluateDeep action

-- | Internal helper function
evaluateDeep :: NFData a => IO a -> IO a
evaluateDeep action = do
    res <- action
    E.evaluate $!! res
