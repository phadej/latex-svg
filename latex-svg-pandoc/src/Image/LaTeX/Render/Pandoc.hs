{-# LANGUAGE GADTs, OverloadedStrings #-}
module Image.LaTeX.Render.Pandoc (
    -- * SVG
    convertFormulaSVG,
    convertAllFormulaeSVG,
    -- * Separate Files
    convertFormulaFiles,
    convertAllFormulaeFiles,
    -- ** Name Supplies
    NameSupply,
    newNameSupply,
    -- * Options
    PandocFormulaOptions(..),
    ShrinkSize,
    defaultPandocFormulaOptions,
    -- ** Error display functions
    hideError,
    displayError,
    -- * Generalised versions
    -- ** SVG
    convertFormulaSVGWith,
    convertAllFormulaeSVGWith,
    -- ** Files
    convertFormulaFilesWith,
    convertAllFormulaeFilesWith,
    ) where

import Data.IORef             (IORef, modifyIORef, newIORef, readIORef)
import Data.String            (IsString (..))
import Numeric                (showFFloat)
import System.FilePath        ((<.>), (</>))
import Text.Pandoc.Definition (Format (..), Inline (..), MathType (..), Pandoc, nullAttr)
import Text.Pandoc.Walk       (walkM)

import qualified Data.Text as T

import Image.LaTeX.Render

-- | All options pertaining to the actual display of formulae.
data PandocFormulaOptions = PandocFormulaOptions
    { errorDisplay   :: RenderError -> Inline
      -- ^ How to display various errors (such as LaTeX errors). Usually this can just be @displayError@ but you may wish @hideError@
      --   to avoid putting potentially secure information into the output page.
    , formulaOptions :: MathType -> FormulaOptions
      -- ^ LaTeX environment settings, including the preamble, for each equation type (display and inline)
    }

-- | A set of sensible defaults for formula options.
defaultPandocFormulaOptions :: PandocFormulaOptions
defaultPandocFormulaOptions = PandocFormulaOptions
    { errorDisplay   = displayError
    , formulaOptions = fopts
    }
  where
    fopts DisplayMath = displaymath
    fopts InlineMath  = math

-- | Denominator for various dimensions. For high DPI displays, it can be useful to use values of 2 or 4, so that the dimensions
--   of the image are a fraction of the actual image size, and the image appears more crisp. Otherwise, a value of 1 will always
--   produce sensible, if somewhat pixelated results.
type ShrinkSize = Int

-- | Convert a formula in a pandoc document to an image, embedding the image into the HTML using Data URIs.
convertFormulaSVG
    :: EnvironmentOptions           -- ^ System environment settings
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Inline -> IO Inline
convertFormulaSVG = convertFormulaSVGWith . imageForFormula

-- | Convert all formulae in a pandoc document to images, embedding the images into the HTML using Data URIs.
convertAllFormulaeSVG
    :: EnvironmentOptions           -- ^ System environment settings
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Pandoc -> IO Pandoc
convertAllFormulaeSVG e = walkM . convertFormulaSVG e

-- | A generalisation of 'convertFormulaSVG' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaSVGWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> PandocFormulaOptions -- ^ Formula display settings
    -> Inline -> IO Inline
convertFormulaSVGWith f o (Math t s) = do
    res <- f (formulaOptions o t) (toString s)
    case res of
        Left e    -> return $ errorDisplay o e
        Right svg -> return $ RawInline (Format "html") $ fromString $ alterForHTML svg
convertFormulaSVGWith _ _ x = return x

-- | A generalisation of 'convertAllFormulaeSVG' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertAllFormulaeSVGWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> PandocFormulaOptions -- ^ Formula display settings
    -> Pandoc -> IO Pandoc
convertAllFormulaeSVGWith f = walkM . convertFormulaSVGWith f

-- | If we use files for the images, we need some way of naming the image files we produce
--   A NameSupply provides us with a source of unique names via an ever-increasing integer.
--   It's important that any invocation of 'convertFormulaFiles' or 'convertAllFormulaeFiles'
--   that shares the same image storage directory will also use the same name supply, or they
--   will overwrite each others images.
--
--   TODO: remove
type NameSupply = IORef Int

-- | Create a new name supply.
newNameSupply :: IO NameSupply
newNameSupply = newIORef 0

-- | A generalisation of 'convertFormulaFiles' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaFilesWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Inline -> IO Inline
convertFormulaFilesWith f ns bn o (Math t s) = f (formulaOptions o t) (toString s) >>= \res -> case res of
    Left e    -> return $ errorDisplay o e
    Right svg -> do
        let baseline = getBaseline svg
        fn <- readIORef ns
        modifyIORef ns (+1)
        let uri = bn </> show fn <.> "svg"
        writeFile uri svg
        return $ RawInline (Format "html") $ fromString $
            "<img src=\""  ++ uri ++ "\"" ++
            " class="  ++ (case t of InlineMath -> "inline-math"; DisplayMath -> "display-math") ++
            " style=\"margin:0; vertical-align:-" ++ showFFloat (Just 6) baseline "" ++ "pt;\"/>"

convertFormulaFilesWith _ _ _ _ x = return x

-- | Convert a formula in a pandoc document to an image, storing the images in a separate directory.
convertFormulaFiles
    :: EnvironmentOptions           -- ^ System environment settings
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Inline -> IO Inline
convertFormulaFiles = convertFormulaFilesWith . imageForFormula

-- | Convert every formula in a pandoc document to an image, storing the images in a separate directory.
convertAllFormulaeFiles
    :: EnvironmentOptions           -- ^ System environment settings
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Pandoc -> IO Pandoc
convertAllFormulaeFiles eo ns fp = walkM . convertFormulaFiles eo ns fp

-- | A generalisation of 'convertAllFormulaeFiles' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertAllFormulaeFilesWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Pandoc -> IO Pandoc
convertAllFormulaeFilesWith x y a = walkM . convertFormulaFilesWith x y a

-- | Render all errors simply as "Error"
hideError :: RenderError -> Inline
hideError = const $ Str blank
  where
    blank = "Error"

-- | Render errors nicely, in order to show any problems clearly, with all information intact.
displayError :: RenderError -> Inline
displayError (LaTeXFailure str)   = pandocError [Str "LaTeX failed:", LineBreak, Code nullAttr $ fromString str]
displayError (DVISVGMFailure str) = pandocError [Str "DVIPS failed:", LineBreak, Code nullAttr $ fromString str]
displayError (IOException e)      = pandocError [Str "IO Exception:", LineBreak, Code nullAttr $ fromString $ show e]

pandocError :: [Inline] -> Inline
pandocError = Strong . (Emph [Str "Error:"] :)

-------------------------------------------------------------------------------
-- compat
-------------------------------------------------------------------------------

class IsString s => ToString s where
    toString :: s -> String

instance Char ~ c => ToString [c] where
    toString = id

instance ToString T.Text where
    toString = T.unpack
