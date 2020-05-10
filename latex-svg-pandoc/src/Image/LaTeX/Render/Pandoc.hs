{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Image.LaTeX.Render.Pandoc (
    -- * SVG
    convertFormulaSvgInline,
    convertFormulaSvgBlock,
    convertFormulaSvgPandoc,
    -- * Separate Files
    convertFormulaFilesInline,
    convertFormulaFilesBlock,
    convertFormulaFilesPandoc,
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
    convertFormulaSvgInlineWith,
    convertFormulaSvgBlockWith,
    convertFormulaSvgPandocWith,
    -- ** Files
    convertFormulaFilesInlineWith,
    convertFormulaFilesBlockWith,
    convertFormulaFilesPandocWith,
    ) where

import Data.IORef             (IORef, modifyIORef, newIORef, readIORef)
import Data.String            (IsString (..))
import Numeric                (showFFloat)
import System.FilePath        ((<.>), (</>))
import Text.Pandoc.Definition (Block (..), Format (..), Inline (..), MathType (..), Pandoc, nullAttr)
import Text.Pandoc.Walk       (walkM)

import qualified Data.Text as T

import Image.LaTeX.Render

-- | All options pertaining to the actual display of formulae.
data PandocFormulaOptions = PandocFormulaOptions
    { errorDisplay   :: RenderError -> Inline
      -- ^ How to display various errors (such as LaTeX errors). Usually this can just be @displayError@ but you may wish @hideError@
      --   to avoid putting potentially secure information into the output page.
    , formulaOptions :: Maybe MathType -> FormulaOptions
      -- ^ LaTeX environment settings, including the preamble, for each equation type (display and inline)
    }

-- | A set of sensible defaults for formula options.
defaultPandocFormulaOptions :: PandocFormulaOptions
defaultPandocFormulaOptions = PandocFormulaOptions
    { errorDisplay   = displayError
    , formulaOptions = fopts
    }
  where
    fopts (Just DisplayMath) = displaymath
    fopts (Just InlineMath)  = math
    fopts Nothing            = defaultFormulaOptions

-- | Denominator for various dimensions. For high DPI displays, it can be useful to use values of 2 or 4, so that the dimensions
--   of the image are a fraction of the actual image size, and the image appears more crisp. Otherwise, a value of 1 will always
--   produce sensible, if somewhat pixelated results.
type ShrinkSize = Int

-- | Convert a formula in a pandoc document to an image, embedding the image into the HTML using Data URIs.
convertFormulaSvgInline
    :: EnvironmentOptions           -- ^ System environment settings
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Inline -> IO Inline
convertFormulaSvgInline = convertFormulaSvgInlineWith . imageForFormula
--
-- | Convert all formulae in a pandoc block to images, embedding the images into the HTML using Data URIs.
convertFormulaSvgBlock
    :: EnvironmentOptions           -- ^ System environment settings
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Block -> IO Block
convertFormulaSvgBlock = convertFormulaSvgBlockWith . imageForFormula

-- | Convert all formulae in a pandoc document to images, embedding the images into the HTML using Data URIs.
convertFormulaSvgPandoc
    :: EnvironmentOptions           -- ^ System environment settings
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Pandoc -> IO Pandoc
convertFormulaSvgPandoc e = walkM . convertFormulaSvgBlock e

-- | A generalisation of 'convertFormulaSvgBlock' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaSvgBlockWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> PandocFormulaOptions -- ^ Formula display settings
    -> Block -> IO Block
convertFormulaSvgBlockWith f o (RawBlock format s)
    | format == Format "tex" || format == Format "latex"
    = do
        res <- f (formulaOptions o Nothing) (toString s)
        return $ Para $ singleton $ case res of
            Left e    -> errorDisplay o e
            Right svg -> RawInline (Format "html") $ fromString $ alterForHTML svg
convertFormulaSvgBlockWith f o b = walkM (convertFormulaSvgInlineWith f o) b

-- | A generalisation of 'convertFormulaSvgInline' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaSvgInlineWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> PandocFormulaOptions -- ^ Formula display settings
    -> Inline -> IO Inline
convertFormulaSvgInlineWith f o (Math t s) = do
    res <- f (formulaOptions o (Just t)) (toString s)
    return $ case res of
        Left e    -> errorDisplay o e
        Right svg -> RawInline (Format "html") $ fromString $ alterForHTML svg
convertFormulaSvgInlineWith _ _ x = return x

-- | A generalisation of 'convertFormulaSvgPandoc' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaSvgPandocWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> PandocFormulaOptions -- ^ Formula display settings
    -> Pandoc -> IO Pandoc
convertFormulaSvgPandocWith f = walkM . convertFormulaSvgBlockWith f

-- | If we use files for the images, we need some way of naming the image files we produce
--   A NameSupply provides us with a source of unique names via an ever-increasing integer.
--   It's important that any invocation of 'convertFormulaFiles' or 'convertFormulaFiles'
--   that shares the same image storage directory will also use the same name supply, or they
--   will overwrite each others images.
--
--   TODO: remove
type NameSupply = IORef Int

-- | Create a new name supply.
newNameSupply :: IO NameSupply
newNameSupply = newIORef 0


-- | A generalisation of 'convertFormulaFilesInline' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaFilesInlineWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Inline -> IO Inline
convertFormulaFilesInlineWith f ns bn o (Math t s) = f (formulaOptions o (Just t)) (toString s) >>= \res -> case res of
    Left e    -> return $ errorDisplay o e
    Right svg -> makeSvgFile ns bn (Just t) svg

convertFormulaFilesInlineWith _ _ _ _ x = return x

makeSvgFile :: NameSupply -> FilePath -> Maybe MathType -> SVG -> IO Inline
makeSvgFile ns bn t svg = do
    let baseline = getBaseline svg
    fn <- readIORef ns
    modifyIORef ns (+1)
    let uri = bn </> show fn <.> "svg"
    writeFile uri svg
    let classArg = case t of
          Nothing          -> ""
          Just InlineMath  -> " class='inline-math'"
          Just DisplayMath -> " class='display-math'"
    return $ RawInline (Format "html") $ fromString $
        "<img src=\""  ++ uri ++ "\"" ++ classArg ++
        " style=\"margin:0; vertical-align:-" ++ showFFloat (Just 6) baseline "" ++ "pt;\"/>"

-- | A generalisation of 'convertFormulaFilesBlock' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaFilesBlockWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Block -> IO Block
convertFormulaFilesBlockWith f ns bn o (RawBlock format s)
    | format == Format "tex" || format == Format "latex"
    = do
        res <- f (formulaOptions o Nothing) (toString s)
        case res of
            Left e -> return $ Para $ singleton $ errorDisplay o e
            Right svg -> fmap (Para . singleton) $ makeSvgFile ns bn Nothing svg
convertFormulaFilesBlockWith f ns bn o b = walkM (convertFormulaFilesInlineWith f ns bn o) b

-- | Convert a formula in a pandoc document to an image, storing the images in a separate directory.
convertFormulaFilesInline
    :: EnvironmentOptions           -- ^ System environment settings
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Inline -> IO Inline
convertFormulaFilesInline = convertFormulaFilesInlineWith . imageForFormula
--
-- | Convert every formula in a pandoc block to an image, storing the images in a separate directory.
convertFormulaFilesBlock
    :: EnvironmentOptions           -- ^ System environment settings
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Block -> IO Block
convertFormulaFilesBlock = convertFormulaFilesBlockWith . imageForFormula

-- | Convert every formula in a pandoc document to an image, storing the images in a separate directory.
convertFormulaFilesPandoc
    :: EnvironmentOptions           -- ^ System environment settings
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Pandoc -> IO Pandoc
convertFormulaFilesPandoc eo ns fp = walkM . convertFormulaFilesInline eo ns fp

-- | A generalisation of 'convertFormulaFilesPandoc' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaFilesPandocWith
    :: (FormulaOptions -> Formula -> IO (Either RenderError SVG))
       -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
    -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
    -> FilePath                     -- ^ Name of image directory where images will be stored
    -> PandocFormulaOptions         -- ^ Formula display settings
    -> Pandoc -> IO Pandoc
convertFormulaFilesPandocWith x y a = walkM . convertFormulaFilesInlineWith x y a

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

singleton :: a -> [a]
singleton = return
