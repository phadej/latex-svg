-- | This module provides a basic framework to render LaTeX formulae inside Pandoc documents
--   for Hakyll pages.
--
--   See the latex-svg page on GitHub for more information.
--
--      https://github.com/phadej/latex-svg#readme
--
module Hakyll.Contrib.LaTeX (
    initFormulaCompilerSVG,
    initFormulaCompilerSVGPure,
    CacheSize,
    compileFormulaeSVG,
    ) where

import Data.Char              (isSpace)
import Hakyll.Core.Compiler   (Compiler, unsafeCompiler)
import Text.Pandoc.Definition (Pandoc)

import Image.LaTeX.Render
import Image.LaTeX.Render.Pandoc

import qualified Data.Cache.LRU.IO as LRU

-- | Number of formula images to keep in memory during a @watch@ session.
type CacheSize = Integer

-- | Creates a formula compiler with caching. Can be used as in the following minimal example:
--
-- @
-- main = do
--     renderFormulae <- 'initFormulaCompilerSVG' 1000 'defaultEnv"
--     hakyll $
--         match "posts/*.markdown" $ do
--             route $ setExtension "html"
--             compile $ pandocCompilerWithTransformM
--                  defaultHakyllReaderOptions
--                  defaultHakyllWriterOptions
--                  (renderFormulae 'defaultPandocFormulaOptions')
-- @
--
initFormulaCompilerSVG
    :: CacheSize
    -> EnvironmentOptions
    -> IO (PandocFormulaOptions -> Pandoc -> Compiler Pandoc)
initFormulaCompilerSVG cs eo = do
    mImageForFormula <- curry <$> memoizeLru (Just cs) (uncurry drawFormula)
    let eachFormula x y = do
          putStrLn $ "    formula (" ++ environment x ++ ") \"" ++ equationPreview y ++ "\""
          mImageForFormula x y
    return $ \fo -> unsafeCompiler . convertAllFormulaeSVGWith eachFormula fo
  where
    drawFormula x y = do
        putStrLn "      drawing..."
        imageForFormula eo x y
--
-- | Creates a formula compiler. Can be used as in the following minimal example:
--
-- @
-- main = hakyll $ do
--     let renderFormulae = 'initFormulaCompilerSVGPure' 'defaultEnv"
--     match "posts/*.markdown" $ do
--         route $ setExtension "html"
--         compile $ pandocCompilerWithTransformM
--              defaultHakyllReaderOptions
--              defaultHakyllWriterOptions
--              (renderFormulae 'defaultPandocFormulaOptions')
-- @
--
initFormulaCompilerSVGPure
    :: EnvironmentOptions
    -> PandocFormulaOptions -> Pandoc -> Compiler Pandoc
initFormulaCompilerSVGPure eo fo pandoc = do
    let mImageForFormula = drawFormula
    let eachFormula x y = do
          putStrLn $ "    formula (" ++ environment x ++ ") \"" ++ equationPreview y ++ "\""
          mImageForFormula x y

    unsafeCompiler (convertAllFormulaeSVGWith eachFormula fo pandoc)
  where
    drawFormula x y = do
        putStrLn "      drawing..."
        imageForFormula eo x y

-- | A formula compiler that does not use caching, which works in a more drop-in fashion, as in:
--
-- > compile $ pandocCompilerWithTransformM (compileFormulaeSVG defaultEnv defaultPandocFormulaOptions)
--
compileFormulaeSVG
    :: EnvironmentOptions
    -> PandocFormulaOptions
    -> Pandoc -> Compiler Pandoc
compileFormulaeSVG eo po =
    let eachFormula x y = do
          putStrLn $ "    formula (" ++ environment x ++ ") \"" ++ equationPreview y ++ "\""
          putStrLn   "      drawing..."
          imageForFormula eo x y
    in unsafeCompiler . convertAllFormulaeSVGWith eachFormula po

equationPreview :: String -> String
equationPreview x'
    | length x <= 16 = x
    | otherwise      = take 16 $ filter (/= '\n') x ++ "..."
  where
    x = dropWhile isSpace x'

memoizeLru :: Ord a => Maybe Integer -> (a -> IO b) -> IO (a -> IO b)
memoizeLru msize action = do
    lru <- LRU.newAtomicLRU msize
    return $ \arg -> do
        mret <- LRU.lookup arg lru
        case mret of
            Just ret -> return ret
            Nothing -> do
                ret <- action arg
                LRU.insert arg ret lru
                return ret
