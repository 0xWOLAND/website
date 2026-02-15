{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hakyll
import System.FilePath (takeBaseName)
import Text.Pandoc.Highlighting (pygments, styleToCss)
import Text.Pandoc.Options

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match ("images/**" .||. "favicon.svg") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ makeItem (styleToCss pygments)

    match "posts/*" $ do
        route   blogRoute
        compile $ postCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "sites.md" $ do
        route   $ constRoute "sites/index.html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx =
                    mconcat
                        [ listField "posts" postCtx (pure posts)
                        , constField "title" "\127851\127963\65039"
                        , defaultContext
                        ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html"   ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- | Post metadata: both a display date and a compact ISO date for the index.

postCtx :: Context String
postCtx =
    mconcat
        [ dateField "date" "%B %e, %Y"
        , dateField "isodate" "%Y-%m-%d"
        , defaultContext
        ]

--------------------------------------------------------------------------------
-- | Pandoc compiler with raw HTML passthrough and KaTeX math rendering.

postCompiler :: Compiler (Item String)
postCompiler = pandocCompilerWith readerOpts writerOpts
  where
    readerOpts =
        defaultHakyllReaderOptions
            { readerExtensions =
                foldr disableExtension baseReaderExtensions disabledExtensions
            }
    baseReaderExtensions = readerExtensions defaultHakyllReaderOptions
    disabledExtensions =
        [ Ext_native_divs
        , Ext_native_spans
        , Ext_markdown_in_html_blocks
        ]
    writerOpts = defaultHakyllWriterOptions
        { writerHTMLMathMethod = KaTeX "" }

--------------------------------------------------------------------------------
-- | Route @YYYY-MM-DD-slug.md@ to @blog\/slug\/index.html@, preserving
--   clean URLs while keeping posts sorted by filename on disk.

blogRoute :: Routes
blogRoute = customRoute $
    format . stripDatePrefix . takeBaseName . toFilePath
  where
    stripDatePrefix (_:_:_:_:'-':_:_:'-':_:_:'-':slug) = slug
    stripDatePrefix slug = slug
    format slug    = "blog/" <> slug <> "/index.html"
