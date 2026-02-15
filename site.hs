{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import System.FilePath            (takeBaseName)
import Text.Pandoc.Highlighting   (breezeDark, pygments, styleToCss)
import Text.Pandoc.Options

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
        compile . makeItem $ syntaxCss

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
            let indexCtx = listField "posts" postCtx (pure posts)
                        <> constField "title" "🍫🏛️"
                        <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html"   indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

postCompiler :: Compiler (Item String)
postCompiler = pandocCompilerWith readerOpts writerOpts

readerOpts :: ReaderOptions
readerOpts = defaultHakyllReaderOptions
    { readerExtensions = foldr disableExtension
        (readerExtensions defaultHakyllReaderOptions)
        [ Ext_native_divs
        , Ext_native_spans
        , Ext_markdown_in_html_blocks
        ]
    }

writerOpts :: WriterOptions
writerOpts = defaultHakyllWriterOptions
    { writerHTMLMathMethod = KaTeX "" }


blogRoute :: Routes
blogRoute = customRoute $ \ident ->
    "blog/" ++ takeBaseName (toFilePath ident) ++ "/index.html"

--------------------------------------------------------------------------------
-- Syntax highlighting

syntaxCss :: String
syntaxCss = lightCss ++ "\n" ++ darkCss
  where
    lightCss = styleToCss pygments
    darkCss  = unlines
        [ ":root.dark " ++ rule
        | rule <- lines (styleToCss breezeDark)
        , "code span" `isPrefixOf` rule
        ]

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix
