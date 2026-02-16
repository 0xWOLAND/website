{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad ((>=>))
import Hakyll
import Skylighting.Format.HTML (styleToCss)
import Skylighting.Styles (pygments)
import System.FilePath (takeBaseName)
import Text.Pandoc.Options

main :: IO ()
main = hakyll $ do
  match assetPattern $ route idRoute >> compile copyFileCompiler
  match "css/*" $ route idRoute >> compile compressCssCompiler

  create ["css/syntax-light.css"] $ do
    route idRoute
    compile $ makeItem (styleToCss pygments)

  match postsPattern $ do
    route postRoute
    compile $ compilePost >>= applyPostTemplates

  match "sites.md" $ do
    route $ constRoute "sites/index.html"
    compile $ pandocCompiler >>= applyDefaultTemplate defaultContext

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPattern
      let ctx = indexContext posts
      makeItem "" >>= loadAndApplyTemplate "templates/index.html" ctx >>= applyDefaultTemplate ctx

  match "templates/*" $ compile templateBodyCompiler

assetPattern :: Pattern
assetPattern = "images/**" .||. "favicon.svg"

postsPattern :: Pattern
postsPattern = "posts/*"

defaultTemplate :: Identifier
defaultTemplate = "templates/default.html"

applyDefaultTemplate :: Context String -> Item String -> Compiler (Item String)
applyDefaultTemplate ctx = loadAndApplyTemplate defaultTemplate ctx >=> relativizeUrls

applyPostTemplates :: Item String -> Compiler (Item String)
applyPostTemplates =
  loadAndApplyTemplate "templates/post.html" postContext
    >=> applyDefaultTemplate postContext

indexContext :: [Item String] -> Context String
indexContext posts =
  mconcat
    [ listField "posts" postContext (pure posts)
    , constField "title" "\127851\127963\65039"
    , defaultContext
    ]

postContext :: Context String
postContext =
  mconcat
    [ dateField "date" "%B %e, %Y"
    , dateField "isodate" "%Y-%m-%d"
    , defaultContext
    ]

compilePost :: Compiler (Item String)
compilePost = pandocCompilerWith readerOptions writerOptions
  where
    readerOptions =
      defaultHakyllReaderOptions
        { readerExtensions =
            foldr disableExtension
              (readerExtensions defaultHakyllReaderOptions)
              disabledReaderExtensions
        }
    disabledReaderExtensions =
      [ Ext_native_divs
      , Ext_native_spans
      , Ext_markdown_in_html_blocks
      , Ext_implicit_figures
      ]
    writerOptions = defaultHakyllWriterOptions {writerHTMLMathMethod = KaTeX ""}

postRoute :: Routes
postRoute = customRoute $ toPostPath . takeBaseName . toFilePath
  where
    toPostPath slug = "blog/" <> stripDatePrefix slug <> "/index.html"
    stripDatePrefix (_:_:_:_:'-':_:_:'-':_:_:'-':rest) = rest
    stripDatePrefix slug = slug
