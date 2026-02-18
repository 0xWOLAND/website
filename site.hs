{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Hakyll
import Skylighting.Format.HTML (styleToCss)
import Skylighting.Styles (pygments)
import System.FilePath (takeBaseName)
import Text.Pandoc.Options

main :: IO ()
main = hakyll $ do
  match assetPattern staticFile
  match "css/*" $ route idRoute >> compile compressCssCompiler

  create ["css/syntax-light.css"] $ do
    route idRoute
    compile $ makeItem (styleToCss pygments)

  match postsPattern $ do
    route postRoute
    compile $ compilePost >>= saveSnapshot "content" >>= applyPostTemplates

  match "sites.md" $ do
    route $ constRoute "sites/index.html"
    compile $ pandocCompiler >>= applyDefaultTemplate defaultContext

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPattern
      let ctx = indexContext posts
      makeItem "" >>= loadAndApplyTemplate "templates/index.html" ctx >>= applyDefaultTemplate ctx

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentPostSnapshots
      renderRss feedConfiguration feedContext posts

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentPostSnapshots
      renderAtom feedConfiguration feedContext posts

  match "templates/*" $ compile templateBodyCompiler

staticFile :: Rules ()
staticFile = route idRoute >> compile copyFileCompiler

assetPattern :: Pattern
assetPattern = "images/**" .||. "favicon.svg"

postsPattern :: Pattern
postsPattern = "posts/*"

applyDefaultTemplate :: Context String -> Item String -> Compiler (Item String)
applyDefaultTemplate = loadAndApplyTemplate "templates/default.html"

applyPostTemplates :: Item String -> Compiler (Item String)
applyPostTemplates item =
  loadAndApplyTemplate "templates/post.html" postContext item
    >>= applyDefaultTemplate postContext

indexContext :: [Item String] -> Context String
indexContext posts =
  listField "posts" postContext (pure posts)
    <> constField "title" "\127851\127963\65039"
    <> defaultContext

postContext :: Context String
postContext =
  field "permalink" (pure . postPermalink . itemIdentifier)
    <> dateField "date" "%B %e, %Y"
    <> dateField "isodate" "%Y-%m-%d"
    <> defaultContext

feedContext :: Context String
feedContext = field "description" (pure . itemBody) <> postContext

recentPostSnapshots :: Compiler [Item String]
recentPostSnapshots = take 10 <$> (recentFirst =<< loadAllSnapshots postsPattern "content")

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "bhar · gav"
    , feedDescription = "Posts from bhargav.wtf"
    , feedAuthorName = "Bhargav"
    , feedAuthorEmail = ""
    , feedRoot = "https://bhargav.wtf"
    }

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
postRoute = customRoute $ \ident -> "blog/" <> postSlug ident <> "/index.html"

postPermalink :: Identifier -> String
postPermalink ident = "/blog/" <> postSlug ident <> "/"

postSlug :: Identifier -> String
postSlug = simplifySlug . stripDatePrefix . takeBaseName . toFilePath

stripDatePrefix :: String -> String
stripDatePrefix slug
  | length slug > 10
      && slug !! 4 == '-'
      && slug !! 7 == '-'
      && slug !! 10 == '-' =
      drop 11 slug
  | otherwise = slug

simplifySlug :: String -> String
simplifySlug slug = case breakOn "-part-" slug of
  Just (base, n) | not (null n) && all isDigit n -> base <> "-" <> n
  _ -> slug
  where
    breakOn needle s
      | needle `isPrefixOf` s = Just ("", drop (length needle) s)
      | null s = Nothing
      | otherwise = do
          (prefix, rest) <- breakOn needle (tail s)
          pure (head s : prefix, rest)
