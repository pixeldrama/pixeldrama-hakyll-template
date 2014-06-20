{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>), mappend, mconcat)
import Hakyll

main :: IO ()
main = hakyll $ do
  
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pages/*.markdown" $ do
      route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "posts/*" $ do
      route $ setExtension ".html"
      compile $ do
        pandocCompiler
          >>= saveSnapshot "content"
          >>= return . fmap demoteHeaders
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" defaultContext

    create ["blog.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let ctx = (listField "posts" postCtx (return posts)) <> postCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/blog.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls


    create ["posts.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let ctx = (listField "posts" postCtx (return posts)) <> postCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/postlist.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
    
    -- read templates
    match "templates/*" $ compile $ templateCompiler
 
    -- images
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "files/*" $ do
      route idRoute
      compile copyFileCompiler

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        loadAllSnapshots "posts/*" "content"
          >>= fmap (take 10) . recentFirst
          >>= renderRss (feedConfiguration "posts") feedCtx

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend` defaultContext

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]


feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle = "pixeldrama - " ++ title
    , feedDescription = "Blog pixeldrama"
    , feedAuthorName = "Benjamin Weißenfels"
    , feedRoot = "http://pixeldrama.de"
    }
