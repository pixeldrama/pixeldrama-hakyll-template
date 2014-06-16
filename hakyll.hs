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

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend` defaultContext
