{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Hakyll

baseHeaderLevel :: Int
baseHeaderLevel = 3

main :: IO ()
main =
  hakyllWith (defaultConfiguration {deployCommand = "rsync -av _site/ us:html"}) $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "js/*" $ do
      route idRoute
      compile getResourceString
    match (fromList ["about.md", "contact.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompilerWithTransform
          defaultHakyllReaderOptions
          (defaultHakyllWriterOptions
           {writerHtml5 = True, writerHighlight = False})
          (headerShift (baseHeaderLevel - 1)) >>=
        loadAndApplyTemplate "templates/post.html" postCtx >>=
        saveSnapshot "content" >>=
        loadAndApplyTemplate "templates/default.html" postCtx >>=
        relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend` defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateCompiler
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "purelyfunctional.org"
    , feedDescription = "Functional programming and formal methods"
    , feedAuthorName  = "Moritz Kiefer"
    , feedAuthorEmail = "moritz.kiefer@purelyfunctional.org"
    , feedRoot        = "http://purelyfunctional.org"
    }
