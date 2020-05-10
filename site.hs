--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options
import           System.Environment (lookupEnv)
import           Data.Maybe (isJust)
import           Control.Monad


--------------------------------------------------------------------------------
main :: IO ()
main =
  do prod <- isJust <$> lookupEnv "PROD"
     let myDefaultContext = mconcat
                            [ boolField "prod" (const prod)
                            , constField "root" root
                            , defaultContext ]
     hakyll $ do
       match "images/*" $ do
         route   idRoute
         compile copyFileCompiler

       match "css/*" $ do
         route   idRoute
         compile compressCssCompiler

       match (fromList ["CNAME", "favicon.ico", "robots.txt"]) $ do
         route   idRoute
         compile copyFileCompiler

       match "pages/*" $ do
         route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
         compile $ pandocCompiler
           >>= loadAndApplyTemplate "templates/default.html" myDefaultContext
           >>= relativizeUrls

       tags <- buildTags "posts/**" (fromCapture "tags/*.html")
       let myPostCtx = mconcat
                       [ dateField "date" "%B %e, %Y"
                       , tagsField "tags" tags
                       , myDefaultContext ]
       tagsRules tags $ \tag pat -> do
         route idRoute
         compile $ do
           posts <- recentFirst =<< loadAll pat
           let myTagPageCtx = mconcat
                 [ listField "posts" myPostCtx (return posts)
                 , constField "title" $ "Posts tagged \"" ++ tag ++ "\""
                 , boolField "noindex" (pure True)
                 , myDefaultContext ]

           makeItem ""
             >>= loadAndApplyTemplate "templates/tag.html" myTagPageCtx
             >>= loadAndApplyTemplate "templates/default.html" myTagPageCtx
             >>= relativizeUrls

       match "posts/**" $ do
         route $ setExtension "html"
         compile $ customPandocCompiler
           >>= saveSnapshot "content"
           >>= loadAndApplyTemplate "templates/post.html"    myPostCtx
           >>= loadAndApplyTemplate "templates/default.html" myPostCtx
           >>= relativizeUrls

       create ["archive.html"] $ do
         route idRoute
         compile $ do
           posts <- recentFirst =<< loadAll "posts/**"
           tagList <- renderTagList tags
           let myArchiveCtx =
                 listField "posts" myPostCtx (return posts) `mappend`
                 constField "taglist"  tagList              `mappend`
                 constField "title" "Archives"              `mappend`
                 myDefaultContext

           makeItem ""
             >>= loadAndApplyTemplate "templates/archive.html" myArchiveCtx
             >>= loadAndApplyTemplate "templates/default.html" myArchiveCtx
             >>= relativizeUrls

       create ["sitemap.xml"] $ do
         route   idRoute
         compile $ do
           posts <- recentFirst =<< loadAll "posts/*"
           pages <- loadAll "pages/*"
           let allPages = return (pages ++ posts)
           let sitemapCtx = mconcat
                            [ listField "pages" myPostCtx allPages
                            , myDefaultContext
                            ]
           makeItem ""
             >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

       create ["rss.xml"] $ do
         route idRoute
         compile $ do
           let feedCtx = mconcat
                         [ teaserField "teaser" "content"
                         , bodyField "description"
                         , myPostCtx
                         ]
               absolutizeUrl u = if isExternal u then u else root ++ u
           posts <- fmap (take 10) . recentFirst =<<
             loadAllSnapshots "posts/*" "content"
           processedPosts <- forM posts $
             \p -> do pp <- loadAndApplyTemplate "templates/rss-description.html" feedCtx p
                      return $ fmap (withUrls absolutizeUrl) pp
           renderRss myFeedConfiguration feedCtx processedPosts

       match "index.html" $ do
         route idRoute
         compile $ do
           posts <- fmap (take 5) . recentFirst =<< loadAllSnapshots "posts/*" "content"
           let myTeaserPostCtx =
                 teaserField "teaser" "content" <> myPostCtx
               myIndexCtx = mconcat
                            [ listField "posts" myTeaserPostCtx (return posts)
                            , constField "canonical" (root ++ "/")
                            , constField "homepage" "yes"
                            , myDefaultContext ]

           getResourceBody
             >>= applyAsTemplate myIndexCtx
             >>= loadAndApplyTemplate "templates/default.html" myIndexCtx
             >>= relativizeUrls

       match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

root :: String
root = "https://blog.thjread.com"

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  let defaultReaderExtensions = readerExtensions defaultHakyllReaderOptions
      readerOptions = defaultHakyllReaderOptions {
        readerExtensions = enableExtension Ext_tex_math_single_backslash defaultReaderExtensions
        }

      defaultWriterExtensions = writerExtensions defaultHakyllWriterOptions
      writerOptions = defaultHakyllWriterOptions {
        writerExtensions = enableExtension Ext_tex_math_single_backslash defaultWriterExtensions,
          writerHTMLMathMethod = MathJax ""
        }
  in pandocCompilerWith readerOptions writerOptions

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Thomas Read's blog"
    , feedDescription = "My personal blog - mostly maths and programming"
    , feedAuthorName  = "Thomas Read"
    , feedAuthorEmail = "thjread@gmail.com"
    , feedRoot        = root
    }
