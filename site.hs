--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Contrib.LaTeX
import           Image.LaTeX.Render
import           Image.LaTeX.Render.Pandoc
import           Text.Pandoc.Options
import           Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = do renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv
          hakyll $ do
              match "images/*" $ do
                  route   idRoute
                  compile copyFileCompiler

              match "css/*" $ do
                  route   idRoute
                  compile compressCssCompiler

              match (fromList ["about.rst", "contact.markdown"]) $ do
                  route   $ setExtension "html"
                  compile $ pandocCompiler
                      >>= loadAndApplyTemplate "templates/default.html" defaultContext
                      >>= relativizeUrls

              match "posts/*" $ do
                  route $ setExtension "html"
                  compile $ customPandocCompiler renderFormulae
                      >>= loadAndApplyTemplate "templates/post.html"    postCtx
                      >>= loadAndApplyTemplate "templates/default.html" postCtx
                      >>= relativizeUrls

              create ["archive.html"] $ do
                  route idRoute
                  compile $ do
                      posts <- recentFirst =<< loadAll "posts/*"
                      let archiveCtx =
                              listField "posts" postCtx (return posts) `mappend`
                              constField "title" "Archives"            `mappend`
                              defaultContext

                      makeItem ""
                          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                          >>= relativizeUrls


              match "index.html" $ do
                  route idRoute
                  compile $ do
                      posts <- recentFirst =<< loadAll "posts/*"
                      let indexCtx =
                              listField "posts" postCtx (return posts) `mappend`
                              defaultContext

                      getResourceBody
                          >>= applyAsTemplate indexCtx
                          >>= loadAndApplyTemplate "templates/default.html" indexCtx
                          >>= relativizeUrls

              match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
customPandocCompiler :: (PandocFormulaOptions -> Pandoc -> Compiler Pandoc) -> Compiler (Item String)
customPandocCompiler renderFormulae =
   let defaultReaderExtensions = readerExtensions defaultHakyllReaderOptions
       readerOptions = defaultHakyllReaderOptions {
         readerExtensions = enableExtension Ext_tex_math_single_backslash defaultReaderExtensions
       }

       defaultWriterExtensions = writerExtensions defaultHakyllWriterOptions
       writerOptions = defaultHakyllWriterOptions {
         writerExtensions = enableExtension Ext_tex_math_single_backslash defaultWriterExtensions
       }
       preamble = "\\usepackage{amsmath} \\usepackage{amssymb} \\usepackage{enumerate} \\usepackage{diffcoeff}"
       displayMathOptions = displaymath {
         preamble = preamble
       }
       mathOptions = math {
         preamble = preamble
       }
       formulaOptions = defaultPandocFormulaOptions {
         formulaOptions = \case DisplayMath -> displayMathOptions; _ -> mathOptions
       }
   in pandocCompilerWithTransformM readerOptions writerOptions
          (renderFormulae formulaOptions)
