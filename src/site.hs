--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Cv
import Data.Functor ((<&>))
import Data.Monoid (mappend)
import Data.Time as Time
import Data.Time.Format as TimeFormat
import Data.Time.LocalTime as LocalTime
import Hakyll hiding (pandocCompiler)
import Katex

--------------------------------------------------------------------------------

configuration :: Configuration
configuration =
  defaultConfiguration
    { providerDirectory = "website"
    }

getCurrentDate :: IO String
getCurrentDate =
  LocalTime.getZonedTime
    <&> TimeFormat.formatTime Time.defaultTimeLocale "%d/%m/%y"

baseContext :: IO (Context String)
baseContext = do
  date <- getCurrentDate
  return
    ( constField "siteName" "Noé De Santo"
        `mappend` constField "sourcesRepository" "https://github.com/Ef55/ef5.ch"
        `mappend` constField "buildDate" date
        `mappend` defaultContext
    )

pandocCompiler :: Compiler (Item String)
pandocCompiler =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    localKatex

main :: IO ()
main = do
  context <- baseContext
  hakyllWith configuration $ do
    -- Data serving
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "fonts/**" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "katex/katex.min.css" $ do
      route idRoute
      compile compressCssCompiler
    match "katex/fonts/*" $ do
      route idRoute
      compile copyFileCompiler

    -- Main pages
    match (fromList ["index.md", "about.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" context
          >>= relativizeUrls

    create ["cv.html"] $ do
      route $ idRoute
      compile $
        cvCompiler
          >>= loadAndApplyTemplate
            "templates/default.html"
            ( constField "title" "Curriculum Vitae"
                <> constField "inlineCSS" cvCss
                <> context
            )
          >>= relativizeUrls

    -- Project list
    match "projects/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" context
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
