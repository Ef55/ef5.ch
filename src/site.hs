--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll hiding (pandocCompiler)
import Data.Functor ((<&>))
import Data.Time as Time
import Data.Time.Format as TimeFormat
import Data.Time.LocalTime as LocalTime

import Katex
--------------------------------------------------------------------------------

configuration :: Configuration
configuration = defaultConfiguration {
        providerDirectory = "website"
    }

getCurrentDate :: IO String
getCurrentDate =
    LocalTime.getZonedTime <&>
        TimeFormat.formatTime Time.defaultTimeLocale "%d/%m/%y"

baseContext :: IO (Context String)
baseContext = do
    date <- getCurrentDate
    return (
        constField "siteName" "Noé De Santo" `mappend`
            constField "sourcesRepository" "https://github.com/Ef55/ef5.ch" `mappend`
            constField "buildDate" date `mappend`
            defaultContext)

pandocCompiler :: Compiler (Item String)
pandocCompiler = pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    localKatex

main :: IO ()
main = do
    context <- baseContext
    hakyllWith configuration $ do

        -- Data serving
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "fonts/**" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "katex/katex.min.css" $ do
            route   idRoute
            compile compressCssCompiler
        match "katex/fonts/*" $ do
            route   idRoute
            compile copyFileCompiler

        -- Main pages
        match (fromList ["index.md", "about.md", "cv.html"]) $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= relativizeUrls

        -- Project list
        match "projects/*" $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler
--------------------------------------------------------------------------------

