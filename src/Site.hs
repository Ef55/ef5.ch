--------------------------------------------------------------------------------
import Cv
import Data.Functor ((<&>))
import Data.Monoid (mappend)
import Data.Time as Time
import Data.Time.Format as TimeFormat
import Data.Time.LocalTime as LocalTime
import Hakyll hiding (pandocCompiler)
import Katex
import Text.Pandoc (Extension (..), readerExtensions)
import Text.Pandoc.Extensions (extensionsFromList, pandocExtensions)

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
        `mappend` showTitleField
        `mappend` defaultContext
    )

-- | `titleShown` is set when a page's metadata has `showTitle: true`.
-- This ensures that hakyll's `if` are included iff it is true (as opposed to
-- iff it is set)
showTitleField :: Context String
showTitleField = field "titleShown" $ \item -> do
  value <- getMetadataField (itemIdentifier item) "showTitle"
  if value == Just "true" then return "true" else noResult "showTitle is not true"

pandocCompiler :: Compiler (Item String)
pandocCompiler =
  pandocCompilerWithTransform
    ( defaultHakyllReaderOptions
        { readerExtensions =
            pandocExtensions
              <> extensionsFromList
                [ Ext_footnotes
                ]
        }
    )
    defaultHakyllWriterOptions
    localKatex

main :: IO ()
main = do
  context <- baseContext
  hakyllWith configuration $ do
    -- Data serving
    match "robots.txt" $ do
      route idRoute
      compile copyFileCompiler

    match "favicon.ico" $ do
      route idRoute
      compile copyFileCompiler

    match "js/*" $ do
      route idRoute
      -- TODO: compress
      compile copyFileCompiler

    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "fonts/**" $ do
      route idRoute
      compile copyFileCompiler

    match "icons/**" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "bibliographies/**.bib" $ do
      compile getResourceBody

    match "papers/**.pdf" $ do
      route idRoute
      compile copyFileCompiler

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
