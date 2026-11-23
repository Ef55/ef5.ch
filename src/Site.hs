--------------------------------------------------------------------------------
import Bibtex (publicationsCss)
import Clay (Css, compact, renderWith, (?))
import Cv
import Data.Functor ((<&>))
import qualified Data.Maybe as Maybe
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Time as Time
import Data.Time.Format as TimeFormat
import Data.Time.LocalTime as LocalTime
import Hakyll
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
    <&> TimeFormat.formatTime Time.defaultTimeLocale "%Y-%m-%d"

baseContext :: IO (Context String)
baseContext = do
  date <- getCurrentDate
  return
    ( constField "siteName" "Noé De Santo"
        `mappend` constField "sourcesRepository" "https://github.com/Ef55/ef5.ch"
        `mappend` constField "buildDate" date
        `mappend` showTitleField
        `mappend` hasMathField
        `mappend` stylesheetsField
        `mappend` publicationsField
        `mappend` defaultContext
    )

-- | `titleShown` is set when a page's metadata has `showTitle: true`.
-- This ensures that hakyll's `if` are included iff it is true (as opposed to
-- iff it is set)
showTitleField :: Context String
showTitleField = field "titleShown" $ \item -> do
  value <- getMetadataField (itemIdentifier item) "showTitle"
  if value == Just "true" then return "true" else noResult "showTitle is not true"

-- | `hasMath` is set when the page contains math rendered by KaTeX (see
-- Katex.hs), so that only those pages load KaTeX's stylesheet.
hasMathField :: Context String
hasMathField = field "hasMath" $ \item ->
  if "katex" `elem` classesIn (itemBody item)
    then return "true"
    else noResult "no math"

-- | Stylesheets of generated components, by the class of their root element.
-- A page gets a component's stylesheet if (and only if) it has an element with
-- that class.
componentStylesheets :: [(String, Css)]
componentStylesheets =
  [ ("publications", publicationsCss),
    ("cv", cvCss)
  ]

-- | Generate this page's custom CSS, based on the components it uses, and
-- some user provided CSS.
stylesheetsField :: Context String
stylesheetsField = listFieldWith "stylesheets" (bodyField "body") $ \item -> do
  let classes = classesIn (itemBody item)
      components =
        [ renderWith compact [] (fromString ('.' : root) ? css)
        | (root, css) <- componentStylesheets,
          root `elem` classes
        ]
  own <- getMetadataField (itemIdentifier item) "inlineCSS"
  mapM makeItem (map LText.unpack components ++ Maybe.maybeToList own)

-- | All the classes used in some HTML.
classesIn :: String -> [String]
classesIn =
  map Text.unpack
    . concatMap (Text.words . Text.takeWhile (/= '"'))
    . drop 1
    . Text.splitOn "class=\""
    . Text.pack

-- | `$publications$` expands to the publication list.
publicationsField :: Context String
publicationsField = field "publications" $ \_ -> publicationsHtml

-- | Markdown pages are first applied as templates (so they can use fields such
-- as `$publications$`; a literal `$` must be written `$$`), then rendered by
-- Pandoc (including Katex).
markdownCompiler :: Context String -> Compiler (Item String)
markdownCompiler context =
  getResourceBody
    >>= applyAsTemplate context
    >>= renderPandocWithTransform
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
    match ("index.md" .||. "research.md") $ do
      route $ setExtension "html"
      compile $
        markdownCompiler context
          >>= loadAndApplyTemplate "templates/default.html" context
          >>= relativizeUrls

    create ["cv.html"] $ do
      route $ idRoute
      compile $
        cvCompiler
          >>= loadAndApplyTemplate
            "templates/default.html"
            ( constField "title" "Curriculum Vitae"
                <> context
            )
          >>= relativizeUrls

    -- Project list
    match "projects/*" $ do
      route $ setExtension "html"
      compile $
        markdownCompiler context
          >>= loadAndApplyTemplate "templates/default.html" context
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
