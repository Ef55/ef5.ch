{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bibtex
  ( -- * Publications
    parsePublications,
    Publication (..),
    Month (..),

    -- * Rendering
    publicationListHtml,
    publicationsCss,
  )
where

import Clay hiding (code, data_, filter, map, span_, title, type_, url)
import qualified Clay.Size as Size
import qualified Control.Monad as Monad
import Control.Monad.Error.Class (MonadError, throwError)
import Data.Bifunctor (first)
import Data.Char (toLower)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Titlecase (titlecase)
import Icons (codeLink, iconButtonHtml, iconsHtml, linkHtml, moreLink, pdfLink)
import Lucid
  ( Html,
    ToHtml (..),
    a_,
    article_,
    class_,
    data_,
    details_,
    div_,
    h4_,
    header_,
    href_,
    id_,
    onclick_,
    p_,
    span_,
    summary_,
  )
import Markdown (Markdown, markdownInlineHtml, maybeHtml)
import Prelude hiding ((**))
import qualified Text.BibTeX.Entry as Entry
import qualified Text.BibTeX.Parse as Parse
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Shared (stringify)
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.Read (readMaybe)

data Publication = Publication
  { key :: Text,
    venue :: Maybe Text,
    doi :: Maybe Text,
    url :: Maybe Text,
    title :: Text,
    subtitle :: Maybe Text,
    authors :: [Text],
    abstract :: [Text],
    year :: Maybe Int,
    month :: Maybe Month,
    extras :: Maybe Markdown,
    -- | Non-standard BibTeX fields.
    pdf :: Maybe Text,
    code :: Maybe Text,
    more :: Maybe Text,
    -- | The entry as BibTeX, for citing it.
    bibtex :: Maybe Text
  }
  deriving (Show)

parsePublications :: (MonadError [String] m) => String -> m [Publication]
parsePublications s =
  case Parsec.parse (Parse.skippingLeadingSpace Parse.file) "bibliography" s of
    Left err -> throwError [show err]
    Right entries -> either throwError return (mapM toPublication entries)

toPublication :: Entry.T -> Either [String] Publication
toPublication entry = first (map (("Publication " <> Entry.identifier entry <> ": ") <>)) $ do
  title <- required "title" >>= text "title"
  subtitle <- traverse (text "subtitle") (field "subtitle")
  authors <- required "author" >>= mapM (text "author" . Entry.flipName) . Parse.splitAuthorList
  abstract <- maybe (Right []) (paragraphs "abstract") (field "abstract")
  -- PACMPL-style journals put the conference in `number`, proceedings in `series`.
  venue <- traverse (text "venue") (field "number" `orElse` field "series")
  year <- traverse readYear (field "year")
  month <- traverse readMonth (field "month")
  return
    Publication
      { key = Text.pack (Entry.identifier entry),
        venue,
        doi = raw "doi",
        url = raw "url",
        title,
        subtitle,
        authors,
        abstract,
        year,
        month,
        extras = Nothing,
        pdf = raw "pdf",
        code = raw "code",
        more = raw "more",
        bibtex = Just (citation entry)
      }
  where
    fields = Entry.fields (Entry.lowerCaseFieldNames entry)
    field name = lookup name fields
    required name = maybe (Left ["missing field " <> name]) Right (field name)
    -- URLs and identifiers: used as they are.
    raw name = Text.strip . Text.pack <$> field name
    orElse (Just a) _ = Just a
    orElse Nothing b = b

    readYear y = maybe (Left ["invalid year " <> y]) Right (readMaybe y)
    -- 1-12, or the name, in full or abbreviated (month = aug).
    readMonth m = case readMaybe m of
      Just n | n >= 1 && n <= 12 -> Right (toEnum (n - 1))
      _ -> maybe (Left ["invalid month " <> m]) Right (List.find (matches m) [minBound ..])
    matches m month = map toLower m `elem` [name, take 3 name]
      where
        name = map toLower (show month)

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The entry as BibTeX, without the "custom" fields and the abstract
citation :: Entry.T -> Text
citation entry =
  Text.pack $
    "@" <> Entry.entryType entry <> "{" <> Entry.identifier entry <> ",\n"
      <> concatMap fieldLine (filter (keep . fst) (Entry.fields entry))
      <> "}"
  where
    keep name = map toLower name `notElem` ["pdf", "code", "more", "abstract", "shorttitle"]
    fieldLine (name, value) = "  " <> name <> " = " <> fieldValue name value <> ",\n"
    -- Month macros (month = aug) must stay bare for BibTeX to expand them.
    fieldValue name value
      | map toLower name == "month" && map toLower value `elem` monthMacros = value
      | otherwise = "{" <> value <> "}"
    monthMacros = [map toLower (take 3 (show m)) | m <- [minBound .. maxBound :: Month]]

-- | A field's TeX as one line of plain text.
text :: String -> String -> Either [String] Text
text name s = do
  ps <- paragraphs name s
  case ps of
    [] -> Right ""
    [p] -> Right p
    _ -> Left ["unexpected paragraph break in field " <> name]

-- | A field's TeX as paragraphs of plain text, decoded by pandoc's LaTeX
-- reader.
paragraphs :: String -> String -> Either [String] [Text]
paragraphs name s =
  case Pandoc.runPure (Pandoc.readLaTeX Pandoc.def (Text.pack s)) of
    Left err -> Left [Text.unpack (Pandoc.renderError err) <> " in field " <> name]
    Right (Pandoc.Pandoc _ blocks) -> Right (map stringify blocks)

--------------------------------------------------------------------------------
--- Rendering to HTML
--------------------------------------------------------------------------------

-- | A button copying a BibTeX citation to the clipboard (see js/main.js).
citeHtml :: Text -> Html ()
citeHtml bib =
  iconButtonHtml
    "Copy BibTeX"
    "icon-quote"
    [data_ "bibtex" bib, onclick_ "copyBibtex(this)"]

-- | Groups of publications, each under a heading. Styled by `publicationsCss`.
publicationListHtml :: [(String, [Publication])] -> Html ()
publicationListHtml groups =
  div_ [class_ "publications"] (foldMap groupHtml groups)
  where
    groupHtml (name, publications) =
      div_ (h4_ (toHtml name) <> foldMap publicationHtml publications)

publicationHtml :: Publication -> Html ()
publicationHtml (Publication key venue doi url title subtitle authors abstract year month extra pdf code more bibtex) =
  article_ [class_ "publication", id_ key] $ do
    div_ [class_ "line"] $ do
      div_ $ do
        header_ $ do
          let titleHtml = do
                fromString $ titlecase $ Text.unpack title
          let subtitleHtml = do
                maybeHtml
                  (\s -> span_ [class_ "subtitle"] (fromString $ titlecase $ Text.unpack s))
                  subtitle
          span_ [class_ "title"] $ do
            maybe titleHtml (\u -> a_ [href_ u] titleHtml) url
            subtitleHtml
        div_ [class_ "metadata"] $ do
          div_ [class_ "authors"] $ do
            Foldable.fold $ List.intersperse ", " $ fmap styleAuthor authors
      -- Series and links on the right, as for projects.
      div_ [class_ "side-links"] $ do
        span_ [class_ "series"] $
          case venue of
            Just venue ->
              fromString
                ( "["
                    <> Text.unpack venue
                    <> foldMap (\y -> "'" <> show (y `mod` 100)) year
                    <> "]"
                )
            Nothing -> span_ [class_ "draft-tag"] "[Draft]"
        iconsHtml $
          fmap linkHtml (Maybe.catMaybes [pdfLink <$> pdf, codeLink <$> code, moreLink <$> more])
            ++ foldMap (pure . citeHtml) bibtex
    maybeHtml (div_ [class_ "extra"] . markdownInlineHtml) extra
    Monad.unless (List.null abstract) $
      details_ $ do
        summary_ "Abstract"
        div_ [class_ "abstract"] (foldMap (p_ . toHtml) abstract)
  where
    styleAuthor :: Text -> Html ()
    styleAuthor name =
      span_ [class_ (if name == "Noé De Santo" then "author hg" else "author")] (toHtml name)

--------------------------------------------------------------------------------
--- Styling
--------------------------------------------------------------------------------

-- | The publication list's stylesheet. These rules will get scoped under
-- `.publication`.
publicationsCss :: Css
publicationsCss = do
  h4 ? do
    margin (Size.em 1.2) (Size.em 0) (Size.em 0.4) (Size.em 0)
  ".publication" ? do
    marginTop (Size.em 1)
    marginBottom (Size.em 1)
  ".publication" ** header ? do
    fontWeight bold
  ".publication" ** ".side-links" |> ".series" ? do
    fontWeight bold
  ".publication" ** ".subtitle" ? do
    display block
    fontWeight normal
    fontStyle italic
  ".publication" |> details |> ".abstract" |> p # firstChild ? do
    marginTop (Size.em 0)
  ".authors" |> ".author.hg" ? do
    fontWeight bold
    fontStyle italic
    "color" -: "var(--cerulean)"
  ".draft-tag" ? do
    "background" -: "color-mix(in srgb, gray 60%, transparent)"
  ".abstract" ? do
    marginLeft (Size.em 0.1875)
    paddingLeft (Size.em 0.5)
    "border-left" -: "var(--cerulean) solid 1px"
  ".publication" ** summary # hover ? do
    cursor pointer
    "color" -: "var(--cerulean)"
