module Bibtex (parsePublications, Publication (..)) where

import Control.Monad.Error.Class (MonadError (catchError), throwError)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.Map as Map
import Data.Text (Text, breakOn, dropEnd, pack, strip, unpack)
import Markdown (Markdown (Markdown))
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Readers.BibTeX
import qualified Text.Pandoc.Writers as Writers
import Control.Applicative ((<|>))
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Text.Pandoc.Shared (stringify)

data Publication = Publication
  { venue :: Maybe Text,
    doi :: Maybe Text,
    url :: Maybe Text,
    title :: Text,
    subtitle :: Maybe Text,
    authors :: [(Text, Text)],
    abstract :: Maybe Markdown,
    year :: Maybe Int,
    month :: Maybe Int,
    extras :: Maybe Markdown,
    annote :: [Markdown]
  }
  deriving (Show)

parsePublications :: (MonadError [String] m) => String -> m [Publication]
parsePublications s =
  case run of
    Left err -> throwError $ return $ unpack $ Pandoc.renderError err
    Right (Left errs) -> throwError errs
    Right (Right r) -> return r
  where
    run = Pandoc.runPure $ do
      Pandoc.Pandoc bib _ <- Pandoc.readBibTeX Pandoc.def (pack s)
      return $ case Pandoc.lookupMeta (pack "references") bib of
        Just (Pandoc.MetaList m) -> mapM parseEntry m
        Nothing -> Right []
        Just _ -> Left ["Unexpected bibliography structure."]

    -- Report which entry failed
    parseEntry :: Pandoc.MetaValue -> Either [String] Publication
    parseEntry m = case toPublication m of
      Just p -> Right p
      Nothing ->
        Left
          [ "Parsing of publication "
              <> maybe "<unknown id>" unpack (entryId m)
              <> " failed (it needs at least a title and authors)."
          ]

    entryId :: Pandoc.MetaValue -> Maybe Text
    entryId (Pandoc.MetaMap m) = case m Map.!? pack "id" of
      Just (Pandoc.MetaString i) -> Just i
      Just (Pandoc.MetaInlines is) -> Just (stringify is)
      _ -> Nothing
    entryId _ = Nothing

toPublication :: Pandoc.MetaValue -> Maybe Publication
toPublication m = do
  let (year, month) = getTimestamp
  (title, subtitle) <- getTitle
  Publication
    (getFieldText "issue" <|> getFieldText "collection-title" >>= toPlain)
    (getFieldString "doi")
    (getFieldString "url")
    title
    subtitle
    <$> getAuthors
    <*> return (getFieldText "abstract" >>= toMarkdown)
    <*> return year
    <*> return month
    <*> return Nothing
    <*> getAnnote
  where
    Pandoc.MetaMap m' = m

    rightToMaybe m = case m of
      Left _ -> Nothing
      Right v -> Just v

    toPandoc :: [Pandoc.Block] -> Pandoc.Pandoc
    toPandoc = Pandoc.Pandoc (Pandoc.Meta Map.empty)

    toPlain :: Pandoc.Pandoc -> Maybe Text
    toPlain m = strip <$> rightToMaybe (Pandoc.runPure $ Writers.writePlain Pandoc.def m)

    toMarkdown :: Pandoc.Pandoc -> Maybe Markdown
    toMarkdown m = Markdown <$> rightToMaybe (Pandoc.runPure $ Writers.writeMarkdown Pandoc.def m)

    getAnnote :: Maybe [Markdown]
    getAnnote = do
      Pandoc.MetaInlines str <- m' Map.!? pack "annote"
      r <- rightToMaybe $ Pandoc.runPure $ mapM (Writers.writeMarkdown Pandoc.def . toPandoc . return . Pandoc.Para . return) str
      return (Markdown <$> r)

    getFieldText :: String -> Maybe Pandoc.Pandoc
    getFieldText name = do
      Pandoc.MetaInlines str <- m' Map.!? pack name
      return $ toPandoc (Pandoc.Para <$> toList (asParagraphs str))

    asParagraphs :: [Pandoc.Inline] -> NonEmpty [Pandoc.Inline]
    asParagraphs [] = [] :| []
    asParagraphs (h : t) =
      let b :| bs = asParagraphs t
       in case h of
            Pandoc.LineBreak -> [] :| b : bs
            _ -> (h : b) :| bs

    getFieldString :: String -> Maybe Text
    getFieldString name = do
      Pandoc.MetaString str <- m' Map.!? pack name
      return str

    getAuthors :: Maybe [(Text, Text)]
    getAuthors = do
      Pandoc.MetaList ls <- m' Map.!? pack "author"
      mapM getAuthor ls

    getAuthor :: Pandoc.MetaValue -> Maybe (Text, Text)
    getAuthor (Pandoc.MetaMap m) = do
      Pandoc.MetaString first <- m Map.!? pack "given"
      Pandoc.MetaString last <- m Map.!? pack "family"
      return (first, last)

    -- Pandoc merges BibTeX's `subtitle` into `title` ("Title: Subtitle"), but
    -- keeps the main title as `title-short`, which we use to split them again.
    -- (Pandoc also derives `title-short` from a colon in `title`, so such
    -- titles get split too.)
    getTitle :: Maybe (Text, Maybe Text)
    getTitle = do
      full <- normalize <$> (getFieldText "title" >>= toPlain)
      let short = normalize <$> (getFieldText "title-short" >>= toPlain)
      return $ case short >>= \s -> (,) s <$> Text.stripPrefix s full of
        Just (s, rest)
          | Just sub <- Text.stripPrefix ":" rest,
            not (Text.null (strip sub)) ->
              (s, Just (strip sub))
        _ -> (full, Nothing)

    -- The plain writer wraps long lines.
    normalize :: Text -> Text
    normalize = Text.unwords . Text.words

    -- "issued" is "YYYY" or "YYYY-MM" (and absent if there is no year).
    getTimestamp :: (Maybe Int, Maybe Int)
    getTimestamp = case getFieldString "issued" of
      Nothing -> (Nothing, Nothing)
      Just ts ->
        let (y, m) = breakOn "-" ts
         in (readMaybe (unpack y), readMaybe (unpack (Text.drop 1 m)))