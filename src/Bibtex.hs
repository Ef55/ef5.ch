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

data Publication = Publication
  { venue :: Maybe Text,
    doi :: Maybe Text,
    url :: Text,
    title :: Text,
    authors :: [(Text, Text)],
    abstract :: Maybe Markdown,
    year :: Int,
    month :: Int,
    extras :: Maybe Markdown,
    annote :: [Markdown]
  }
  deriving (Show)

parsePublications :: (MonadError [String] m) => String -> m [Publication]
parsePublications s =
  case run of
    Left err -> throwError $ return $ unpack $ Pandoc.renderError err
    Right Nothing -> throwError $ return "Parsing of publication failed."
    Right (Just r) -> return r
  where
    run = Pandoc.runPure $ do
      Pandoc.Pandoc bib _ <- Pandoc.readBibTeX Pandoc.def (pack s)
      let Just (Pandoc.MetaList m) = Pandoc.lookupMeta (pack "references") bib
      return $ mapM toPublication m

toPublication :: Pandoc.MetaValue -> Maybe Publication
toPublication m = do
  -- ["abstract","author","container-title","doi","id","issue","issued","keyword","publisher","publisher-place","title","type","url","volume"]
  (year, month) <- getTimestamp
  Publication
    (getFieldText "issue" >>= toPlain)
    (getFieldString "doi")
    <$> getFieldString "url"
    <*> (getFieldText "title" >>= toPlain)
    <*> getAuthors
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

    getTimestamp :: Maybe (Int, Int)
    getTimestamp = do
      ts <- getFieldString "issued"
      let (y, m) = breakOn "-" ts
      return (read $ unpack y, read $ unpack m)