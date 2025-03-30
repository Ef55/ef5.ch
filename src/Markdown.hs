module Markdown where

import Data.Text (Text)
import Lucid
  ( Html,
    ToHtml (..),
  )
import qualified Text.Pandoc as Pandoc

newtype Markdown = Markdown Text
  deriving (Eq, Show)

markdownToHtml :: Bool -> Markdown -> Html ()
markdownToHtml i (Markdown s) =
  let r = Pandoc.runPure $ do
        md <- Pandoc.readMarkdown Pandoc.def s
        let md' = case md of
              Pandoc.Pandoc m [Pandoc.Para p] | i -> Pandoc.Pandoc m [Pandoc.Plain p]
              _ | i -> error $ "Could not render `" <> show s <> "` to inline html."
              _ -> md
        Pandoc.writeHtml5String Pandoc.def md'
   in case r of
        Left err -> error "Pandoc error"
        Right r -> toHtmlRaw r

maybeHtml :: (a -> Html ()) -> Maybe a -> Html ()
maybeHtml f (Just v) = f v
maybeHtml _ Nothing = pure ()

markdownHtml :: Markdown -> Html ()
markdownHtml = markdownToHtml False

markdownInlineHtml :: Markdown -> Html ()
markdownInlineHtml = markdownToHtml True