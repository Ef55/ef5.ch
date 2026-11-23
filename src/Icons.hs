module Icons
  ( Link (..),
    pdfLink,
    codeLink,
    moreLink,
    linksHtml,
    linkHtml,
    iconButtonHtml,
    iconsHtml,
  )
where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import Data.Text (Text)
import Lucid (Attributes, Html, a_, button_, class_, href_, span_, type_)
import Lucid.Base (makeAttributes)

-- | Label (tooltip), icon class, URL.
data Link = Link Text Text Text

pdfLink, codeLink, moreLink :: Text -> Link
pdfLink = Link "PDF" "icon-file-text"
codeLink = Link "Repository" "icon-folder-git-2"
moreLink = Link "More" "icon-info"

-- | Links as icons, with the label as tooltip.
linksHtml :: [Link] -> Html ()
linksHtml = iconsHtml . fmap linkHtml

linkHtml :: Link -> Html ()
linkHtml (Link label icon url) =
  a_ ([href_ url] <> iconAttributes label) (iconHtml icon)

-- | A button resembling an icon
iconButtonHtml :: Text -> Text -> [Attributes] -> Html ()
iconButtonHtml label icon attributes =
  button_ ([type_ "button"] <> iconAttributes label <> attributes) (iconHtml icon)

-- | A group of icon links/buttons; nothing if there are none.
iconsHtml :: [Html ()] -> Html ()
iconsHtml icons =
  Monad.unless (null icons) $
    span_ [class_ "links"] (Foldable.fold icons)

iconAttributes :: Text -> [Attributes]
iconAttributes label =
  [ class_ "icon-link",
    makeAttributes "title" label,
    makeAttributes "aria-label" label
  ]

iconHtml :: Text -> Html ()
iconHtml icon = span_ [class_ ("icon " <> icon)] ""
