{-# LANGUAGE ExtendedDefaultRules #-}
-- {-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Cv where

import Bibtex
import Clay hiding (link, location, name, title, url)
import qualified Clay.Size as Size
import Clay.Stylesheet (Feature (Feature))
import qualified Control.Monad as Monad
import Control.Monad.Error.Class (MonadError (catchError))
import qualified Data.Foldable as Foldable
import qualified Data.Foldable1 as Foldable1
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Text.Titlecase (titlecase)
import Hakyll hiding (Markdown, trim)
import Lucid
  ( Html,
    ToHtml (..),
    a_,
    article_,
    class_,
    details_,
    div_,
    h3_,
    h4_,
    header_,
    hr_,
    href_,
    id_,
    li_,
    renderText,
    span_,
    style_,
    summary_,
    ul_,
  )
import Markdown
import qualified Text.Pandoc as Pandoc
import Prelude hiding (span, (**))

--------------------------------------------------------------------------------
--- Data representation
--------------------------------------------------------------------------------

data Link = Link Text Text

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
  deriving (Eq, Ord)

instance Show Month where
  show m = case m of
    January -> "January"
    February -> "February"
    March -> "March"
    April -> "April"
    May -> "May"
    June -> "June"
    July -> "July"
    August -> "August"
    September -> "September"
    October -> "October"
    November -> "November"
    December -> "December"

data Date
  = Present
  | Year Int
  | MonthYear Month Int

trim :: Date -> Date
trim (MonthYear _ y) = Year y
trim d = d

instance Show Date where
  show Present = "Present"
  show (Year i) = show i
  show (MonthYear m y) = show m ++ " " ++ show y

data Period = Period {from :: Date, to :: Date}

instance Show Period where
  show (Period l@(MonthYear lm ly) r@(MonthYear rm ry))
    | ly == ry = show lm ++ "-" ++ show rm ++ " " ++ show ly
  show (Period l r) = show (trim l) ++ "-" ++ show (trim r)

comb (Period l _) (Period _ r) = Period l r

data Institution = Institution
  { instname :: String,
    city :: String,
    country :: String
  }

instance Show Institution where
  show (Institution name city count) = name ++ " (" ++ city ++ ", " ++ count ++ ")"

data Education = Education
  { institution :: Institution,
    diplomas :: NonEmpty.NonEmpty (String, Period)
  }

data Experience = Experience
  { jobname :: String,
    period :: Period,
    location :: Markdown,
    supervisors :: [String],
    description :: Markdown
  }

data Class = Class
  { className :: String,
    editions :: [Markdown]
  }

data Teaching = Teaching
  { teachingRole :: String,
    teachingPeriod :: Period,
    teachingInstitution :: Institution,
    classes :: [Class]
  }

data Project = Project
  { projName :: String,
    projContext :: Markdown,
    projSupervisors :: [String],
    projDescription :: Markdown,
    projLinks :: [Link]
  }

--------------------------------------------------------------------------------
--- Renderers to HTML
--------------------------------------------------------------------------------

linkHtml :: Link -> Html ()
linkHtml (Link label link) =
  a_ [href_ link] $ toHtml label

boxedLinkHtml :: Link -> Html ()
boxedLinkHtml (Link label link) =
  a_ [href_ link, class_ "box-link"] $ toHtml label

institutionHtml :: Institution -> Html ()
institutionHtml = span_ . fromString . show

educationHtml :: Education -> Html ()
educationHtml (Education inst dipls) =
  article_ [class_ "education", id_ (fromString $ instname inst)] $ do
    header_ [class_ "line"] $ do
      institutionHtml inst
      periodHtml (Foldable1.foldl1 comb $ fmap snd dipls)
    ul_ (Foldable.foldMap (li_ . diplomaHtml) (NonEmpty.reverse dipls))
  where
    periodHtml :: Period -> Html ()
    periodHtml = span_ . fromString . show

    diplomaHtml :: (String, Period) -> Html ()
    diplomaHtml (name, period) =
      div_ [class_ "line"] (span_ (fromString name) <> periodHtml period)

publicationHtml :: Publication -> Html ()
publicationHtml (Publication venue doi url title authors abstract year month extra annote) =
  article_ [class_ "publication"] $ do
    header_ $ do
      span_ [class_ "series"] $
        case venue of
          Just venue ->
            fromString
              ( "["
                  <> Text.unpack venue
                  <> "'"
                  <> show (year `mod` 100)
                  <> "]"
              )
          Nothing -> span_ [class_ "draft-tag"] $ fromString "[Draft]"
      " "
      span_ [class_ "title"] $ a_ [href_ url] (fromString $ titlecase $ Text.unpack title)
    div_ [class_ "metadata"] $ do
      div_ [class_ "authors"] $ do
        Foldable.fold $ List.intersperse ", " $ fmap styleAuthor authors
    -- I don't like to use annote for that, but pandoc/bibtex don't make my life
    -- easy...
    Monad.unless (List.null annote) $ div_ [class_ "links"] $ do
      "["
      Foldable.fold $ List.intersperse "; " (markdownInlineHtml <$> annote)
      "]"
    maybeHtml (div_ [class_ "extra"] . markdownInlineHtml) extra
    maybeHtml
      ( \abst -> details_ $ do
          summary_ "Abstract"
          div_ [class_ "abstract"] (markdownHtml abst)
      )
      abstract
  where
    styleAuthor :: (Text, Text) -> Html ()
    styleAuthor (fname, lname) =
      let name = (Text.unpack fname <> " " <> Text.unpack lname)
       in span_ [class_ (if isMe name then "author hg" else "author")] $ fromString name

    isMe :: String -> Bool
    isMe name = name == "Noé De Santo"

supervisorsHtml :: [String] -> Html ()
supervisorsHtml supervisors =
  Monad.unless (List.null supervisors) $ div_ [class_ "supervisors"] $ do
    "Supervised by " <> toHtml (Foldable.fold $ List.intersperse " & " supervisors)

experienceHtml :: Experience -> Html ()
experienceHtml (Experience name period loc supers desc) =
  article_ [class_ "experience"] $ do
    header_ [class_ "line"] $ do
      span_ $ toHtml name
      span_ $ toHtml (show period)
    div_ [class_ "metadata"] $ do
      div_ [class_ "location"] (markdownInlineHtml loc)
      supervisorsHtml supers
    div_ [class_ "description"] (markdownHtml desc)

teachingHtml :: Teaching -> Html ()
teachingHtml (Teaching role period institution classes) =
  article_ [class_ "teaching"] $ do
    header_ [class_ "line"] $ do
      span_ $ toHtml role
      span_ $ toHtml (show period)
    div_ [class_ "metadata"] $ do
      div_ [class_ "location"] (institutionHtml institution)
    div_ [class_ "classes"] (ul_ (Foldable.foldMap classHtml classes))
  where
    classHtml :: Class -> Html ()
    classHtml (Class name editions) = do
      li_ $ do
        toHtml name
        toHtml " ("
        Foldable.fold (List.intersperse (toHtml ", ") $ markdownHtml <$> editions)
        toHtml ")"

projectHtml :: Project -> Html ()
projectHtml (Project name ctx supers desc links) =
  article_ [class_ "experience"] $ do
    div_ [class_ "line"] $ do
      div_ $ do
        header_ $ toHtml name
        div_ [class_ "metadata"] $ do
          div_ [class_ "context"] (markdownInlineHtml ctx)
          supervisorsHtml supers
      div_ [class_ "side-links"] $ do
        Foldable.foldMap boxedLinkHtml links
    div_ [class_ "description"] (markdownHtml desc)

--------------------------------------------------------------------------------
--- Styling
--------------------------------------------------------------------------------

css :: Css
css = do
  ".line" ? do
    display inlineFlex
    width (Size.pct 100)
  ".line" |> star # firstChild ? do
    flexGrow 1
  ".header" ? do
    textAlign center
  ul ? do
    listStyleType none
    paddingLeft (Size.ex 2)
  ul |> li # before ? do
    content (stringContent "* ")
    fontWeight bold
    display inlineBlock
    width (Size.em 0)
    position relative
    right (Size.em 1)
  li |> star ? do
    display inline
  hr ? do
    margin (Size.em 2) (Size.em 0) (Size.em 2) (Size.em 0)
  ul ? do
    marginTop (Size.ex 0)
    marginBottom (Size.ex 0)

  ".box-link" ? do
    "color" -: envyColor
    "background-color" -: transparentEnvyColor
    fontWeight (weight 500)
    textDecoration none
    padding (Size.px 0) (Size.px 4) (Size.px 0) (Size.px 4)
    borderRadius (Size.px 4) (Size.px 4) (Size.px 4) (Size.px 4)
    width fitContent

  article ? do
    marginTop (Size.em 1)
    marginBottom (Size.em 1)
  article ** header ? do
    fontWeight bold

  ".publication" |> details |> ".abstract" |> p # firstChild ? do
    marginTop (Size.em 0)
  ".authors" |> ".author.hg" ? do
    fontWeight bold
    fontStyle italic
    "color" -: "var(--cerulean)"
  ".draft-tag" ? do
    "background" -: "color-mix(in srgb, gray 60%, transparent)"
  ".abstract" ? do
    marginLeft (Size.em 3 @/ 16)
    paddingLeft (Size.em 1 @/ 2)
    "border-left" -: "var(--cerulean) solid 1px"
  summary # hover ? do
    cursor pointer
    "color" -: "var(--cerulean)"

  ".side-links" ? do
    display flex
    flexDirection column
    "row-gap" -: "0.05em"
  ".side-links" |> a ? do
    marginLeft auto
  where
    envyColor :: Text
    envyColor = "var(--envy)"
    transparentEnvyColor :: Text
    transparentEnvyColor = "color-mix(in srgb, var(--envy) 20%, transparent)"

--------------------------------------------------------------------------------
--- Actual CV
--------------------------------------------------------------------------------

epfl, penn :: Institution
epfl = Institution "EPFL" "Lausanne" "Switzerland"
penn = Institution "University of Pennsylvania" "Philadelphia" "USA"

education :: [Education]
education =
  [ Education penn $ phd :| [],
    Education epfl $ bachelor :| [master]
  ]
  where
    bachelor, master, phd :: (String, Period)
    bachelor = ("Bachelor of Science BSc in Computer Science", Period (Year 2018) (Year 2021))
    master = ("Master of Science MSc in Computer Science", Period (Year 2021) (Year 2024))
    phd = ("PhD student in Computer and Information Science", Period (Year 2024) Present)

mkPublications :: Compiler [(String, [Publication])]
mkPublications = do
  articles <- loadBody "bibliographies/main.bib" >>= parsePublications
  return
    [ ("Articles", articles),
      ("Other publications", [warblrePoster])
    ]
  where
    warblrePoster =
      Publication
        { venue = Just "PLDI-SRC",
          doi = Nothing,
          url = "https://systemf.epfl.ch/posters/2024-warblre/",
          title = "Mechanized semantics for ECMAScript regexes",
          authors = [("Noé", "De Santo")],
          abstract = Nothing,
          year = 2024,
          month = 6,
          extras = Just $ Markdown "Poster; [1st place](https://pldi24.sigplan.org/track/pldi-2024-src#winners) (graduate category).",
          annote = []
        }

experiences :: [Experience]
experiences =
  [ Experience
      { jobname = "“Master's Valorization”",
        period = Period (MonthYear March 2024) (MonthYear July 2024),
        location = Markdown "[System F](https://systemf.epfl.ch/) (EPFL), Switzerland.",
        supervisors = ["Dr. Aurèle Barrière", "Prof. Clément Pit-Claudel"],
        description = Markdown "Continued work on my Master’s thesis, [Warblre](/projects/warblre.html), a mechanization in Rcoq of JavaScript regex semantics."
      },
    Experience
      { jobname = "Student Intern",
        period = Period (MonthYear March 2023) (MonthYear August 2023),
        location = Markdown "Oracle Labs, Switzerland.",
        supervisors = ["Dr. Peter Hofer"],
        description = Markdown "Implemented an almost complete support for the (then) new upcoming foreign functions interface of the Java ecosystem in native-image, an ahead-of-time compiler for Java, and SubstrateVM, its companion runtime. The implementation fully supports dynamic library loading, calls from Java to native functions, and calls from native to Java methods."
      },
    Experience
      { jobname = "Research Intern",
        period = Period (MonthYear July 2022) (MonthYear September 2022),
        location = Markdown "[LARA](https://lara.epfl.ch/w/) (EPFL), Switzerland.",
        supervisors = ["Prof. Viktor Kunčak"],
        description = Markdown "Developed a calculus allowing the elimination of pieces of code (called “ghost”) without altering the result or visible effects of a computation. The calculus notably supports both mutable references and subtyping. Also investigated the relation between ghost code and the non-interference property, which comes from the domain of security type systems."
      }
  ]

teaching :: [Teaching]
teaching =
  [ Teaching
      { teachingRole = "Teaching Assistant",
        teachingPeriod = Period (Year 2025) Present,
        teachingInstitution = penn,
        classes =
          [ Class {
            className = "CIS-5000 Software Foundations",
            editions = [Markdown "[2025](https://www.seas.upenn.edu/~cis5000/current/)"]
          },
            Class
              { className = "CIS-5521 Compilers",
                editions =
                  [ Markdown "[2025](https://www.seas.upenn.edu/~cis5521/current/)"
                  ]
              }
          ]
      },
    Teaching
      { teachingRole = "Student Assistant",
        teachingPeriod = Period (Year 2020) (Year 2023),
        teachingInstitution = epfl,
        classes =
          [ Class
              { className = "CS-550 Formal Verification",
                editions =
                  [ Markdown "[2022](https://gitlab.epfl.ch/lara/cs550/-/tree/2022)",
                    Markdown "[2023](https://gitlab.epfl.ch/lara/cs550/-/tree/2023)"
                  ]
              },
            Class
              { className = "CS-320 Computer Language Processing",
                editions = [Markdown "[2021](https://isa.epfl.ch/imoniteur_ISAP/!itffichecours.htm?ww_i_matiere=1887888591&ww_x_anneeAcad=2021-2022&ww_i_section=249847)"]
              },
            Class
              { className = "CS-108 Practice of Object-Oriented Programming",
                editions =
                  [ Markdown "[2020](https://cs108.epfl.ch/archive/20/)",
                    Markdown "[2021](https://cs108.epfl.ch/archive/21/)"
                  ]
              }
          ]
      }
  ]

projects :: [Project]
projects =
  [ Project
      { projName = "Chicken-Pi — Mixing Rocq and pi-forall",
        projContext = Markdown "Course project for the [Advanced Programming](https://www.seas.upenn.edu/~cis5520/24fa/) class.",
        projSupervisors = [],
        projDescription = Markdown "Extended [pi-forall](https://github.com/sweirich/pi-forall?tab=readme-ov-file) with features taken from the Rocq proof-assistant, such as parametric & indexed datatypes, dependent pattern matching, universes and structural recursion. These features should make it logically sound, effectively transforming pi-forall into a toy proof-assistant.",
        projLinks = [Link "Repository" "https://github.com/Ef55/chicken-pi"]
      },
    Project
      { projName = "Exproc — Computation Expressions for Scala",
        projContext = Markdown "Semester project done at [LAMP](https://www.epfl.ch/labs/lamp), EPFL.",
        projSupervisors = ["Anatolii Kmetiuk"],
        projDescription = Markdown "Implemented an adaptation of F#’s computation expressions in Scala 3 using its meta-programming facilities. Demonstrated how it could be used to make more natural and practical domain specific languages.",
        projLinks = [Link "Repository" "https://github.com/Ef55/scala-expression-processor"]
      },
    Project
      { projName = "Verified System F in Stainless",
        projContext = Markdown "Course project for the [Formal Verification](https://gitlab.epfl.ch/lara/cs550/-/tree/2021) class.",
        projSupervisors = [],
        projDescription = Markdown "Implemented an interpreter for System F in the subset of Scala supported by [Stainless](https://github.com/epfl-lara/stainless), a verification tool for Scala. Then proved the main properties of the implemented calculus, most notably that it satisfies progress and preservation. This project used to be the second largest one (in number of verification conditions) done in Stainless. It is also still used as a benchmark for Stainless, as part of the [bolts](https://github.com/epfl-lara/bolts) repository.",
        projLinks =
          [ Link "Repository" "https://github.com/Ef55/stainless-stlc",
            Link "Report" "https://github.com/Ef55/Reports-and-presentations/blob/main/stainless_systemf/build/report.pdf"
          ]
      }
  ]

cvCss :: String
cvCss = LText.unpack $ render css

cvCompiler :: Compiler (Item String)
cvCompiler = do
  publications <- mkPublications
  makeItem $
    LText.unpack $
      renderText $
        div_ $
          Foldable.fold $
            List.intersperse
              (hr_ [])
              [ section "Education" educationHtml education,
                section "Publications" publicationGroup publications,
                section "Research & Industry Experience" experienceHtml experiences,
                section "Teaching" teachingHtml teaching,
                section "Other Software Projects" projectHtml projects
              ]
  where
    publicationGroup :: (String, [Publication]) -> Html ()
    publicationGroup (name, publications) =
      div_ (h4_ (toHtml name) <> Foldable.foldMap publicationHtml publications)

    section :: Text -> (a -> Html ()) -> [a] -> Html ()
    section name f elems =
      div_
        [id_ name]
        (h3_ [class_ "header"] (toHtml name) <> Foldable.foldMap f elems)