{-# LANGUAGE ExtendedDefaultRules #-}
-- {-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Cv where

import Clay hiding (link, location, name, title)
import qualified Clay.Size as Size
import Clay.Stylesheet (Feature (Feature))
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Foldable1 as Foldable1
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text
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
import qualified Text.Pandoc as Pandoc
import Prelude hiding (span, (**))

--------------------------------------------------------------------------------
--- Data representation
--------------------------------------------------------------------------------

newtype Markdown = Markdown Text

markdownToHtml :: Bool -> Markdown -> Html ()
markdownToHtml i (Markdown s) =
  let r = Pandoc.runPure $ do
        md <-
          Pandoc.readMarkdown Pandoc.def s
        let md' = case md of
              Pandoc.Pandoc m [Pandoc.Para p] | i -> Pandoc.Pandoc m [Pandoc.Plain p]
              _ | i -> error $ "Could not render `" <> show s <> "` to inline html."
              _ -> md
        Pandoc.writeHtml5String Pandoc.def md'
   in case r of
        Left err -> error "Pandoc error"
        Right r -> toHtmlRaw r

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
    | ly == ry =
        show lm ++ "-" ++ show rm ++ " " ++ show ly
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

data Publication = Publication
  { venue :: String,
    title :: String,
    link :: Maybe String,
    authors :: [String],
    extra :: Maybe Markdown,
    abstract :: Maybe Markdown
  }

data Experience = Experience
  { jobname :: String,
    period :: Period,
    location :: Markdown,
    supervisors :: [String],
    description :: Markdown
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

maybeHtml :: (a -> Html ()) -> Maybe a -> Html ()
maybeHtml f (Just v) = f v
maybeHtml _ Nothing = pure ()

markdownHtml :: Markdown -> Html ()
markdownHtml = markdownToHtml False

markdownInlineHtml :: Markdown -> Html ()
markdownInlineHtml = markdownToHtml True

linkHtml :: Link -> Html ()
linkHtml (Link label link) =
  a_ [href_ link, class_ "box-link"] $ toHtml label

educationHtml :: Education -> Html ()
educationHtml (Education inst dipls) =
  article_ [class_ "educatin", id_ (fromString $ instname inst)] $ do
    header_ [class_ "line"] $ do
      institutionHtml inst
      periodHtml (Foldable1.foldl1 comb $ fmap snd dipls)
    ul_ (Foldable.foldMap (li_ . diplomaHtml) (NonEmpty.reverse dipls))
  where
    institutionHtml :: Institution -> Html ()
    institutionHtml = span_ . fromString . show

    periodHtml :: Period -> Html ()
    periodHtml = span_ . fromString . show

    diplomaHtml :: (String, Period) -> Html ()
    diplomaHtml (name, period) =
      div_ [class_ "line"] (span_ (fromString name) <> periodHtml period)

publicationHtml :: Publication -> Html ()
publicationHtml (Publication venue title link authors extra abst) =
  article_ [class_ "publication"] $ do
    header_ $ do
      span_ [class_ "series"] $ fromString ("[" <> venue <> "]")
      " "
      span_ [class_ "title"] $
        Maybe.maybe (fromString title) (\l -> a_ [href_ (fromString l)] (fromString title)) link
    div_ [class_ "metadata"] $ do
      div_ [class_ "authors"] $ do
        Foldable.fold $ List.intersperse ", " $ fmap styleAuthor authors
    maybeHtml (div_ [class_ "extra"] . markdownInlineHtml) extra
    maybeHtml
      ( \abst -> details_ $ do
          summary_ "Abstract"
          div_ [class_ "abstract"] (markdownHtml abst)
      )
      abst
  where
    styleAuthor :: String -> Html ()
    styleAuthor name =
      span_ [class_ (if isMe name then "author hg" else "author")] $ fromString name

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

projectHtml :: Project -> Html ()
projectHtml (Project name ctx supers desc links) =
  article_ [class_ "experience"] $ do
    div_ [class_ "line"] $ do
      div_ $ do
        header_ $ toHtml name
        div_ [class_ "metadata"] $ do
          div_ [class_ "context"] (markdownInlineHtml ctx)
          supervisorsHtml supers
      div_ [class_ "links"] $ do
        Foldable.foldMap linkHtml links
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
  ".abstract" ? do
    marginLeft (Size.em 3 @/ 16)
    paddingLeft (Size.em 1 @/ 2)
    "border-left" -: "var(--cerulean) solid 1px"
  summary # hover ? do
    cursor pointer
    "color" -: "var(--cerulean)"

  ".links" ? do
    display flex
    flexDirection column
    "row-gap" -: "0.05em"
  ".links" |> star ? do
    marginLeft auto
  where
    -- .abstract {
    --   margin-left: 0.5rem;
    --   padding-left: 0.5rem;
    --   border-left: var(--cerulean) solid 1px;
    -- }
    -- .publication summary:hover {
    --   cursor: pointer;
    --   color: var(--cerulean);
    -- }

    envyColor :: Text
    envyColor = "var(--envy)"
    transparentEnvyColor :: Text
    transparentEnvyColor = "color-mix(in srgb, var(--envy) 20%, transparent)"

--------------------------------------------------------------------------------
--- Actual CV
--------------------------------------------------------------------------------

education :: [Education]
education =
  [ Education penn $ phd :| [],
    Education epfl $ bachelor :| [master]
  ]
  where
    epfl, penn :: Institution
    epfl = Institution "EPFL" "Lausanne" "Switzerland"
    penn = Institution "University of Pennsylvania" "Philadelphia" "USA"

    bachelor, master, phd :: (String, Period)
    bachelor = ("Bachelor of Science BSc in Computer Science", Period (Year 2018) (Year 2021))
    master = ("Master of Science BSc in Computer Science", Period (Year 2021) (Year 2024))
    phd = ("PhD candidate in Computer Science", Period (Year 2024) Present)

publications :: [Publication]
publications =
  [ warblre,
    warblrePoster
  ]
  where
    warblre =
      Publication
        { venue = "ICFP'24",
          title = "A Coq Mechanization of JavaScript Regular Expression Semantics",
          link = Just "https://doi.org/10.1145/3674666",
          authors = ["Noé De Santo", "Aurèle Barrière", "Clément Pit-Claudel"],
          abstract =
            Just $
              Markdown $
                "We present an executable, proven-safe, faithful, and future-proof Coq mechanization of JavaScript regular expression (regex) matching, as specified by the last published edition of ECMA-262 section 22.2. This is, to our knowledge, the first time that an industrial-strength regex language has been faithfully mechanized in an interactive theorem prover. We highlight interesting challenges that arose in the process (including issues of encoding, corner cases, and executability), and we document the steps that we took to ensure that the result is straightforwardly auditable and that our understanding of the spec aligns with existing implementations."
                  <> "\n\n"
                  <> "We demonstrate the usability and versatility of the mechanization through a broad collection of analyses, case studies, and experiments: we prove that JavaScript regex matching always terminates and is safe (no assertion failures); we identify subtle corner cases that led to mistakes in previous publications; we verify an optimization extracted from a state-of-the-art regex engine; we show that some classic properties described in automata textbooks and used in derivatives-based matchers do not hold in JavaScript regex; and we demonstrate that the cost of updating the mechanization to account for changes in the original specification is reasonably low."
                  <> "\n\n"
                  <> "Our mechanization can be extracted to OCaml and linked with Unicode libraries to produce an executable engine that passes the relevant parts of the official Test262 conformance test suite.",
          extra = Nothing
        }
    warblrePoster =
      Publication
        { venue = "PLDI'24 SRC",
          title = "Mechanized semantics for ECMAScript regexes",
          link = Just "https://systemf.epfl.ch/posters/2024-warblre/",
          authors = ["Noé De Santo"],
          abstract = Nothing,
          extra =
            Just $ Markdown "Poster; [1st place](https://pldi24.sigplan.org/track/pldi-2024-src#winners) (graduate category)."
        }

experiences :: [Experience]
experiences =
  [ Experience
      { jobname = "“Master's Valorization”",
        period = Period (MonthYear March 2024) (MonthYear July 2024),
        location = Markdown "[System F](https://systemf.epfl.ch/) (EPFL), Switzerland.",
        supervisors = ["Dr. Aurèle Barrière", "Prof. Clément Pit-Claudel"],
        description = Markdown "Continued work on my Master’s thesis, [Warblre](https://ef5.ch/projects/warblre.html), a mechanization in Coq of JS regex semantics."
      },
    Experience
      { jobname = "Student Intern",
        period = Period (MonthYear March 2023) (MonthYear August 2023),
        location = Markdown "Oracle Labs, Switzerland.",
        supervisors = ["Dr. Peter Hofer"],
        description = Markdown "Implemented an almost complete support for the new upcoming foreign functions interface of the Java ecosystem in native-image, an ahead-of-time compiler for Java, and SubstrateVM, its companion runtime. The implementation fully supports dynamic library loading, calls from Java to native functions, and calls from native to Java methods."
      },
    Experience
      { jobname = "Research Intern",
        period = Period (MonthYear July 2022) (MonthYear September 2022),
        location = Markdown "[LARA](https://lara.epfl.ch/w/) (EPFL), Switzerland.",
        supervisors = ["Prof. Viktor Kunčak"],
        description = Markdown "Developed a calculus allowing the elimination of pieces of code (called “ghost”) without altering the result or visible effects of a computation. The calculus notably supports both mutable references and subtyping. Also investigated the relation between ghost code and the non-interference property, which comes from the domain of security type systems."
      },
    Experience
      { jobname = "Student Assistant",
        period = Period (Year 2020) (Year 2023),
        location = Markdown "EPFL, Switzerland.",
        supervisors = [],
        description =
          Markdown $
            "For the following classes:\n\n"
              <> "- Formal Verification ([2022](https://isa.epfl.ch/imoniteur_ISAP/!itffichecours.htm?ww_i_matiere=2507044661&ww_x_anneeAcad=2022-2023&ww_i_section=249847), [2023](https://isa.epfl.ch/imoniteur_ISAP/!itffichecours.htm?ww_i_matiere=2507044661&ww_x_anneeAcad=2023-2024&ww_i_section=249847))\n"
              <> "- Computer Language Processing ([2021](https://isa.epfl.ch/imoniteur_ISAP/!itffichecours.htm?ww_i_matiere=1887888591&ww_x_anneeAcad=2021-2022&ww_i_section=249847))\n"
              <> "- Practice of Object-Oriented Programming ([2020](https://isa.epfl.ch/imoniteur_ISAP/!itffichecours.htm?ww_i_matiere=1677965677&ww_x_anneeAcad=2019-2020&ww_i_section=249847), [2021](https://isa.epfl.ch/imoniteur_ISAP/!itffichecours.htm?ww_i_matiere=1677965677&ww_x_anneeAcad=2020-2021&ww_i_section=249847))\n\n"
              <> "Responsibilities included: presenting labs to students, correcting labs and exams, creating and maintaining grading infrastructures, overhauling labs, writing solution sheets."
      }
  ]

projects :: [Project]
projects =
  [ Project
      { projName = "Warblre — Mechanized Semantics for JavaScript Regexes",
        projContext = Markdown "Master thesis done at [System F](https://systemf.epfl.ch/), EPFL.",
        projSupervisors = ["Dr. Aurèle Barrière", "Prof. Clément Pit-Claudel"],
        projDescription = Markdown "Ported the JavaScript regexes specification to the Coq proof assistant. Care was taken to ensure that the ported specification was faithful to the original paper one, and that one could convince oneself of this fact. Proved key properties of the specification, such as termination. Additionally dis-proved some incorrect simplifications found in the literature about these semantics.",
        projLinks = [Link "More" "https://ef5.ch/projects/warblre.html"]
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
        projDescription = Markdown "Implemented an interpreter for System F in the subset of Scala supported by [Stainless](https://github.com/epfl-lara/stainless), a verification tool for Scala. Then proved the main properties of the implemented calculus, most notably that it satisfies progress and preservation. This project used to be the second largest one (in number of verification conditions) done in Stainless. It is also still used as a benchmark for Stainless, as part of the bolts repository.",
        projLinks =
          [ Link "Repository" "https://github.com/Ef55/stainless-stlc",
            Link "Report" "https://github.com/Ef55/Reports-and-presentations/blob/main/stainless_systemf/build/report.pdf"
          ]
      }
  ]

cvCss :: String
cvCss = Text.unpack $ render css

cvCompiler :: Compiler (Item String)
cvCompiler =
  makeItem $
    Text.unpack $
      renderText $
        div_ $
          Foldable.fold $
            List.intersperse
              (hr_ [])
              [ section "Education" educationHtml education,
                section "Publications" publicationHtml publications,
                section "Research & Industry Experience" experienceHtml experiences,
                section "Research & Software Projects" projectHtml projects
              ]
  where
    section :: Text -> (a -> Html ()) -> [a] -> Html ()
    section name f elems =
      div_
        [id_ name]
        (h3_ [class_ "header"] (toHtml name) <> Foldable.foldMap f elems)