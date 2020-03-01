{-# LANGUAGE InstanceSigs #-}
module Main where

import           Control.Lens                   ( (^?)
                                                , (?~)
                                                , at
                                                )
import           Control.Monad                  ( void
                                                , join
                                                )
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Ord
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics                   ( Generic )
import           Slick
import           Hasmin
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.List                      ( sortOn )

import qualified Data.HashMap.Lazy             as HML
import qualified Data.Text                     as T

import           Pandoc

siteMeta :: SiteMeta
siteMeta = SiteMeta { siteAuthor   = "Jared Weakly"
                    , baseUrl      = "https://jaredweakly.com"
                    , siteTitle    = "Jared Weakly"
                    , redditHandle = Just "jared--w"
                    , githubUser   = Just "jared-w"
                    }

outputFolder :: FilePath
outputFolder = "dist/"

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

-- | Base meta information
data SiteMeta =
    SiteMeta { siteAuthor    :: String
             , baseUrl       :: String
             , siteTitle     :: String
             , redditHandle  :: Maybe String
             , githubUser    :: Maybe String
             }
    deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    , pages :: [Page]
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post =
    Post { title   :: String
         , content :: String
         , url     :: String
         , date    :: String
         , image   :: Maybe String
         , template :: Maybe String
         }
    deriving (Generic, Eq, Show, FromJSON, ToJSON, Binary)

instance Ord Post where
  compare :: Post -> Post -> Ordering
  compare Post { date = a } Post { date = b } = compare (parse a) (parse b)
   where
    parse :: String -> Maybe LocalTime
    parse = parseTimeM True defaultTimeLocale "%Y-%-m-%-d"

-- | Data for a page
data Page =
    Page { title    :: String
         , content  :: String
         , url      :: String
         , template :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

buildDir :: FilePath -> (FilePath -> Action a) -> Action [a]
buildDir p f = getDirectoryFiles "." [p] >>= parallel . map f

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = sortOn Down <$> buildDir "site/blog//*.md" buildPost

-- | Find and build all pages
buildPages :: Action [Page]
buildPages = buildDir "site/pages//*.md" buildPage

buildHTML :: FilePath -> T.Text -> Action ()
buildHTML p html = do
  Stdout result <- cmd (Stdin $ T.unpack html) npx
  writeFileChanged (outputFolder </> p) result
 where
  npx :: String
  npx = concat
    [ "html-minifier --collapse-whitespace"
    , " --remove-comments --remove-redundant-attributes"
    , " --remove-script-type-attributes --remove-style-link-type-attributes"
    , " --sort-attributes --use-short-doctype"
    , " --minify-css true --minify-js true"
    ]

-- | given a list of posts and pages this will build a table of contents
buildIndex :: [Post] -> [Page] -> Action ()
buildIndex posts pages = do
  indexT <- compileTemplate' "site/templates/archive.html"
  let indexInfo = IndexInfo { posts, pages }
      indexHTML = substitute indexT (withSiteMeta $ toJSON indexInfo)

  buildHTML "blog.html" indexHTML

-- | Load a page, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPage :: FilePath -> Action Page
buildPage srcPath = do
  putInfo $ "Rebuilding page: " <> srcPath
  fillTemplate "page"
               srcPath
               (dropDirectory1 . dropDirectory1 $ srcPath -<.> "html")

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = do
  putInfo $ "Rebuilding post: " <> srcPath
  fillTemplate "post" srcPath (dropDirectory1 srcPath -<.> "html")

fillTemplate :: FromJSON b => String -> FilePath -> FilePath -> Action b
fillTemplate def srcPath url = do
  content <- readFile' srcPath
  data'   <- mdToHtml . T.pack $ content
  let u       = "/" </> dropExtension url
  let withUrl = _Object . at "url" ?~ String (T.pack u)

  let fullData = withSiteMeta . withUrl $ data'
      t        = maybe def T.unpack $ fullData ^? key "template" . _String
  template <- compileTemplate' ("site/templates/" <> t -<.> "html")
  buildHTML url (substitute template fullData)
  convert fullData

buildCSS :: FilePath -> FilePath -> Action ()
buildCSS src dst = do
  putInfo $ "Minimizing CSS: " <> src
  css <- readFile' src
  let c = either (error . show) id (minifyCSS $ T.pack css)
  writeFileChanged dst (T.unpack c)

copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "site" ["images//*", "_redirects"]
  void $ forP filepaths $ \f ->
    copyFileChanged ("site" </> f) (outputFolder </> f)

buildStaticFiles :: Action ()
buildStaticFiles = do
  filepaths <- getDirectoryFiles "site" ["css//*"]
  void $ forP filepaths $ \f -> buildCSS ("site" </> f) (outputFolder </> f)

buildRules :: Action ()
buildRules = void $ parallel
  [ join $ buildIndex <$> buildPosts <*> buildPages
  , copyStaticFiles
  , buildStaticFiles
  ]

main :: IO ()
main =
  shakeArgsForward shakeOptions { shakeThreads = 0, shakeLintInside = ["."] }
    $ buildRules
