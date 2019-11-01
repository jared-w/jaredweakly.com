module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                    as A
import           Data.Aeson.Lens
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics                   ( Generic )
import           Slick
import           Hasmin
import           System.Process

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

-- Ridiculous
npx :: String
npx = concat
  [ "npx html-minifier"
  , " --collapse-whitespace"
  , " --remove-comments"
  , " --remove-redundant-attributes"
  , " --remove-script-type-attributes"
  , " --remove-style-link-type-attributes"
  , " --sort-attributes"
  , " --use-short-doctype"
  , " --minify-css true"
  , " --minify-js true"
  ]

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
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

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
buildPosts = buildDir "site/blog//*.md" buildPost

-- | Find and build all pages
buildPages :: Action [Page]
buildPages = buildDir "site/pages//*.md" buildPage

buildHTML :: FilePath -> T.Text -> Action ()
buildHTML p html = do
  -- yolo swaggins assume blindly that I can exec npx html-minifier since
  -- Haskell doesn't have a minifying library for HTML
  -- Further, this isn't POSIX compliant because it uses heredocs
  --
  -- This would've been vaguely nicer if I could've figured out how to use
  -- shake's cmd and pipe things from stdin but meh
  stdout <- liftIO
    $ readCreateProcess (shell $ npx <> " <<< '" <> T.unpack html <> "'") ""
  writeFile' (outputFolder </> p) stdout

-- | given a list of posts and pages this will build a table of contents
buildIndex :: [Post] -> [Page] -> Action ()
buildIndex posts' pages' = do
  indexT <- compileTemplate' "site/templates/archive.html"
  let indexInfo = IndexInfo { posts = posts', pages = pages' }
      indexHTML = substitute indexT (withSiteMeta $ toJSON indexInfo)

  buildHTML "blog/index.html" indexHTML

-- | Load a page, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPage :: FilePath -> Action Page
buildPage srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding page: " <> srcPath
  fillTemplate "page" srcPath (dropDirectory1 . dropDirectory1 $ srcPath)

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  fillTemplate "post" srcPath (dropDirectory1 srcPath)

fillTemplate :: FromJSON b => String -> FilePath -> FilePath -> Action b
fillTemplate def srcPath url = do
  content <- readFile' srcPath
  data'    <- mdToHtml . T.pack $ content
  let u = url -<.> "html"
  let withUrl = _Object . at "url" ?~ String (T.pack u)

  let fullData = withSiteMeta . withUrl $ data'
      t = maybe def T.unpack $ fullData ^? key "template" . _String
  template <- compileTemplate' ("site/templates/" <> t -<.> ".html")
  buildHTML u (substitute template fullData)
  convert fullData

buildCSS :: FilePath -> FilePath -> Action ()
buildCSS src dst = cacheAction ("css" :: T.Text, src) $ do
  liftIO . putStrLn $ "Minimizing CSS: " <> src
  css <- readFile' src
  let c = either (error . show) id (minifyCSS $ T.pack css)
  writeFile' dst (T.unpack c)

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "_redirects"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

buildStaticFiles :: Action ()
buildStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["css//*"]
  void $ forP filepaths $ \filepath ->
    buildCSS ("site" </> filepath) (outputFolder </> filepath)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  join $ buildIndex <$> buildPosts <*> buildPages
  copyStaticFiles
  buildStaticFiles

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions { shakeVerbosity = Chatty }
  shakeArgsForward shOpts buildRules
