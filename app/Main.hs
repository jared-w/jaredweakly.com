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

import qualified Data.HashMap.Lazy             as HML
import qualified Data.Text                     as T

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
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post =
    Post { title   :: String
         , content :: String
         , url     :: String
         , date    :: String
         , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo { posts = posts' }
      indexHTML =
        T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath

  postData    <- markdownToHTML . T.pack $ postContent
  let postUrl     = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl

  let fullPostData = withSiteMeta . withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute
    template
    fullPostData
  convert fullPostData

buildCSS :: FilePath -> FilePath -> Action ()
buildCSS src dst = cacheAction ("css" :: T.Text, src) $ do
  liftIO . putStrLn $ "Minimizing CSS: " <> src
  css <- readFile' src
  let c = either (const "") id (minifyCSS $ T.pack css)
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
  allPosts <- buildPosts
  buildIndex allPosts
  copyStaticFiles
  buildStaticFiles

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions { shakeVerbosity = Chatty }
  shakeArgsForward shOpts buildRules
