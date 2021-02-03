{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Utils where

import Control.Monad (void, (<=<))
import Control.Monad.Except (liftEither, runExcept)
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import Development.Shake
import Development.Shake.MinifyCSS (minifyCSSAction)
import GHC.Generics (Generic)
import Lucid
import Lucid.Base (makeAttribute)
import Main.Utf8 ()
import Rib (IsRoute, Pandoc)
import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc
import System.FilePath (dropExtension, takeDirectory, (-<.>), (</>))
import Text.Mustache (Template (ast), localAutomaticCompile)
import Text.Mustache.Compile (getPartials)
import Text.Pandoc (Block (CodeBlock), Inline (Code, Link), Pandoc (..), ReaderOptions (readerExtensions), WriterOptions (writerExtensions), def, runPure, writeHtml5String)
import Text.Pandoc.Extensions
import Text.Pandoc.Walk (Walkable (query), walk)

data Route a where
    RouteHome :: Route ()
    RouteAbout :: Route Pandoc
    RouteBlog :: Route [(Route Pandoc, Pandoc)]
    RouteArticle :: FilePath -> Route Pandoc

instance IsRoute Route where
    routeFile =
        pure . \case
            RouteHome -> "index.html"
            RouteAbout -> "about.html"
            RouteBlog -> "blog" </> "index.html"
            RouteArticle srcPath -> srcPath -<.> ".html"

routeMeta :: forall a. Route a -> a -> SrcMeta
routeMeta route a = case route of
    RouteArticle _ -> getMeta a
    _ -> SrcMeta{title = routeTitle route a, description = mempty, date = mempty}

routeTitle :: forall a. Route a -> a -> Text
routeTitle route a = case route of
    RouteHome -> "Jared<br/>Weakly"
    RouteAbout -> "About Me"
    RouteBlog -> "Posts"
    RouteArticle _ -> title $ getMeta a

isHome :: Route a -> Bool
isHome RouteHome = True
isHome _ = False

whenHome :: Route a -> b -> Maybe b
whenHome rt y = case rt of RouteHome -> Just y; _ -> Nothing

td :: Text -> Text
td d = T.pack $ takeDirectory (T.unpack d)

(</./>) :: Text -> Text -> Text
a </./> b = T.pack $ T.unpack a </> T.unpack b

transformLinks :: Text -> Inline -> Inline
transformLinks root (Link attrs t (T.stripPrefix "./" <=< T.stripSuffix ".md" -> Just u, t')) =
    Link attrs t (td root </./> u, t')
transformLinks root (Link attrs t (T.stripSuffix ".md" -> Just u, t')) =
    Link attrs t (td root </./> u, t')
transformLinks _ x = x

filterLinks :: Text -> Pandoc -> Pandoc
filterLinks root (Pandoc meta blocks) =
    Pandoc meta $
        walk (transformLinks root) blocks

-- | Metadata in our markdown sources
data SrcMeta = SrcMeta
    { title :: Text
    , description :: Maybe Text
    , date :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: Pandoc -> SrcMeta
getMeta src = case Pandoc.extractMeta src of
    Nothing -> error "No YAML metadata"
    Just (Left e) -> error $ T.unpack e
    Just (Right val) -> case fromJSON val of
        Aeson.Error e -> error $ "JSON error: " <> e
        Aeson.Success v -> v

toText :: Monad f => Pandoc -> f Text
toText doc = fmap T.toStrict $ Lucid.renderTextT $ Pandoc.render doc

-- | Render a Pandoc document to Text
render :: Pandoc -> Text
render doc =
    either (error . show) id
        . runExcept
        . (liftEither . runPure)
        $ writeHtml5String writerSettings doc

-- from Rib
exts :: Extensions
exts =
    mconcat
        [ extensionsFromList
            [ Ext_yaml_metadata_block
            , Ext_fenced_code_attributes
            , Ext_auto_identifiers
            , Ext_smart
            ]
        , githubMarkdownExtensions
        ]

readerSettings :: ReaderOptions
readerSettings = def{readerExtensions = exts}

writerSettings :: WriterOptions
writerSettings = def{writerExtensions = exts}

-- writeLucidHtmlRoute :: Route a -> a -> Action ()
-- writeLucidHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r

-- writeTypeHtmlRoute :: Route a -> a -> Action ()
-- writeTypeHtmlRoute r a =
--   let x = H.renderText (page r a)
--    in Rib.writeRoute r x

css_ :: Applicative m => Text -> HtmlT m ()
css_ h = link_ [href_ h, rel_ "stylesheet"]

prel_ :: Applicative m => Text -> HtmlT m ()
prel_ h = link_ [rel_ "preload", makeAttribute "as" "style", href_ h]

html5 :: Monad m => HtmlT m () -> HtmlT m () -> HtmlT m ()
html5 hd bd = doctypehtml_ $
    html_ [lang_ "en"] $ do
        head_ $ do
            meta_ [charset_ "UTF-8"]
            meta_ [name_ "viewport", content_ "width_ device-width, initial-scale_ 1.0"]
            meta_ [httpEquiv_ "X-UA-Compatible", content_ "ie_ edge"]
            hd
        bd

-- | Like 'compileTemplate' from <http://hackage.haskell.org/package/mustache mustache> but tracks changes to template files and partials within Shake for cache-busting.
compileTemplate' :: FilePath -> Action Template
compileTemplate' fp = do
    need [fp]
    result <- liftIO $ localAutomaticCompile fp
    case result of
        Right templ -> do
            need (getPartials . ast $ templ)
            pure templ
        Left err -> fail $ show err

generateSite :: (forall a. Route a -> a -> Action ()) -> Action ()
generateSite writeHtmlRoute = do
    (i, d) <- (,) <$> Rib.ribInputDir <*> Rib.ribOutputDir
    Rib.buildStaticFiles ["static/**", "images/**", "_redirects"]
    void . Rib.forEvery ["css/syntax.css", "css/home.css"] $ \f ->
        minifyCSSAction (i </> f) (d </> f)
    void . Rib.forEvery ["css/reset.css", "css/scale.css", "css/base.css"] $ \f ->
        minifyCSSAction (i </> f) (d </> "temp-css" </> f)
    css' <- parallel $ (\f -> liftIO $ readFile (d </> "temp-css/css" </> f)) <$> ["reset.css", "scale.css", "base.css"]
    Rib.writeFileCached "css/base.css" (concat css')
    liftIO $ removeFiles d ["temp-css"]

    writeHtmlRoute RouteHome ()
    writeHtmlRoute RouteAbout
        <$> filterLinks (Rib.routeUrl RouteAbout)
        =<< Pandoc.parse Pandoc.readMarkdown "pages/about.md"
    articles <-
        Rib.forEvery ["blog/*.md"] $ \srcPath -> do
            let r = RouteArticle srcPath
            doc <- filterLinks (Rib.routeUrl r) <$> Pandoc.parse Pandoc.readMarkdown srcPath
            writeHtmlRoute r doc
            pure (r, doc)
    writeHtmlRoute RouteBlog articles

-- void $
--     getDirectoryFiles "" [d </> "**/*.html"] >>= \fps -> forP fps $ \fp ->
--         cmd_ ("npx subfont -i --formats woff2 --root " <> d :: String) [fp]

hasCode :: Pandoc -> Bool
hasCode = (||) <$> (or . query go) <*> (or . query go')
    where
        go' = \case
            CodeBlock _ _ -> [True]
            _ -> [False]
        go = \case
            Code _ _ -> [True]
            _ -> [False]

toUrl :: Route r -> Text
toUrl = T.pack . dropExtension . T.unpack . Rib.routeUrl
