{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.HashMap.Lazy as HM
import Data.Text (Text)
import Development.Shake (Action)
import Development.Shake.FilePath ((</>))
import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc
import Text.Mustache (substitute)
import Utils (Route (..), compileTemplate', render, toUrl)

writeHtmlRoute :: Route a -> a -> Action ()
writeHtmlRoute r a = Rib.writeRoute r =<< renderTemplate r a

renderTemplate :: Route a -> a -> Action Text
renderTemplate r a = case r of
    RouteHome -> do
        i <- Rib.ribInputDir
        m <- meta <$> Pandoc.parse Pandoc.readMarkdown "pages/index.md"
        y <- compileTemplate' $ i </> "templates/home.html"
        pure $ substitute y m
    RouteAbout -> do
        i <- Rib.ribInputDir
        m <- meta <$> Pandoc.parse Pandoc.readMarkdown "pages/about.md"
        y <- compileTemplate' $ i </> "templates/pages.html"
        pure $ substitute y m
    RouteBlog -> do
        i <- Rib.ribInputDir
        y <- compileTemplate' $ i </> "templates/archive.html"
        let f (r', s) = u (toUrl r') $ meta s
        let ms = object ["posts" .= (f <$> a)]
        pure $ substitute y ms
    (RouteArticle x) -> do
        i <- Rib.ribInputDir
        m <- meta <$> Pandoc.parse Pandoc.readMarkdown x
        y <- compileTemplate' $ i </> "templates/blog.html"
        pure $ substitute y m

u :: Text -> Value -> Value
u t (Object v) = Object $ v <> ("url" .= t)
u _ v = v

meta :: Pandoc.Pandoc -> Value
meta doc = case Pandoc.extractMeta doc of
    Just (Right (Object m)) -> Object $ m <> ("content" .= render doc) <> siteMeta
    _ -> object mempty

siteMeta :: HM.HashMap Text Value
siteMeta =
    HM.fromList
        [ ("siteAuthor", "Jared Weakly")
        , ("baseUrl", "https://jaredweakly.com")
        , ("siteTitle", "Jared Weakly")
        , ("redditHandle", "jared--w")
        , ("githubUser", "jared-w")
        ]
