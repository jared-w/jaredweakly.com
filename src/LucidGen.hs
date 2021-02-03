{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LucidGen where

import Control.Monad (forM_, when)
import qualified Data.Text as T
import Development.Shake (Action)
import Development.Shake.FilePath
import Lucid
import Lucid.Base (makeAttribute, makeXmlElementNoEnd)
import qualified Rib

import Utils

writeHtmlRoute :: Route a -> a -> Action ()
writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r

renderPage :: Route a -> a -> Html ()
renderPage route val =
    let rt = routeTitle route val
        hd = do
            link_ [rel_ "dns-prefetch", href_ "//fonts.googleapis.com"]
            link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com/", crossorigin_ ""]
            prel_ "https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400;0,600;1,400;1,600&display=swap"
            prel_ "/css/syntax.css"
            css_ "/css/reset.css"
            css_ "/css/scale.css"
            css_ "/css/base.css"

            meta_ [name_ "description", content_ "Jared Weakly"]
            meta_ [name_ "author", content_ "Jared Weakly"]
            link_ [rel_ "icon", type_ "image/x-icon", href_ "/images/favicon.ico"]
            meta_ [name_ "twitter:card", content_ "summary_large_image"]
            meta_ [name_ "twitter:title", content_ rt]
            meta_ [name_ "twitter:description", content_ ("Jared Weakly - " <> rt)]
            when (isHome route) $ link_ [href_ "/css/home.css", rel_ "stylesheet"]
            title_ (toHtmlRaw rt)
     in html5 hd $ do
            body_ [class_ "cover" | isHome route] $ do
                header_ $ do
                    a_ [href_ "#main"] "Skip to main content"
                    nav_ [class_ "wrapper"] $ do
                        ul_ [class_ "top-navigation"] $ do
                            li_ $ a_ [href_ "/"] "Home"
                            li_ $ a_ [href_ "/about"] "About"
                            li_ $ a_ [href_ "/blog"] "Blog"
                            li_ $ a_ [href_ "https://brain.jaredweakly.com"] "Brain"
                            span_ [class_ "split"] mempty
                            li_ $
                                a_ [href_ "https://github.com/jared-w"] $ do
                                    svg_ [makeAttribute "viewBox" "0 0 24 24", xmlns_ "http://www.w3.org/2000/svg"] $ do
                                        with (makeXmlElementNoEnd "path") [makeAttribute "d" "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"]
                            li_ $ a_ [href_ "/"] "Twitter"
                            li_ $ a_ [href_ "/"] "LinkedIn"
                main_ [id_ "main", tabindex_ "-1"] $ do
                    let e :: Html () = case route of
                            RouteHome -> h1_ (toHtmlRaw rt)
                            RouteAbout -> do
                                article_ [class_ "wrapper"] $ do
                                    h1_ (toHtmlRaw rt)
                                    toHtmlRaw (render val)
                            RouteBlog ->
                                article_ [class_ "wrapper"] $ do
                                    h1_ (toHtmlRaw rt)
                                    ul_ $ do
                                        forM_ val $ \(r, src) ->
                                            li_ $ do
                                                let meta = getMeta src
                                                a_ [href_ (T.pack . dropExtension . T.unpack $ Rib.routeUrl r)] $ do
                                                    toHtmlRaw $ title meta
                                                    span_ $ toHtmlRaw `mapM_` date meta
                            RouteArticle _ ->
                                article_ [class_ "stack"] $ do
                                    header_ $ do
                                        div_ [classes_ ["wrapper", "stack"]] $ do
                                            h1_ (toHtmlRaw rt)
                                            span_ $ toHtmlRaw `mapM_` date (getMeta val)
                                            toHtmlRaw . render $ val
                    e

                css_ "https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400;0,600;1,400;1,600&display=swap"
                css_ "/css/syntax.css"
                css_ "https://cdn.jsdelivr.net/npm/victormono@latest/dist/index.min.css"
