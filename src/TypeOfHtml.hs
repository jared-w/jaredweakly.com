{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

module TypeOfHtml (page, writeHtmlRoute) where

import qualified Data.Text as T
import Development.Shake
import Html
import qualified Rib
import Utils
import Prelude

writeHtmlRoute :: Route a -> a -> Action ()
writeHtmlRoute r = Rib.writeRoute r . renderText . page r

page rt a = do
    DOCTYPE
    Html :@ LangA := "en" :> do
        Head :> do
            Meta :@ CharsetA := "UTF-8"
            Meta :@ (NameA := "viewport" # ContentA := "width=device-width, initial-scale=1.0")
            Meta :@ (HttpEquivA := "X-UA-Compatible" # ContentA := "ie_ edge")
            Link :@ (RelA := "dns-prefetch" # HrefA := "//fonts.googleapis.com")
            Link :@ (RelA := "preconnect" # HrefA := "https://fonts.gstatic.com/" # CrossoriginA)

            prelf "/static/Exodus-Sharpen.woff2"
            prelf "/static/Exodus-Striped.woff2"
            prel "https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400;0,600;1,400;1,600&display=swap"
            prel "/css/base.css"
            css "/css/base.css"
            Meta :@ (NameA := "description" # ContentA := "Jared Weaky")
            whenHome rt (css "/css/home.css")
            Title :> routeTitle rt a

        Body :@ whenHome rt (ClassA := "cover") :> do
            Header :> do
                A :@ HrefA := "#main" :> "Skip to main content"
                Nav :@ ClassA := "wrapper" :> do
                    Ul :@ ClassA := "top-navigation" :> do
                        Li :> A :@ HrefA := "/" :> "Home"
                        Li :> A :@ HrefA := "/about" :> "About"
                        Li :> A :@ HrefA := "/blog" :> "Blog"
                        Li :> A :@ HrefA := "https://brain.jaredweakly.com" :> "Brain"
                        Raw "<span class=\"split\"></span>"
                        Li :> A :@ (ClassA := "icon" # HrefA := "https://github.com/jared-w") :> Raw githubSvg
                        Li :> A :@ (ClassA := "icon" # HrefA := "https://twitter.com/jaredweakly") :> Raw twitterSvg
                        Li :> A :@ (ClassA := "icon" # HrefA := "https://www.linkedin.com/in/jaredweakly") :> Raw linkedInSVG
            Main :@ (IdA := "main" # TabindexA := "-1") :> do
                let t = H1 :> Raw (routeTitle rt a)
                let e :: El
                    e = case rt of
                        RouteHome -> El t
                        RouteAbout -> El $ Article :@ ClassA := "wrapper" :> (t # Raw (render a))
                        RouteBlog ->
                            El $ Article :@ ClassA := "wrapper" :> (t # Ul :> map mkListing a)
                        RouteArticle _ ->
                            El $
                                Article :@ ClassA := "stack" :> do
                                    Header :> Div :@ (ClassA := "wrapper stack") :> do
                                        t
                                        Span :> date (getMeta a)
                                    Div :@ ClassA := "wrapper stack -lg post-body" :> Raw (render a)
                e
            css "https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400;0,600;1,400;1,600&display=swap"
            let c :: El
                c = case rt of
                    RouteArticle _ ->
                        if hasCode a
                            then El $ do
                                css "https://cdn.jsdelivr.net/npm/victormono@latest/dist/index.min.css"
                                css "/css/syntax.css"
                            else El ""
                    _ -> El ""
            c
    where
        (>>) = (#)
        ifThenElse a b c = case a of
            True -> b
            False -> c

instance Convert El where {-# INLINE convert #-}; convert (El a) = Converted $ renderBuilder a

data El = forall a. Document a => El a

mkListing (r, doc) =
    let meta = routeMeta r doc
     in Li :> A :@ HrefA := toUrl r
            :> (title meta # Span :> date meta)

githubSvg :: T.Text
githubSvg =
    (T.pack . concat)
        [ "<svg width=\"30\" height=\"30\" viewBox=\"0 0 24 24\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"M12"
        , " .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205"
        , " 11.385.6.113.82-.258.82-.577"
        , " 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633"
        , " 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236"
        , " 1.07 1.835 2.809 1.305"
        , " 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93"
        , " 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3"
        , " 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23"
        , " 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805"
        , " 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0"
        , " .315.21.69.825.57C20.565 22.092 24 17.592 24"
        , " 12.297c0-6.627-5.373-12-12-12\"/></svg>"
        ]

twitterSvg :: T.Text
twitterSvg =
    (T.pack . concat)
        [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"30\" height=\"30\" viewBox=\"0 0 400 400\"><path d=\"M400"
        , " 200a200 200 0 11-400 0 200 200 0 01400 0zM163 306c89 0 138-74 138-138v-6c9-7"
        , " 17-15 24-25-9 4-18 7-28 8 10-6 18-16 21-27-9 6-19 10-30 12-9-10-22-15-36-15a48"
        , " 48 0 00-47 59c-40-2-75-21-99-51a48 48 0 0015 65c-8-1-15-3-22-6 0 24 17 43 39"
        , " 47a48 48 0 01-22 1c6 19 24 33 45 34a97 97 0 01-71 20c21 14 46 22 73 22\"/></svg>"
        ]

linkedInSVG :: T.Text
linkedInSVG =
    (T.pack . concat)
        [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"30\" height=\"30\" viewBox=\"0 0 455 455\"><g"
        , " fill-rule=\"evenodd\" clip-rule=\"evenodd\"><path d=\"M246"
        , " 204zM0 0v455h455V0H0zm142"
        , " 378H74V175h68v203zm-34-231h-1c-22 0-37-15-37-35s15-35"
        , " 38-35 38 15 38 35-15 35-38 35zm277"
        , " 231h-68V269c0-27-9-46-34-46-18 0-29 13-34 25-2 4-3 10-3"
        , " 17v113h-67V175h67v29c9-14 25-34 61-34 45 0 78 29 78"
        , " 92v116z\"/></g></svg>"
        ]

prel h = Link :@ (RelA := "preload" # AsA := "style" # HrefA := h)
prels h = Link :@ (RelA := "preload" # AsA := "script" # HrefA := h)
prelf h = Link :@ (RelA := "preload" # AsA := "font" # HrefA := h # TypeA := "font/woff2" # CrossoriginA)
css h = Link :@ (HrefA := h # RelA := "stylesheet")
