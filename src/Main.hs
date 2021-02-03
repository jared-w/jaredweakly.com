module Main where

import Control.Concurrent.Async
import Control.Retry
import qualified LucidGen (writeHtmlRoute)
import Main.Utf8 (withUtf8)
import Rib (run)
import qualified Templates (writeHtmlRoute)
import qualified TypeOfHtml (writeHtmlRoute)
import Utils (generateSite)

main :: IO ()
main = withUtf8 $ mapConcurrently_ (recoverAll (fullJitterBackoff 10000) . const) [mainHtml, mainLucid, mainTemplate]

-- main :: IO ()
-- main = mainHtml

mainHtml :: IO ()
mainHtml = withUtf8 $ run "site" "dist" (generateSite TypeOfHtml.writeHtmlRoute)

mainLucid :: IO ()
mainLucid = withUtf8 $ run "site" "distLucid" (generateSite LucidGen.writeHtmlRoute)

mainTemplate :: IO ()
mainTemplate = withUtf8 $ run "site" "distTemplate" (generateSite Templates.writeHtmlRoute)
