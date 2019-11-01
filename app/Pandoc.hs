module Pandoc where

import           Control.Monad
import           Data.Aeson                    as A
import           Development.Shake
import qualified Data.Text                     as T
import           Slick.Pandoc
import           Text.Pandoc
import           Text.Pandoc.Builder           as B
import           Text.Pandoc.Walk

mdToHtml :: T.Text -> Action Value
mdToHtml t = loadUsing md (writeHtml5String defaultHtml5Options) t
 where
  md   = readMarkdown exts >=> pure . walk processBlock
  exts = def { readerExtensions = pandocExtensions }

-- The goal of this is to take markdown of the form
-- ::: component
-- Text
-- :::
--
-- And render it as a web component with the appropriate structure, classes,
-- etc. without giving up and just raw dumping the HTML into the pipeline.
-- That way I can handle recursive structure and keep things inside pandoc as
-- much as possible to take advantage of the generic AST
processBlock :: Block -> Block
processBlock (B.Div a@(_, classes, attrs) contents) | "cover" `elem` classes =
  B.Div a
    $   (head . toList . divWith nullAttr)
    <$> [get "header", fromList contents, get "footer"]
  where get s = plain . text $ maybe mempty id $ lookup s attrs
processBlock b = b
