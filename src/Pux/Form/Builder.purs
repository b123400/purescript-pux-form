module Pux.Form.Builder where

import Prelude
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (foldl)
import Data.Array ((:))
import Data.Lens (Lens, view, set)

import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (form)
import Text.Smolder.Markup (text)

data Builder s e = Builder s (Fields s e)

data Fields s e = Fields (Field s e) (Fields s e)
                | NoField

data FieldF s e a = FieldF (Lens s s a a) ((a -> e) -> a -> HTML e) (a -> Boolean)

type Field s e = Exists (FieldF s e)

class Render a where
  render :: forall e. (a -> e) -> a -> HTML e

instance renderString :: Render String where
  render _ a = text a

makeField :: forall s a e. (Render a) => Lens s s a a -> Field s e
makeField lens = mkExists $ FieldF lens render (const true)

makeFields :: forall s e. Field s e -> Fields s e
makeFields a = Fields a NoField

andFields :: forall s e. Fields s e -> Field s e -> Fields s e
andFields curr new = Fields new curr

makeBuilder :: forall s e. s -> Fields s e -> Builder s e
makeBuilder = Builder

renderForm :: forall s e. Builder s e -> (s -> e) -> HTML e
renderForm (Builder obj f) event = form (foldl (*>) (text "init") children)
  where fields = genFields f
        children = fields <#> \field-> runExists (\(FieldF lens r _)-> r (\a-> event $ set lens a obj) (view lens obj)) field

genFields :: forall s e. Fields s e -> Array (Field s e)
genFields NoField = []
genFields (Fields a b) = a : (genFields b)


-- form :: FormData
-- form =
--   config .^ httpPort .| text "http port"        .? (\x-> x > 1000 && x < 65535)
--          .> destDir  .| text "dest directory"   ./ filePicker
--          .> inline   .| text "Combine JS files"

-- runForm :: FormData -> (State -> Event) -> HTML

-- instance Fieldable Int where
--   field = input ! (type "number")
