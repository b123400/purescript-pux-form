module Pux.Form.Builder where

import Prelude
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (foldl)
import Data.Array (snoc)
import Data.Lens (Lens, view, set)
import Data.Newtype (class Newtype, unwrap)

import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange, targetValue)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes (value, type')
import Text.Smolder.Markup (text, (!), (#!))

data Builder s e = Builder s (Fields s e)

data Fields s e = Fields (Fields s e) (Field s e)
                | NoField

data FieldF s e a = FieldF (Lens s s a a) ((a -> e) -> a -> HTML e) (a -> Boolean)

newtype Field s e = Field (Exists (FieldF s e))

derive instance newtypeField :: Newtype (Field s e) _

class AsFields a s e where
  toFields :: a s e -> Fields s e

instance fieldsAsFields :: AsFields Fields s e where
  toFields = id

instance fieldAsFields :: AsFields Field s e where
  toFields = Fields NoField

class Render a where
  render :: forall e. (a -> e) -> a -> HTML e

instance renderString :: Render String where
  render toEvent a = HTML.input ! (value a)
                                ! (type' "text")
                                #! onChange (toEvent <<< targetValue)

field :: forall s a e. (Render a) => Lens s s a a -> Field s e
field lens = Field $ mkExists $ FieldF lens render (const true)

andFields :: forall s e. Fields s e -> Field s e -> Fields s e
andFields = Fields

infixl 4 andFields as .>

andField :: forall s e. Field s e -> Field s e -> Fields s e
andField = (andFields <<< toFields)

infixl 4 andField as .+

form :: forall s e a. (AsFields a s e) => a s e -> s -> Builder s e
form = (flip Builder) <<< toFields

renderForm :: forall s e. Builder s e -> (s -> e) -> HTML e
renderForm (Builder obj f) event = HTML.form (foldl (*>) (text "") children)
  where fields = genFields f
        children = fields
                   <#> unwrap
                   <#> runExists (\(FieldF lens r _)-> r (\a-> event $ set lens a obj) (view lens obj))

genFields :: forall s e. Fields s e -> Array (Field s e)
genFields NoField = []
genFields (Fields a b) = snoc (genFields a) b


-- form :: FormData
-- form =    httpPort .| text "http port"        .? (\x-> x > 1000 && x < 65535)
--        .> destDir  .| text "dest directory"   ./ filePicker
--        .> inline   .| text "Combine JS files"

-- runForm :: FormData -> (State -> Event) -> HTML

-- instance Fieldable Int where
--   field = input ! (type "number")
