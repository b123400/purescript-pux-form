module Pux.Form.Builder where

import Prelude
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (foldl)
import Data.Array (snoc)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Lens (Lens, view, set)
import Data.Newtype (class Newtype, unwrap)
import Partial.Unsafe (unsafePartial)

import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange, targetValue)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes (value, type')
import Text.Smolder.Markup (text, (!), (#!))

data Builder s e a = Builder s (Fields s e a)

data FieldsF s e a a2 = FieldsF (Lens s s a a)
                                ((a -> e) -> a -> HTML e)
                                (a -> Boolean)
                                (Fields s e a2)
                      | NoField

type Fields s e a = Exists (FieldsF s e a)

class Render a where
  render :: forall e. (a -> e) -> a -> HTML e

instance renderString :: Render String where
  render toEvent a = HTML.input ! (value a)
                                ! (type' "text")
                                #! onChange (toEvent <<< targetValue)


field :: forall s e a. (Render a) => Lens s s a a -> Fields s e a
field lens = mkExists $ FieldsF lens render (const true) $ mkExists NoField

andField :: forall s e a1 a2. Fields s e a1 -> Fields s e a2 -> Fields s e a2
andField a b =
  runExists (\b'->
    runExists (\a'->
      mkExists $ unsafePartial $ unsafeMerge a' b') a) b
  where unsafeMerge :: forall x1 x2. Partial => FieldsF s e a1 x1 -> FieldsF s e a2 x2 -> FieldsF s e a2 a1
        unsafeMerge a (FieldsF lens ren pred _) = FieldsF lens ren pred (mkExists a)

infixl 4 andField as .+

fieldWithDOM :: forall s e a. (Render a) => Lens s s a a -> HTML e -> Fields s e a
fieldWithDOM lens dom = mkExists $ FieldsF lens customRender (const true) $ mkExists NoField
  where customRender a b =  dom *> (render a b)

infixl 5 fieldWithDOM as .|

withPred :: forall s a e. Fields s e a -> (a -> Boolean) -> Fields s e a
withPred fields pred = runExists (\f-> case f of
    (FieldsF lens ren _ next) -> mkExists $ FieldsF lens ren pred next
    NoField                   -> mkExists NoField
  ) fields

infixl 5 withPred as .?

form :: forall s e a. Fields s e a -> s -> Builder s e a
form = flip Builder

renderForm :: forall s e a. Builder s e a -> (s -> e) -> HTML e
renderForm (Builder obj f) event = HTML.form inputs
  where inputs = toInputs f
        toInputs :: forall a. Fields s e a -> HTML e
        toInputs = runExists (\f'-> case f' of
          NoField -> text ""
          (FieldsF lens ren pred rest) -> toInputs rest *> ren (\a-> if pred a
                                                                     then event $ set lens a obj
                                                                     else event obj)
                                                               (view lens obj))


-- form :: FormData
-- form =    httpPort .| text "http port"        .? (\x-> x > 1000 && x < 65535)
--        .> destDir  .| text "dest directory"   ./ filePicker
--        .> inline   .| text "Combine JS files"

-- runForm :: FormData -> (State -> Event) -> HTML

-- instance Fieldable Int where
--   field = input ! (type "number")
