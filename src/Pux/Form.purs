module Pux.Form
  ( Fields, FieldsF
  , field
  , (.+), andField
  , (.|), fieldWrapped
  , (./), fieldWithLabel
  , (.\), fieldWithText
  , (.?), withPred
  , form
  , module Pux.Form.Render
  ) where

import Prelude
import Data.Exists (Exists, mkExists, runExists)
import Data.Lens (Lens, view, set)
import Partial.Unsafe (unsafePartial)

import Pux.DOM.HTML (HTML, mapEvent)
import Text.Smolder.Markup (text)
import Text.Smolder.HTML as HTML

import Pux.Form.Render (class Render, render)

data FieldsF s e a a2 = FieldsF (Lens s s a a)
                                (a -> HTML a)
                                (HTML e -> HTML e)
                                (a -> Boolean)
                                (Fields s e a2)
                      | NoField

type Fields s e a = Exists (FieldsF s e a)

-- | Wraps a lens into a field, without HTML label
field :: forall s e a. (Render a) => Lens s s a a -> Fields s e a
field lens = mkExists $ FieldsF lens render id (const true) $ mkExists NoField

-- | Combine 2 fields
andField :: forall s e a1 a2. Fields s e a1 -> Fields s e a2 -> Fields s e a2
andField a b =
  runExists (\b'->
    runExists (\a'->
      mkExists $ unsafePartial $ unsafeMerge a' b') a) b
  where unsafeMerge :: forall x1 x2
                    .  Partial
                    => FieldsF s e a1 x1
                    -> FieldsF s e a2 x2
                    -> FieldsF s e a2 a1
        unsafeMerge first (FieldsF lens ren tran pred _) = FieldsF lens ren tran pred (mkExists first)

infixl 4 andField as .+

-- | Create a field with a lens and a function that wraps around the <input> element.
-- | For example typical form would wrap <input> with a <label> tag.
-- | See fieldWithLabel for example.
fieldWrapped :: forall s e a
             .  (Render a)
             => Lens s s a a
             -> (HTML e -> HTML e)
             -> Fields s e a
fieldWrapped lens f = mkExists $ FieldsF lens render f (const true) $ mkExists NoField

-- | Create a field with a lens and a HTML element.
-- | The element will be added in front of the <input> element,
-- | inside the <label> tag.
fieldWithLabel :: forall s e a
               .  (Render a)
               => Lens s s a a
               -> HTML e
               -> Fields s e a
fieldWithLabel lens label = fieldWrapped lens (\e-> HTML.p $ HTML.label $ label *> e)

-- | Create a field with a lens and a String that is used as the label.
fieldWithText :: forall s e a
              .  (Render a)
              => Lens s s a a
              -> String
              -> Fields s e a
fieldWithText lens t = fieldWithLabel lens $ text t

infixl 5 fieldWrapped   as .|
infixl 5 fieldWithLabel as ./
infixl 5 fieldWithText  as .\

-- | Add a condition to the field.
-- | If the condition does not match, the field won't be updated.
withPred :: forall s a e. Fields s e a -> (a -> Boolean) -> Fields s e a
withPred fields pred = runExists (\f-> case f of
    (FieldsF lens ren tran _ next) -> mkExists $ FieldsF lens ren tran pred next
    NoField                        -> mkExists NoField
  ) fields

infixl 5 withPred as .?

-- | Turns fields into the form HTML.
form :: forall s e a1. s -> Fields s e a1 -> (s -> e) -> HTML e
form obj f event = HTML.form inputs
  where inputs = toInputs f
        toInputs :: forall a2. Fields s e a2 -> HTML e
        toInputs = runExists (\f'-> case f' of
          NoField -> text ""
          (FieldsF lens ren custom pred rest) ->
            let ele = (ren $ view lens obj)
                      `flip mapEvent` (\a-> if pred a
                                            then event $ set lens a obj
                                            else event obj)
            in toInputs rest *> (custom ele)
        )
