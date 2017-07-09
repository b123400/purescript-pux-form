module Pux.Form.Builder where

import Prelude
import Data.Exists (Exists, mkExists, runExists)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Lens (Lens, view, set)
import Partial.Unsafe (unsafePartial)

import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange, targetValue)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes (value, type')
import Text.Smolder.Markup (text, (!), (#!))

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

instance renderInt :: Render Int where
  render toEvent a = HTML.input ! (value $ show a)
                                ! (type' "number")
                                #! onChange (\e-> case (fromString $ targetValue e) of
                                                    Nothing -> toEvent a
                                                    Just b  -> toEvent b)

field :: forall s e a. (Render a) => Lens s s a a -> Fields s e a
field lens = mkExists $ FieldsF lens render (const true) $ mkExists NoField

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
        unsafeMerge first (FieldsF lens ren pred _) = FieldsF lens ren pred (mkExists first)

infixl 4 andField as .+

fieldWrapped :: forall s e a
             .  (Render a)
             => Lens s s a a
             -> (HTML e -> HTML e)
             -> Fields s e a
fieldWrapped lens f = mkExists $ FieldsF lens customRender (const true) $ mkExists NoField
  where customRender a b = f $ render a b

fieldWithLabel :: forall s e a
               .  (Render a)
               => Lens s s a a
               -> HTML e
               -> Fields s e a
fieldWithLabel lens label = fieldWrapped lens (\e-> HTML.p $ HTML.label $ label *> e)

fieldWithText :: forall s e a
              .  (Render a)
              => Lens s s a a
              -> String
              -> Fields s e a
fieldWithText lens t = fieldWithLabel lens $ text t

infixl 5 fieldWrapped   as .|
infixl 5 fieldWithLabel as ./
infixl 5 fieldWithText  as .\

withPred :: forall s a e. Fields s e a -> (a -> Boolean) -> Fields s e a
withPred fields pred = runExists (\f-> case f of
    (FieldsF lens ren _ next) -> mkExists $ FieldsF lens ren pred next
    NoField                   -> mkExists NoField
  ) fields

infixl 5 withPred as .?

form :: forall s e a1. s -> Fields s e a1 -> (s -> e) -> HTML e
form obj f event = HTML.form inputs
  where inputs = toInputs f
        toInputs :: forall a2. Fields s e a2 -> HTML e
        toInputs = runExists (\f'-> case f' of
          NoField -> text ""
          (FieldsF lens ren pred rest) -> toInputs rest *> ren (\a-> if pred a
                                                                     then event $ set lens a obj
                                                                     else event obj)
                                                               (view lens obj))
