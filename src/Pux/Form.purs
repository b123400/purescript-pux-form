module Pux.Form
  ( Field, FieldF, Fields
  , field
  , (.|), fieldWrapped
  , (./), fieldWithLabel
  , (.\), fieldWithText
  , form
  , module Pux.Form.Render
  ) where

import Prelude

import Data.CatList (CatList)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (foldl)
import Data.Lens (Lens', set, view)
import Pux.DOM.HTML (HTML, mapEvent)
import Pux.Form.Render (class Render, render)
import Text.Smolder.HTML as HTML
import Text.Smolder.Markup (text)

data FieldF s e a = FieldF (Lens' s a)
                           (a -> HTML a)
                           (HTML e -> HTML e)

type Field s e = Exists (FieldF s e)
type Fields s e = CatList (Field s e)

-- | Wraps a lens into a field, without HTML label
field :: forall s e a. (Render a) => Lens' s a -> Fields s e
field lens = pure $ mkExists $ FieldF lens render id

-- | Create a field with a lens and a function that wraps around the <input> element.
-- | For example typical form would wrap <input> with a <label> tag.
-- | See fieldWithLabel for example.
fieldWrapped :: forall s e a
             .  (Render a)
             => Lens' s a
             -> (HTML e -> HTML e)
             -> Fields s e
fieldWrapped lens f = pure $ mkExists $ FieldF lens render f

-- | Create a field with a lens and a HTML element.
-- | The element will be added in front of the <input> element,
-- | inside the <label> tag.
fieldWithLabel :: forall s e a
               .  (Render a)
               => Lens' s a
               -> HTML e
               -> Fields s e
fieldWithLabel lens label = fieldWrapped lens (HTML.p <<< HTML.label <<< ((*>) label))

-- | Create a field with a lens and a String that is used as the label.
fieldWithText :: forall s e a
              .  (Render a)
              => Lens' s a
              -> String
              -> Fields s e
fieldWithText lens t = fieldWithLabel lens $ text t

infixl 5 fieldWrapped   as .|
infixl 5 fieldWithLabel as ./
infixl 5 fieldWithText  as .\

-- | Turns fields into the form HTML.
form :: forall s e. s -> Fields s e -> (s -> e) -> HTML e
form obj fields event = HTML.form $ foldl (*>) (text "") (toInput <$> fields)
  where toInput = runExists (\(FieldF lens renda trans)->
                                view lens obj
                                # renda
                                # mapEvent (\a-> event $ set lens a obj)
                                # trans)
