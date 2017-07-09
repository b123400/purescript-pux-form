module Pux.Form.Render
  (class Render
  , render
  , TextArea
  , asTextArea
  , cast
  ) where

import Prelude
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Lens (Lens', lens)

import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes (value, type', checked)
import Text.Smolder.Markup (text, (!), (#!))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (DOMEvent, onChange, targetValue)

class Render a where
  render :: forall e. (a -> e) -> a -> HTML e

instance renderString :: Render String where
  render toEvent a = HTML.input ! (value a)
                                ! (type' "text")
                                #! onChange (toEvent <<< targetValue)

foreign import targetChecked :: DOMEvent -> Boolean

instance renderBoolean :: Render Boolean where
  render toEvent a = if a then element ! (checked "true") else element
                     where element = HTML.input ! (type' "checkbox")
                                                #! onChange (toEvent <<< targetChecked)

instance renderInt :: Render Int where
  render toEvent a = HTML.input ! (value $ show a)
                                ! (type' "number")
                                #! onChange (\e-> case (fromString $ targetValue e) of
                                                    Nothing -> toEvent a
                                                    Just b  -> toEvent b)

newtype TextArea = TextArea String

derive instance newtypeTextArea :: Newtype TextArea _

instance renderTextAreaString :: Render TextArea where
  render toEvent a = HTML.textarea (text $ unwrap a) #! onChange (toEvent <<< wrap <<< targetValue)

asTextArea :: forall s. Lens' s String -> Lens' s TextArea
asTextArea l = (cast l) :: Lens' s TextArea

cast :: forall s a b.(Newtype a b)=> Lens' s b -> Lens' s a
cast l = lens wrap (const unwrap) >>> l
