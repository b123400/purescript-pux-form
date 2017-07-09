module Pux.Form.Render
  (class Render
  , render
  , TextArea
  , asTextArea
  , Password
  , asPassword
  , File
  , asFile
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
  render toEvent a = HTML.input ! (type' "text")
                                ! (value a)
                                #! onChange (toEvent <<< targetValue)

foreign import targetChecked :: DOMEvent -> Boolean

instance renderBoolean :: Render Boolean where
  render toEvent a = if a
                     then element ! (checked "true")
                     else element
                     where element = HTML.input ! (type' "checkbox")
                                                #! onChange (toEvent <<< targetChecked)

instance renderInt :: Render Int where
  render toEvent a = HTML.input ! (type' "number")
                                ! (value $ show a)
                                #! onChange (\e-> case (fromString $ targetValue e) of
                                                    Nothing -> toEvent a
                                                    Just b  -> toEvent b)

newtype TextArea = TextArea String
derive instance newtypeTextArea :: Newtype TextArea _

instance renderTextAreaString :: Render TextArea where
  render toEvent a = HTML.textarea (text $ unwrap a) #! onChange (toEvent <<< wrap <<< targetValue)

asTextArea :: forall s. Lens' s String -> Lens' s TextArea
asTextArea l = (cast l) :: Lens' s TextArea

newtype Password = Password String
derive instance newtypePassword :: Newtype Password _

instance renderPasswordString :: Render Password where
  render toEvent a = HTML.input ! (type' "password")
                                ! (value $ unwrap a)
                                #! onChange (toEvent <<< wrap <<< targetValue)

asPassword :: forall s. Lens' s String -> Lens' s Password
asPassword l = (cast l) :: Lens' s Password

newtype File = File String
derive instance newtypeFile :: Newtype File _

instance renderFileString :: Render File where
  render toEvent a = HTML.input ! (type' "file")
                                ! (value $ unwrap a)
                                #! onChange (toEvent <<< wrap <<< targetValue)

asFile :: forall s. Lens' s String -> Lens' s File
asFile l = (cast l) :: Lens' s File

cast :: forall s a b.(Newtype a b)=> Lens' s b -> Lens' s a
cast l = lens wrap (const unwrap) >>> l
