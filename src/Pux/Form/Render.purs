module Pux.Form.Render
  (class Render
  , render
  , TextArea
  , asTextArea
  , Password
  , asPassword
  , File
  , asFile
  , Range
  , asRange
  , RangeNum
  , asRangeNum
  , class MultipleChoice
  , choices
  , Dropdown
  , asDropdown
  , asDropdown'
  , WithNothing
  , withNothing
  ) where

import Prelude hiding (min, max)
import Global (readFloat)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Lens (Lens', lens, view, set)
import Data.Foldable (foldl)
import Data.Array ((!!), (:))

import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes (value, type', checked, step, min, max, selected)
import Text.Smolder.Markup (text, (!), (#!))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (DOMEvent, onChange, targetValue)

foreign import targetChecked :: DOMEvent -> Boolean
foreign import targetSelectedIndex :: DOMEvent -> Int


class Render a where
  render :: a -> HTML a


instance renderString :: Render String where
  render a = HTML.input ! (type' "text")
                                ! (value a)
                                #! onChange targetValue


instance renderBoolean :: Render Boolean where
  render a = if a
                     then element ! (checked "true")
                     else element
                     where element = HTML.input ! (type' "checkbox")
                                                #! onChange targetChecked


instance renderInt :: Render Int where
  render a = HTML.input ! (type' "number")
                                ! (value $ show a)
                                #! onChange (\e-> case (fromString $ targetValue e) of
                                                    Nothing -> a
                                                    Just b  -> b)


instance renderNumber :: Render Number where
  render a = HTML.input ! (type' "number")
                                ! (value $ show a)
                                #! onChange (readFloat <<< targetValue)


newtype TextArea = TextArea String
derive instance newtypeTextArea :: Newtype TextArea _

instance renderTextAreaString :: Render TextArea where
  render a = HTML.textarea (text $ unwrap a) #! onChange (wrap <<< targetValue)

asTextArea :: Lens' String TextArea
asTextArea = cast :: Lens' String TextArea


newtype Password = Password String
derive instance newtypePassword :: Newtype Password _

instance renderPasswordString :: Render Password where
  render a = HTML.input ! (type' "password")
                                ! (value $ unwrap a)
                                #! onChange (wrap <<< targetValue)

asPassword :: Lens' String Password
asPassword = cast :: Lens' String Password


newtype File = File String
derive instance newtypeFile :: Newtype File _

instance renderFileString :: Render File where
  render a = HTML.input ! (type' "file")
                                ! (value $ unwrap a)
                                #! onChange (wrap <<< targetValue)

asFile :: Lens' String File
asFile = cast :: Lens' String File

data Range = Range Int Int Int Int

instance renderRange :: Render Range where
  render (Range min' max' step' val) =
    HTML.input ! (type' "range")
               ! (value $ show val)
               ! (min $ show min')
               ! (max $ show max')
               ! (step $ show step')
               #! onChange (\e-> case (fromString $ targetValue e) of
                                   Nothing -> Range min' max' step' val
                                   Just n  -> Range min' max' step' n
                )

asRange :: Int -> Int -> Int -> Lens' Int Range
asRange min max step = lens (Range min max step) (\_ (Range _ _ _ val)-> val)


data RangeNum = RangeNum Number Number Number Number

instance renderRangeNum :: Render RangeNum where
  render (RangeNum min' max' step' val) =
    HTML.input ! (type' "range")
               ! (value $ show val)
               ! (min $ show min')
               ! (max $ show max')
               ! (step $ show step')
               #! onChange \e-> RangeNum min' max' step' $ readFloat $ targetValue e

asRangeNum :: Number -> Number -> Number -> Lens' Number RangeNum
asRangeNum min max step = lens (RangeNum min max step) (\_ (RangeNum _ _ _ val)-> val)


class (Eq a, Show a) <= MultipleChoice a where
  choices :: Array a

data Dropdown a = Dropdown a (Array a)

instance renderMultipleChoice :: (Eq a, Show a)=> Render (Dropdown a) where
  render (Dropdown val choices') =
    HTML.select #! (onChange \e-> Dropdown (fromMaybe val $ choices' !! targetSelectedIndex e) choices')
                $ foldl (*>) (text "") options
    where options = choices' <#> \c-> let elem = HTML.option $ text $ show c
                                      in if c == val then elem ! (selected "true")
                                         else elem

asDropdown :: forall a. MultipleChoice a => Lens' a (Dropdown a)
asDropdown = lens (\a-> Dropdown a choices) (\_ (Dropdown a _) -> a)

asDropdown' :: forall s a. Eq a => Show a => Array a -> Lens' a (Dropdown a)
asDropdown' choices' = lens (\a-> Dropdown a choices') (\_ (Dropdown a _)-> a)


newtype WithNothing a = WithNothing (Maybe a)
derive instance newtypeWithNothing :: Newtype (WithNothing a) _
derive instance eqWithNothing :: Eq a => Eq (WithNothing a)

withNothing :: forall a. Lens' (Maybe a) (WithNothing a)
withNothing = lens wrap $ const unwrap

instance multipleChoiceWithNothing :: MultipleChoice a => MultipleChoice (WithNothing a) where
  choices = (WithNothing Nothing) : ((WithNothing <<< Just) <$> choices)

instance showWithNothing :: Show a => Show (WithNothing a) where
  show (WithNothing Nothing) = "None"
  show (WithNothing (Just a)) = show a


cast :: forall a b.(Newtype b a)=> Lens' a b
cast = lens wrap $ const unwrap
