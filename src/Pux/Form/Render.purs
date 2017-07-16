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
  ) where

import Prelude hiding (min, max)
import Global (readFloat)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Lens (Lens', lens, view, set)
import Data.Foldable (foldl)
import Data.Array ((!!))

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

asTextArea :: forall s. Lens' s String -> Lens' s TextArea
asTextArea l = (cast l) :: Lens' s TextArea


newtype Password = Password String
derive instance newtypePassword :: Newtype Password _

instance renderPasswordString :: Render Password where
  render a = HTML.input ! (type' "password")
                                ! (value $ unwrap a)
                                #! onChange (wrap <<< targetValue)

asPassword :: forall s. Lens' s String -> Lens' s Password
asPassword l = (cast l) :: Lens' s Password


newtype File = File String
derive instance newtypeFile :: Newtype File _

instance renderFileString :: Render File where
  render a = HTML.input ! (type' "file")
                                ! (value $ unwrap a)
                                #! onChange (wrap <<< targetValue)

asFile :: forall s. Lens' s String -> Lens' s File
asFile l = (cast l) :: Lens' s File

data Range = Range Int Int Int Int

instance renderRange :: Render Range where
  render (Range val min' max' step') =
    HTML.input ! (type' "range")
               ! (value $ show val)
               ! (min $ show min')
               ! (max $ show max')
               ! (step $ show step')
               #! onChange (\e-> case (fromString $ targetValue e) of
                                   Nothing -> Range val min' max' step'
                                   Just n  -> Range n min' max' step'
                )

asRange :: forall s. Lens' s Int -> Int -> Int -> Int -> Lens' s Range
asRange l min max step = lens (\s-> Range (view l s) min max step) (\num (Range val _ _ _)-> set l val num)


data RangeNum = RangeNum Number Number Number Number

instance renderRangeNum :: Render RangeNum where
  render (RangeNum val min' max' step') =
    HTML.input ! (type' "range")
               ! (value $ show val)
               ! (min $ show min')
               ! (max $ show max')
               ! (step $ show step')
               #! onChange \e-> RangeNum (readFloat $ targetValue e) min' max' step'

asRangeNum :: forall s. Lens' s Number -> Number -> Number -> Number -> Lens' s RangeNum
asRangeNum l min max step = lens (\s-> RangeNum (view l s) min max step) (\num (RangeNum val _ _ _)-> set l val num)


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

asDropdown :: forall s a. (MultipleChoice a)=> Lens' s a -> Lens' s (Dropdown a)
asDropdown l = asDropdown' l (choices :: Array a)

asDropdown' :: forall s a. Eq a => Show a=> Lens' s a -> Array a -> Lens' s (Dropdown a)
asDropdown' l choices' = lens (\s-> Dropdown (view l s) choices') (\a (Dropdown b _)-> set l b a)

cast :: forall s a b.(Newtype b a)=> Lens' s a -> Lens' s b
cast l = lens wrap (const unwrap) >>> l
