module Pux.Form.Render
  (class Render
  , render
  ) where

import Prelude
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Lens (Lens', lens, view, set)

import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes (value, type')
import Text.Smolder.Markup (text, (!), (#!))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange, targetValue)

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
