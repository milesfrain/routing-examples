module MyRouting where

import Prelude
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Match (Match, int, lit, param, root, str)

type PostId
  = Int

data MyRoute
  = AuthorizeCallback String

derive instance genericMyRoute :: Generic MyRoute _

instance showMyRoute :: Show MyRoute where
  show = genericShow

myRoute :: Match MyRoute
myRoute =
  root
    *> oneOf
        [ AuthorizeCallback <$> (lit "callback" *> param "code")
        ]
