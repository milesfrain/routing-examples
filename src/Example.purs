module Example where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MyRouting (MyRoute)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

type State
  = Maybe MyRoute

-- Query must be (Type -> Type)
data Query a
  = Nav MyRoute a

data Action
  = Redirect

component :: forall i o m. MonadEffect m => H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleQuery = handleQuery
              , handleAction = handleAction
              }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    renderLink link txt = HH.div_ [ HH.a [ HP.href link ] [ HH.text txt ] ]
  in
    HH.div_
      [ HH.text $ show state
      , renderLink "/posts/" "Index"
      , renderLink "/posts/8" "View post 8"
      , renderLink "/posts/8/edit" "Edit post 8"
      , renderLink "/posts/browse/2004/June" "Browse 2004 June"
      , HH.div_
          [ HH.textarea []
          ]
      , HH.div_
          [ HH.button
              [ HE.onClick \_ -> Just Redirect
              ]
              [ HH.text "Save Gist"
              ]
          ]
      ]

handleQuery ∷ forall a o m. Query a → H.HalogenM State Action () o m (Maybe a)
handleQuery (Nav route a) = do
  H.put $ Just route
  pure (Just a)

handleAction ∷ forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Redirect ->
    liftEffect
      $ do
          win <- window
          loc <- location win
          setHref "https://github.com/login/oauth/authorize?client_id=bbaa8fdc61cceb40c899&scope=gist" loc
