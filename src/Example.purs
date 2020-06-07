module Example where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MyRouting (MyRoute(..))
import Record (delete)
import Request (ghCreateGist, ghRequestToken)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

{-
Todos:
local storage to presever content across callback
-}
type SetRoute
  = String -> Effect Unit

type Input
  = SetRoute

type State
  = { route :: Maybe MyRoute
    , token :: Maybe String
    , setRoute :: SetRoute
    , gistID :: Maybe String
    -- Just going to clear gistID upon new text entered
    -- May add logic to update existing ID and detect changes
    --, dirty :: Boolean -- New text in textbox
    , content :: String
    }

initialState :: Input -> State
initialState setRoute =
  { route: Nothing
  , token: Nothing
  , setRoute
  , gistID: Nothing
  , content: "Welcome"
  }

-- Will assume token is valid if it exists. May refresh to reset.
-- Query must be (Type -> Type)
data Query a
  = Nav MyRoute a

data Action
  = SaveGist
  | SetContent String

component :: forall o m. MonadAff m => H.Component HH.HTML Query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleQuery = handleQuery
              , handleAction = handleAction
              }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  let
    Tuple buttonOnClick buttonText = case state.gistID of
      -- Nothing means gist not saved yet, or new changes detected
      Nothing -> Tuple (Just SaveGist) "Save Gist"
      -- Todo, make this a link
      Just id -> Tuple Nothing $ "?gist=" <> id
  in
    HH.div_
      [ HH.div_ [ HH.text $ show $ delete (SProxy :: _ "setRoute") state ]
      , HH.div_
          [ HH.textarea
              [ HP.value state.content
              -- onChange fires less often
              , HE.onValueChange \str -> Just (SetContent str)
              -- onInput doesn't solve the problem either. Local storage is required.
              --, HE.onValueInput \str -> Just (SetContent str)
              ]
          ]
      , HH.div_
          [ HH.button
              [ HE.onClick \_ -> buttonOnClick
              ]
              [ HH.text buttonText
              ]
          ]
      ]

handleQuery ∷ forall a o m. MonadAff m => Query a → H.HalogenM State Action () o m (Maybe a)
handleQuery (Nav route a) = do
  H.modify_ _ { route = Just route }
  case route of
    AuthorizeCallback code -> do
      res <- liftAff $ ghRequestToken code
      case res of
        Left err -> log err
        Right token -> do
          st <- H.get
          newState <- liftAff $ doSaveGist token $ st { token = Just token }
          H.put newState
          log $ "put content after callback: " <> newState.content
          liftEffect $ st.setRoute "complete"
  pure (Just a)

doSaveGist :: String -> State -> Aff State
doSaveGist token st = do
  log $ "content in doSaveGist: " <> st.content
  result <- ghCreateGist token st.content
  pure
    $ case result of
        Left err -> st { content = err <> "\n" <> st.content }
        Right id -> st { gistID = Just id }

handleAction ∷ forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SaveGist -> do
    st <- H.get
    log $ "incoming content in SaveGist: " <> st.content
    case st.token of
      Nothing -> liftEffect $ ghAuthorize
      Just token -> do
        newState <- liftAff $ doSaveGist token st
        H.put newState
  --SaveGist -> H.modify_ \st -> st { content = st.content <> " -- suffix" }
  -- New content clears existing gistID
  SetContent str -> do
    log $ "writing in SetContent: " <> str
    H.modify_ _ { gistID = Nothing, content = str }

ghAuthorize :: Effect Unit
ghAuthorize = do
  win <- window
  loc <- location win
  -- I believe it's fine for client ID to be public information
  setHref "https://github.com/login/oauth/authorize?client_id=bbaa8fdc61cceb40c899&scope=gist" loc
