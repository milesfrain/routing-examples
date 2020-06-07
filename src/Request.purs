module Request where

import Prelude
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (decodeJson, encodeJson)
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff)
import Effect.Class.Console (log)

tokenServerUrl :: String
tokenServerUrl = "http://localhost:7071/api/localtrigger"

type TokenResp
  = { access_token :: String }

ghRequestToken :: String -> Aff (Either String String)
ghRequestToken code = do
  result <- AX.post AXRF.json tokenServerUrl $ Just $ AXRB.json $ encodeJson { code }
  pure
    $ case result of
        Left err -> do
          Left $ "POST /api response failed to decode: " <> AX.printError err
        Right response -> do
          let
            respStr = "POST /api response: " <> J.stringify response.body
          case decodeJson response.body of
            Left err -> Left $ "Failed to decode json response: " <> respStr <> ", Error: " <> show err
            Right (decoded :: TokenResp) -> Right decoded.access_token

gistApiUrl :: String
gistApiUrl = "https://api.github.com/gists"

type GistJson
  = { files :: { "Main.purs" :: { content :: String } }
    }

type GistJsonWithDescription
  = { files ::
        { "Main.purs" ::
            { content :: String
            , description :: String
            }
        }
    }

setGistContent :: String -> GistJsonWithDescription
setGistContent content =
  { files:
      { "Main.purs":
          { content
          , description: "Created by TryPurescript"
          }
      }
  }

getGistContent :: GistJson -> String
getGistContent obj = obj.files."Main.purs".content

ghGetGist :: String -> Aff (Either String String)
ghGetGist gistID = do
  result <- AX.get AXRF.json $ gistApiUrl <> "/" <> gistID
  pure
    $ case result of
        Left err -> Left $ "GET gist response failed to decode: " <> AX.printError err
        Right response -> do
          let
            respStr = "POST /api response: " <> J.stringify response.body
          case decodeJson response.body of
            Left err -> Left $ "Failed to decode json response: " <> respStr <> ", Error: " <> show err
            Right (decoded :: GistJson) -> Right $ getGistContent decoded

-- Todo, probably want custom types for Token and GistID
-- Ommiting check if existing gist is unchanged
ghCreateGist :: String -> String -> Aff (Either String String)
ghCreateGist token content = do
  result <-
    AX.request
      ( AX.defaultRequest
          { url = gistApiUrl
          , method = Left POST
          , responseFormat = AXRF.json
          , headers = [ AXRH.RequestHeader "Authorization" $ "token " <> token ]
          , content = Just $ AXRB.json $ encodeJson $ setGistContent content
          }
      )
  pure
    $ case result of
        Left err -> do
          Left $ "POST /api response failed to decode: " <> AX.printError err
        Right response -> do
          let
            respStr = "POST /api response: " <> J.stringify response.body
          case decodeJson response.body of
            Left err -> Left $ "Failed to decode json response: " <> respStr <> ", Error: " <> show err
            Right (decoded :: { id :: String }) -> Right decoded.id

-- Upload new gist
-- Return new (or existing) gist ID
