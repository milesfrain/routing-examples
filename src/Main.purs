module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Example (Query(..))
import Example as Example
import Foreign (Foreign, unsafeToForeign)
import Halogen (HalogenIO)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MyRouting (myRoute)
import Routing.PushState (makeInterface, matches)
import Web.HTML (HTMLElement)

main :: Effect Unit
main =
  HA.runHalogenAff do
    (body :: HTMLElement) <- HA.awaitBody
    (replaceState :: Foreign -> String -> Effect Unit) <-
      liftEffect
        $ do
            nav <- makeInterface
            pure nav.replaceState
    halogenIO <- runUI Example.component (replaceState $ unsafeToForeign {}) body
    void
      $ liftEffect do
          nav <- makeInterface
          nav
            # matches myRoute \oldRoute newRoute -> do
                log $ show oldRoute <> " -> " <> show newRoute
                launchAff_ $ halogenIO.query $ H.tell $ Nav newRoute
