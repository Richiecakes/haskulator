{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Snap
import           Snap.Core
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Snap.Http.Server

data App = App
  { _heist     :: Snaplet (Heist App)
  }

makeLenses ''App

instance HasHeist App where heistLens = subSnaplet heist

indexHandler :: Handler App App ()
indexHandler = render "base"

appInit :: SnapletInit App App
appInit = makeSnaplet "haskulator" description Nothing $ do
  hs <- nestSnaplet "" heist $ heistInit "templates"
  addRoutes [ ("static",    serveDirectory "static")
            , ("/",          indexHandler)
            ]
  return $ App hs
  where description = "The haskell calculator"

main :: IO ()
main = serveSnaplet defaultConfig appInit