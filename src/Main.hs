{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import qualified Data.ByteString.UTF8 as BS
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe

import           CalcEval
import           CalcParse

data App = App
  { _heist     :: Snaplet (Heist App),
    _value     :: Maybe Integer
  }

makeLenses ''App

instance HasHeist App where heistLens = subSnaplet heist

indexHandler :: Handler App App ()
indexHandler = do
  mInput <- getParam "input"
  case mInput of
    Just input -> do
      value .= (command . calcLine) (BS.toString input)
      mVal <- gets _value
      case mVal of
        Just val -> writeBS $ BS.fromString $ (show val)
        Nothing  -> writeBS "nope"
    Nothing -> render "index"

appInit :: SnapletInit App App
appInit = makeSnaplet "haskulator" description Nothing $ do
  hs <- nestSnaplet "" heist $ heistInit "templates"
  addRoutes [ ("static",    serveDirectory "static")
            , ("/",         indexHandler)
            ]
  return $ App hs Nothing
    where description = "The haskell calculator"

main :: IO ()
main = serveSnaplet defaultConfig appInit