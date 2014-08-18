{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import qualified Data.Text as T
import           Data.Text.Encoding
import           Heist
import           Heist.Interpreted
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
      value .= (command . calcLine . T.unpack . decodeUtf8) input
      render "index"
    Nothing -> render "index"

appInit :: SnapletInit App App
appInit = makeSnaplet "haskulator" description Nothing $ do
  hs <- nestSnaplet "heist" heist $ heistInit "templates"
  modifyHeistState $ bindAttributeSplices [("main-textbox", resultAttributeSplice)]
  addRoutes [ ("static",    serveDirectory "static")
            , ("/",         indexHandler)
            ]
  return $ App hs (Just 1337)
    where description = "The haskell calculator"

resultAttributeSplice :: AttrSplice (Handler App App)
resultAttributeSplice _ = do
  mValue <- lift $ use value
  case mValue of
    Just val -> return [("value", T.pack $ show val)]
    Nothing -> return []

main :: IO ()
main = serveSnaplet defaultConfig appInit