{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE DataKinds                #-}

module Main where

import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           GHCJS.VDOM
import           Data.JSString               (JSString)
import qualified Data.JSString               as JS (pack, unpack)

import           JavaScript.Object

import           LiveVDom.Adapter.Types
import           LiveVDom.Components
import           LiveVDom.Event
import           LiveVDom.Message
import           LiveVDom.Render
import           LiveVDom.Types
import           Prelude                       hiding (div, sequence)
import           System.IO
import           Text.Read
import           Valentine

import           Servant.API
import           Servant.Client
import           Servant.Common.Req

import           PathJS.RouteHandler

main :: IO ()
main = do  
  runMain
      
runMain :: IO ()
runMain = do
  container <- createContainer
  putStrLn "begin runHomePage"
  [js_|console.log(`container)|]

  putStrLn "rendering"
  
  runRouteHandler (firstPage container) (secondPage container)
 where
   firstPage container = runDomI container (putStrLn "First render") $ return pageOne
   secondPage container = runDomI container (putStrLn "Secondrender") $ return pageTwo
    
pageOne :: LiveVDom Attribute
pageOne = renderPageOne
  where renderPageOne = [valentine|
<div>
  Hello World Page One!!!
|]

pageTwo :: LiveVDom Attribute
pageTwo = renderPageTwo
  where renderPageTwo = [valentine|
<div>
  Hello World Page Two!!!
|]

runARunDOMI myDomI = do
  syncCallback ThrowWouldBlock myDomI

runRouteHandler :: IO () -> IO () -> IO ()
runRouteHandler myDomI1 myDomI2 = do
  cb1 <- (runARunDOMI myDomI1) 
  _ <- createPath "#/users" cb1
  cb2 <- (runARunDOMI myDomI2) 
  _ <- createPath "#/losers" cb2
  listenPath


type RouteApi = "#/users"  :> ReqBody '[JSON] RouteType :> Get '[JSON] (IO ())
           :<|> "#/losers" :> ReqBody '[JSON] RouteType :> Get '[JSON] (IO ())

data RouteType = Users
               | Losers deriving (Show)


