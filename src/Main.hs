{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE DataKinds                #-}

module Main where

import           GHCJS.Foreign
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types


import           PathJS.RouteHandler

main :: IO ()
main = do  
  runMain
      
runMain :: IO ()
runMain = do
  putStrLn "begin runHomePage"

  putStrLn "rendering"
  
  runRouteHandler (putStrLn "page one") (putStrLn "page two")
    
runARunDOMI myDomI = do
  syncCallback ThrowWouldBlock myDomI
    
runRouteHandler :: IO () -> IO () -> IO ()
runRouteHandler myDomI1 myDomI2 = do
  cb1 <- (runARunDOMI myDomI1) 
  _ <- createPath "#/users" cb1
  cb2 <- (runARunDOMI myDomI2) 
  _ <- createPath "#/losers" cb2
  listenPath



