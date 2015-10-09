{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PathJS.RouteHandler where


import           GHCJS.Foreign.Callback
import           GHCJS.Types



foreign import javascript unsafe "Path.map($1).to(function() { var newRoute = $2; newRoute(); });" jq_Path  :: JSString -> Callback (IO ())  -> IO JSRef
foreign import javascript unsafe "Path.listen();" jq_Path_Listen  ::  IO ()

createPath :: JSString -> Callback (IO ()) -> IO JSRef 
createPath = jq_Path

listenPath :: IO ()
listenPath = jq_Path_Listen


