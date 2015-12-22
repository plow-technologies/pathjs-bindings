{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PathJS.RouteHandler where


import           GHCJS.Foreign.Callback
import           GHCJS.Types

import           GHCJS.Marshal               (FromJSVal(..))
import           JavaScript.Object           (getProp, allProps, Object)
import qualified JavaScript.Object.Internal  as OI
import           JavaScript.Array            hiding (take)
import           Data.JSString               (pack, unpack, JSString)
import           Data.JSString.Text          (textToJSString, textFromJSVal)

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M



foreign import javascript unsafe "Path.map($1).to(function() { var newRoute = $2; newRoute(); });" jq_Path  :: JSString -> Callback (IO ())  -> IO JSVal
foreign import javascript unsafe "Path.listen();" jq_Path_Listen  ::  IO ()

foreign import javascript unsafe "Path.root($1);" jq_Path_Root :: JSString -> IO ()

createPath :: JSString -> Callback (IO ()) -> IO JSVal 
createPath = jq_Path

listenPath :: IO ()
listenPath = jq_Path_Listen

rootPath :: JSString -> IO ()
rootPath = jq_Path_Root


foreign import javascript unsafe "Path.map($1).to(function() {var newRoute = $2; newRoute(this.params); console.log(this.params); });" jq_Dynamic_Path  :: JSString -> Callback (JSVal -> IO ())  -> IO JSVal

createDynamicPath :: JSString -> Callback (JSVal -> IO ()) -> IO JSVal 
createDynamicPath = jq_Dynamic_Path

newtype URLParams = URLParams { _getUrlParams :: Map JSString JSString
                              } deriving (Eq, Show)

instance FromJSVal URLParams where
  fromJSVal ref = do
    let objectCandidate = OI.Object ref
    parameterArray <- allProps objectCandidate
    let allParameterPieces = map (textToJSString . textFromJSVal) $ toList parameterArray
    finishedHash <- Prelude.foldr (captureAllPropertyValues objectCandidate) (return M.empty) allParameterPieces
    return . Just . URLParams $ finishedHash

captureAllPropertyValues objectCandidate incomingParameter accumulatingMapIO = do
  hm <- accumulatingMapIO
  property <- getProp incomingParameter objectCandidate
  maybePropertyAsString <- fromJSVal property :: IO (Maybe JSString)
  maybe accumulatingMapIO (return . updatedMap hm) maybePropertyAsString
 where  
    updatedMap :: Map JSString JSString -> JSString -> Map JSString JSString
    updatedMap hm property = M.insert incomingParameter property hm


