{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default
import Data.List.Split (splitOn)
import qualified Data.Text.IO as Text
import Options.Declarative
import Web.Pixela (Agreement (..), Majority (..), DisplayMode (..), version, newClient, fromParameter')
import qualified Web.Pixela as P

main :: IO ()
main =
  run "pixela" (Just version) $
    Group "Pixela client"
      [ subCmd "create-user" createUser
      , subCmd "update-token" updateToken
      , subCmd "delete-user" deleteUser
      , subCmd "create-graph" createGraph
      , subCmd "get-graphs" getGraphs
      , subCmd "get-graph" getGraph
      , subCmd "update-graph" updateGraph
      , subCmd "delete-graph" deleteGraph
      , subCmd "set-quantity" setQuantity
      , subCmd "get-quantity" getQuantity
      , subCmd "update-quantity" updateQuantity
      , subCmd "increment-quantity" incrementQuantity
      , subCmd "decrement-quantity" decrementQuantity
      , subCmd "delete-quantity" deleteQuantity
      , subCmd "create-webhook" createWebhook
      , subCmd "get-webhooks" getWebhooks
      , subCmd "invoke-webhook" invokeWebhook
      , subCmd "delete-webhook" deleteWebhook
      ]

createUser
  :: Flag "a" '["agreement"] "" "agreement to the terms of service" Bool
  -> Flag "m" '["major"] "" "major or mainor" Bool
  -> Arg "USER_NAME" String
  -> Arg "NEW_TOKEN" String
  -> Cmd "create a new user" ()
createUser agreement majority userName token =
  liftIO $ do
    client <- newClient def
    P.createUser (get userName) (get token) (if get agreement then Agree else Disagree) (if get majority then Major else Minor) client

updateToken
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "NEW_TOKEN" String
  -> Cmd "update the token" ()
updateToken userName token newToken =
  liftIO $ do
    client <- newClient def
    P.updateToken (get userName) (get token) (get newToken) client

deleteUser
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Cmd "delete the user" ()
deleteUser userName token =
  liftIO $ do
    client <- newClient def
    P.deleteUser (get userName) (get token) client

createGraph
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "GRAPH_NAME" String
  -> Arg "GRAPH_UNIT" String
  -> Arg "GRAPH_TYPE" String
  -> Arg "GRAPH_COLOR" String
  -> Cmd "create a graph" ()
createGraph userName token graphId graphName graphUnit graphType graphColor =
  liftIO $ do
    client <- newClient def
    P.createGraph (get userName) (get token) (get graphId) (get graphName) (get graphUnit) (fromParameter' $ get graphType) (fromParameter' $ get graphColor) client

getGraphs
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Cmd "list up own graphs" ()
getGraphs userName token =
  liftIO $ do
    client <- newClient def
    P.getGraphsBSL (get userName) (get token) client >>= BSL.putStrLn
    
getGraph
  :: Flag "d" '["date"] "FORMAT" "date format like \"yyyyMMdd\"" (Maybe String)
  -> Flag "s" '["short"] "" "short mode or not" Bool
  -> Arg "USER_NAME" String
  -> Arg "GRAPH_ID" String
  -> Cmd "get the graph SVG" ()
getGraph maybeFormat short userName graphId =
  liftIO $ do
    client <- newClient def
    P.getGraph
      (get userName)
      (get graphId)
      (get maybeFormat)
      ( if get short
          then ShortMode
          else DefaultMode
      )
      client
    >>= BSL.putStrLn

updateGraph
  :: Flag "n" '["name"] "GRAPH_NAME" "graph name" (Maybe String)
  -> Flag "u" '["unit"] "GRAPH_UNIT" "graph unit" (Maybe String)
  -> Flag "c" '["color"] "GRAPH_COLOR" "graph color. shibafu (green), momiji (red), sora (blue), ichou (yellow), ajisai (purple) or kuro (black)" (Maybe String)
  -> Flag "p" '["purge"] "PURGE_CACHE_URLS" "url list separated by \",\" to purge" (Def "" String)
  -> Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Cmd "update the graph" ()
updateGraph maybeGraphName maybeGraphUnit maybeGraphColor purgeCacheUrls userName token graphId =
  liftIO $ do
    client <- newClient def
    let
      urls = 
        case splitOn "," $ get purgeCacheUrls of
          [""] -> []
          urls' -> urls'
    P.updateGraph (get userName) (get token) (get graphId) (get maybeGraphName) (get maybeGraphUnit) (fromParameter' <$> get maybeGraphColor) urls client

deleteGraph
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Cmd "delete the graph" ()
deleteGraph userName token graphId =
  liftIO $ do
    client <- newClient def
    P.deleteGraph (get userName) (get token) (get graphId) client

setQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "DATE" String
  -> Arg "QUANTITY" String
  -> Cmd "set the quantity of the specified date" ()
setQuantity userName token graphId date quantity =
  liftIO $ do
    client <- newClient def
    P.setQuantity (get userName) (get token) (get graphId) (get date) (get quantity) client

getQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "DATE" String
  -> Cmd "get the quantity of the specified date" ()
getQuantity userName token graphId date =
  liftIO $ do
    client <- newClient def
    P.getQuantityBSL (get userName) (get token) (get graphId) (get date) client
    >>= BSL.putStrLn

updateQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "DATE" String
  -> Arg "QUANTITY" String
  -> Cmd "update the quantity of the specified date" ()
updateQuantity userName token graphId date quantity =
  liftIO $ do
    client <- newClient def
    P.updateQuantity (get userName) (get token) (get graphId) (get date) (get quantity) client

incrementQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Cmd "increment the quantity of the specified date" ()
incrementQuantity userName token graphId =
  liftIO $ do
    client <- newClient def
    P.incrementQuantity (get userName) (get token) (get graphId) client

decrementQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Cmd "decrement the quantity of the specified date" ()
decrementQuantity userName token graphId =
  liftIO $ do
    client <- newClient def
    P.decrementQuantity (get userName) (get token) (get graphId) client

deleteQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "DATE" String
  -> Cmd "update the quantity of the specified date" ()
deleteQuantity userName token graphId date =
  liftIO $ do
    client <- newClient def
    P.deleteQuantity (get userName) (get token) (get graphId) (get date) client

createWebhook
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "TYPE" String
  -> Cmd "create a new webhook" ()
createWebhook userName token graphId type' =
  liftIO $ do
    client <- newClient def
    P.createWebhook (get userName) (get token) (get graphId) (fromParameter' $ get type') client
    >>= Text.putStrLn

getWebhooks
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Cmd "get webhooks" ()
getWebhooks userName token =
  liftIO $ do
    client <- newClient def
    P.getWebhooksBSL (get userName) (get token) client
    >>= BSL.putStrLn

invokeWebhook
  :: Arg "USER_NAME" String
  -> Arg "WEBHOOK_HASH" String
  -> Cmd "get webhooks" ()
invokeWebhook userName hash =
  liftIO $ do
    client <- newClient def
    P.invokeWebhook (get userName) (get hash) client

deleteWebhook
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "WEBHOOK_HASH" String
  -> Cmd "delete the webhook" ()
deleteWebhook userName token hash =
  liftIO $ do
    client <- newClient def
    P.deleteWebhook (get userName) (get token) (get hash) client
