{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Default
import           Data.List.Split            (splitOn)
import qualified Data.Text.IO               as Text
import           Data.Version               (showVersion)
import           Options.Declarative
import           Web.Pixela                 (Agreement (..), Config (token, userName), DisplayMode (..), Majority (..),
                                             fromParameter', newClient)
import qualified Web.Pixela                 as P

import           Paths_pixela_cli           (version)

main :: IO ()
main =
  run "pixela-cli" (Just $ showVersion version) $
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
createUser agreement majority userName' token' =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.createUser (if get agreement then Agree else Disagree) (if get majority then Major else Minor)

updateToken
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "NEW_TOKEN" String
  -> Cmd "update the token" ()
updateToken userName' token' newToken =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.updateToken (get newToken)

deleteUser
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Cmd "delete the user" ()
deleteUser userName' token' =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.deleteUser

createGraph
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "GRAPH_NAME" String
  -> Arg "GRAPH_UNIT" String
  -> Arg "GRAPH_TYPE" String
  -> Arg "GRAPH_COLOR" String
  -> Cmd "create a graph" ()
createGraph userName' token' graphId graphName graphUnit graphType graphColor =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.createGraph (get graphId) (get graphName) (get graphUnit) (fromParameter' $ get graphType) (fromParameter' $ get graphColor)

getGraphs
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Cmd "list up own graphs" ()
getGraphs userName' token' =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.getGraphsBSL >>= BSL.putStrLn

getGraph
  :: Flag "d" '["date"] "FORMAT" "date format like \"yyyyMMdd\"" (Maybe String)
  -> Flag "s" '["short"] "" "short mode or not" Bool
  -> Arg "USER_NAME" String
  -> Arg "GRAPH_ID" String
  -> Cmd "get the graph SVG" ()
getGraph maybeFormat short userName' graphId =
  liftIO $
    newClient def { userName = get userName' }
    >>= P.getGraph
      (get graphId)
      (get maybeFormat)
      ( if get short
          then ShortMode
          else DefaultMode
      )
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
updateGraph maybeGraphName maybeGraphUnit maybeGraphColor purgeCacheUrls userName' token' graphId =
  liftIO $
    let
      urls =
        case splitOn "," $ get purgeCacheUrls of
          [""]  -> []
          urls' -> urls'
    in
      newClient def { userName = get userName', token = get token' }
      >>= P.updateGraph (get graphId) (get maybeGraphName) (get maybeGraphUnit) (fromParameter' <$> get maybeGraphColor) urls

deleteGraph
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Cmd "delete the graph" ()
deleteGraph userName' token' graphId =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.deleteGraph (get graphId)

setQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "DATE" String
  -> Arg "QUANTITY" String
  -> Cmd "set the quantity of the specified date" ()
setQuantity userName' token' graphId date quantity =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.setQuantity (get graphId) (get date) (get quantity)

getQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "DATE" String
  -> Cmd "get the quantity of the specified date" ()
getQuantity userName' token' graphId date =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.getQuantityBSL (get graphId) (get date)
    >>= BSL.putStrLn

updateQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "DATE" String
  -> Arg "QUANTITY" String
  -> Cmd "update the quantity of the specified date" ()
updateQuantity userName' token' graphId date quantity =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.updateQuantity (get graphId) (get date) (get quantity)

incrementQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Cmd "increment the quantity of the specified date" ()
incrementQuantity userName' token' graphId =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.incrementQuantity (get graphId)

decrementQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Cmd "decrement the quantity of the specified date" ()
decrementQuantity userName' token' graphId =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.decrementQuantity (get graphId)

deleteQuantity
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "DATE" String
  -> Cmd "update the quantity of the specified date" ()
deleteQuantity userName' token' graphId date =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.deleteQuantity (get graphId) (get date)

createWebhook
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "GRAPH_ID" String
  -> Arg "TYPE" String
  -> Cmd "create a new webhook" ()
createWebhook userName' token' graphId type' =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.createWebhook (get graphId) (fromParameter' $ get type')
    >>= Text.putStrLn

getWebhooks
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Cmd "get webhooks" ()
getWebhooks userName' token' =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.getWebhooksBSL
    >>= BSL.putStrLn

invokeWebhook
  :: Arg "USER_NAME" String
  -> Arg "WEBHOOK_HASH" String
  -> Cmd "invoke the webhook" ()
invokeWebhook userName' hash =
  liftIO $
    newClient def { userName = get userName' }
    >>= P.invokeWebhook (get hash)

deleteWebhook
  :: Arg "USER_NAME" String
  -> Arg "TOKEN" String
  -> Arg "WEBHOOK_HASH" String
  -> Cmd "delete the webhook" ()
deleteWebhook userName' token' hash =
  liftIO $
    newClient def { userName = get userName', token = get token' }
    >>= P.deleteWebhook (get hash)
