module Web.Pixela
  ( -- * Types
    Client (config)
  , newClient
  , Config (..)
  , Exception (..)
  , FromParameter (..)
  , fromParameter'
  , ToParameter (..)
  , Token
  , UserName
  , Agreement (..)
  , Majority (..)
  , GraphId
  , GraphName
  , GraphUnit
  , GraphType (..)
  , GraphColor (..)
  , DateFormat
  , DisplayMode (..)
  , Url
  , Date
  , Quantity
  , WebhookType
  , WebhookHash
    -- * Constants
  , version
    -- * User functions
  , createUser
  , updateToken
  , deleteUser
    -- * Graph functions
  , createGraph
  , getGraphs
  , getGraphsBSL
  , getGraph
  , updateGraph
  , deleteGraph
    -- * Quantity funcitons
  , setQuantity
  , getQuantity
  , getQuantityBSL
  , updateQuantity
  , incrementQuantity
  , decrementQuantity
  , deleteQuantity
    -- * Webhook functions
  , createWebhook
  , getWebhooks
  , getWebhooksBSL
  , invokeWebhook
  , deleteWebhook
  ) where

import           Control.Exception          hiding (Exception)
import qualified Control.Exception          as E (Exception)
import           Control.Monad
import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Default
import qualified Data.HashMap.Strict        as HashMap
import           Data.Maybe                 (fromMaybe)
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Typeable
import qualified Data.Vector                as Vector
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import qualified Network.URI.Encode         as URI

-- | Pixela client.
--
-- @
-- import Web.Pixela
-- import Data.Default (def)
--
-- main :: IO
-- main = do
--   _client <- newClient def
--   pure ()
-- @
data Client =
  Client
    { config       :: Config
    , _httpManager :: Manager
    }

-- | Client configuration.
data Config =
  Config
    { endPoint            :: Url
    , httpManagerSettings :: ManagerSettings
    , userName            :: UserName
    , token               :: Token
    }

instance Default Config where
  def =
    Config
      "https://pixe.la/v1"
      tlsManagerSettings
      ""
      ""

newtype Exception =
  JsonException String
  deriving (Show, Eq, Typeable)

instance E.Exception Exception

-- | A type class to create a value from 'String'.
class FromParameter a where
  -- | If you know this does not fail, you can use 'fromParameter''.
  fromParameter :: String -> Maybe a

-- | Create value from 'String'.
-- This function is unsafe, applys 'error' if fails.
-- 'fromParameter' is safe.
fromParameter' :: (FromParameter a) => String -> a
fromParameter' param =
  fromMaybe
    (error $ "pixela: error parsing \"" <> param <> "\"")
    (fromParameter param)

-- | A type class to convert a value to 'String'.
class ToParameter a where
  toParameter :: a -> String

type Token = String
type UserName = String

-- | Which to agree <https://github.com/a-know/Pixela/wiki/Terms-of-Service terms of service> or not.
data Agreement = Agree | Disagree deriving (Show, Read, Eq)

-- | Major (adult) or minor (child).
data Majority = Major | Minor deriving (Show, Read, Eq)

type GraphId = String
type GraphName = String
type GraphUnit = String

data GraphType = GraphTypeInt | GraphTypeFloat deriving (Show, Read, Eq)

instance FromParameter GraphType where
  fromParameter "int"   = Just GraphTypeInt
  fromParameter "float" = Just GraphTypeFloat
  fromParameter _       = Nothing

instance ToParameter GraphType where
  toParameter GraphTypeInt   = "int"
  toParameter GraphTypeFloat = "float"

data GraphColor
  = Shibafu -- ^ Green
  | Momiji -- ^ Red
  | Sora -- ^ Blue
  | Ichou -- ^ Yellow
  | Ajisai -- ^ Purple
  | Kuro -- ^ Black
  deriving (Show, Read, Eq)

instance FromParameter GraphColor where
  fromParameter "shibafu" = Just Shibafu
  fromParameter "momiji"  = Just Momiji
  fromParameter "sora"    = Just Sora
  fromParameter "ichou"   = Just Ichou
  fromParameter "ajisai"  = Just Ajisai
  fromParameter "kuro"    = Just Kuro
  fromParameter _         = Nothing

instance ToParameter GraphColor where
  toParameter Shibafu = "shibafu"
  toParameter Momiji  = "momiji"
  toParameter Sora    = "sora"
  toParameter Ichou   = "ichou"
  toParameter Ajisai  = "ajisai"
  toParameter Kuro    = "kuro"

type DateFormat = String

data DisplayMode = ShortMode | DefaultMode deriving (Show, Read, Eq)

instance FromParameter DisplayMode where
  fromParameter "short" = Just ShortMode
  fromParameter ""      = Just DefaultMode
  fromParameter _       = Nothing

instance ToParameter DisplayMode where
  toParameter ShortMode   = "short"
  toParameter DefaultMode = ""

type Url = String
type Date = String
type Quantity = String

data WebhookType = Increment | Decrement deriving (Show, Read, Eq)

instance FromParameter WebhookType where
  fromParameter "increment" = Just Increment
  fromParameter "decrement" = Just Decrement
  fromParameter _           = Nothing

instance ToParameter WebhookType where
  toParameter Increment = "increment"
  toParameter Decrement = "decrement"

type WebhookHash = String

-- | Create new client.
newClient :: Config -> IO Client
newClient c =
  Client c <$> newManager (httpManagerSettings c)

-- | Create a user.
-- <https://pixe.la/#api-detail-post-users>
createUser :: Agreement -> Majority -> Client -> IO ()
createUser agreement majority (Client (Config ep _ userName' token') manager) =
  void $
    request
      POST
      (ep </> "users")
      Nothing
      ( Just $ Aeson.object
          [ "token'" .= token'
          , "username" .= userName'
          , "agreeTermsOfService" .= case agreement of Agree -> "yes"; Disagree -> "no" :: String
          , "notMinor" .= case majority of Major -> "yes"; Minor -> "no" :: String
          ]
      )
      manager

-- | Update user token.
-- <https://pixe.la/#api-detail-put-user>
updateToken :: Token -> Client -> IO ()
updateToken newToken (Client (Config ep _ userName' token') manager) =
  void $
    request
      PUT
      (ep </> "users" </> URI.encode userName')
      (Just token')
      (Just $ Aeson.object ["newToken" .= newToken])
      manager

-- | Delete the user.
-- <https://pixe.la/#api-detail-delete-user>
deleteUser :: Client -> IO ()
deleteUser (Client (Config ep _ userName' token') manager) =
  void $
    request
      DELETE
      (ep </> "users" </> URI.encode userName')
      (Just token')
      Nothing
      manager

-- | Create a graph.
-- <https://pixe.la/#api-detail-post-graphs>
createGraph :: GraphId -> GraphName -> GraphUnit -> GraphType -> GraphColor -> Client -> IO ()
createGraph graphId graphName graphUnit graphType graphColor (Client (Config ep _ userName' token') manager) =
  void $
    request
      POST
      (ep </> "users" </> URI.encode userName' </> "graphs")
      (Just token')
      ( Just $ Aeson.object
          [ "id" .= graphId
          , "name" .= graphName
          , "unit" .= graphUnit
          , "type" .= toParameter graphType
          , "color" .= toParameter graphColor
          ]
      )
      manager

-- | Get the list of infomation of graphs.
-- <https://pixe.la/#api-detail-get-graphs>
getGraphs :: Client -> IO Aeson.Value
getGraphs client =
  getGraphsBSL client >>= decodeJson

-- | Get the list of infomation of graphs.
-- <https://pixe.la/#api-detail-get-graphs>
getGraphsBSL :: Client -> IO BSL.ByteString
getGraphsBSL (Client (Config ep _ userName' token') manager) =
  requestBSL
    GET
    (ep </> "users" </> URI.encode userName' </> "graphs")
    (Just token')
    Nothing
    manager

-- | Get the graph.
-- <https://pixe.la/#api-detail-get-graph>
getGraph :: GraphId -> Maybe DateFormat -> DisplayMode -> Client -> IO BSL.ByteString
getGraph graphId maybeFormat mode (Client (Config ep _ userName' _) manager) =
  requestBSL
    GET
    (ep </> "users" </> URI.encode userName' </> "graphs" </> URI.encode graphId)
    Nothing
    ( case (mode, maybeFormat) of
        (DefaultMode, Nothing) -> Nothing
        _ ->
          Just $ Aeson.object $
            case mode of
              ShortMode   -> ["mode" .= toParameter mode]
              DefaultMode -> []
            ++
            case maybeFormat of
              Just format -> ["date" .= format]
              Nothing     -> []
    )
    manager

-- | Update the graph.
-- <https://pixe.la/#api-detail-put-graph>
updateGraph :: GraphId -> Maybe GraphName -> Maybe GraphUnit -> Maybe GraphColor -> [Url] -> Client -> IO ()
updateGraph _ Nothing Nothing Nothing [] _ = pure ()
updateGraph graphId maybeGraphName maybeGraphUnit maybeGraphColor purgeCacheUrls (Client (Config ep _ userName' token') manager) =
  void $
    request
      PUT
      (ep </> "users" </> URI.encode userName' </> "graphs" </> URI.encode graphId)
      (Just token')
      ( Just $ Aeson.object $
          case maybeGraphName of
            Just graphName -> ["name" .= Aeson.String (Text.pack graphName)]
            Nothing        -> []
          ++
          case maybeGraphUnit of
            Just graphUnit -> ["unit" .= Aeson.String (Text.pack graphUnit)]
            Nothing        -> []
          ++
          case maybeGraphColor of
            Just graphColor -> ["color" .= Aeson.String (Text.pack $ toParameter graphColor)]
            Nothing         -> []
          ++
          case purgeCacheUrls of
            _:_ -> ["purgeCacheURLs" .= Aeson.Array (Vector.fromList $ map (Aeson.String . Text.pack) purgeCacheUrls)]
            []  -> []
      )
      manager

-- | Delete the graph.
-- <https://pixe.la/#api-detail-delete-graph>
deleteGraph :: GraphId -> Client -> IO ()
deleteGraph graphId (Client (Config ep _ userName' token') manager) =
  void $
    request
      DELETE
      (ep </> "users" </> URI.encode userName' </> "graphs" </> URI.encode graphId)
      (Just token')
      Nothing
      manager

-- | Set quantity pixel.
-- <https://pixe.la/#api-detail-post-pixels>
setQuantity :: GraphId -> Date -> Quantity -> Client -> IO ()
setQuantity graphId date quantity (Client (Config ep _ userName' token') manager) =
  void $
    request
      POST
      (ep </> "users" </> URI.encode userName' </> "graphs" </> URI.encode graphId)
      (Just token')
      ( Just $ Aeson.object
          [ "date" .= date
          , "quantity" .= quantity
          ]
      )
      manager

-- | Get quantity pixel.
-- <https://pixe.la/#api-detail-get-pixel>
getQuantity :: GraphId -> Date -> Client -> IO Aeson.Value
getQuantity graphId date client =
  getQuantityBSL graphId date client >>= decodeJson

-- | Get quantity pixel.
-- <https://pixe.la/#api-detail-get-pixel>
getQuantityBSL :: GraphId -> Date -> Client -> IO BSL.ByteString
getQuantityBSL graphId date (Client (Config ep _ userName' token') manager) =
  requestBSL
    GET
    (ep </> "users" </> URI.encode userName' </> "graphs" </> URI.encode graphId </> URI.encode date)
    (Just token')
    Nothing
    manager

-- | Update quantity pixel.
-- <https://pixe.la/#api-detail-put-pixel>
updateQuantity :: GraphId -> Date -> Quantity -> Client -> IO ()
updateQuantity graphId date quantity (Client (Config ep _ userName' token') manager) =
  void $
    request
      PUT
      (ep </> "users" </> URI.encode userName' </> "graphs" </> URI.encode graphId </> URI.encode date)
      (Just token')
      (Just $ Aeson.object ["quantity" .= quantity])
      manager

-- | Increment quantity pixel.
-- <https://pixe.la/#api-detail-pixel-increment>
incrementQuantity :: GraphId -> Client -> IO ()
incrementQuantity graphId (Client (Config ep _ userName' token') manager) =
  void $
    request
      PUT
      (ep </> "users" </> URI.encode userName' </> "graphs" </> URI.encode graphId </> "increment")
      (Just token')
      Nothing
      manager

-- | Decrement quantity pixel.
-- <https://pixe.la/#api-detail-pixel-decrement>
decrementQuantity :: GraphId -> Client -> IO ()
decrementQuantity graphId (Client (Config ep _ userName' token') manager) =
  void $
    request
      PUT
      (ep </> "users" </> URI.encode userName' </> "graphs" </> URI.encode graphId </> "decrement")
      (Just token')
      Nothing
      manager

-- | Delete quantity pixel.
-- <https://pixe.la/#api-detail-delete-pixel>
deleteQuantity :: GraphId -> Date -> Client -> IO ()
deleteQuantity graphId date (Client (Config ep _ userName' token') manager) =
  void $
    request
      DELETE
      (ep </> "users" </> URI.encode userName' </> "graphs" </> URI.encode graphId </> URI.encode date)
      (Just token')
      Nothing
      manager

-- | Create a webhook.
-- <https://pixe.la/#api-detail-post-webhooks>
createWebhook :: GraphId -> WebhookType -> Client -> IO Text
createWebhook graphId type' (Client (Config ep _ userName' token') manager) = do
  response <-
    request
      POST
      (ep </> "users" </> URI.encode userName' </> "webhooks")
      (Just token')
      ( Just $ Aeson.object
          [ "graphID" .= graphId
          , "type" .= toParameter type'
          ]
      )
      manager
  let
    throw' = throwIO $ JsonException "hash string cannot be extracted"
  case response of
    Aeson.Object o ->
      case HashMap.lookup "webhookHash" o of
        Just (Aeson.String hashString) -> pure hashString
        _                              -> throw'
    _ -> throw'

-- | Get the webhook.
-- <https://pixe.la/#api-detail-get-webhooks>
getWebhooks :: Client -> IO Aeson.Value
getWebhooks client =
  getWebhooksBSL client >>= decodeJson

-- | Get the webhook.
-- <https://pixe.la/#api-detail-get-webhooks>
getWebhooksBSL :: Client -> IO BSL.ByteString
getWebhooksBSL (Client (Config ep _ userName' token') manager) =
  requestBSL
    GET
    (ep </> "users" </> URI.encode userName' </> "webhooks")
    (Just token')
    Nothing
    manager

-- | Invoke the webhook.
-- <https://pixe.la/#api-detail-post-webhook>
invokeWebhook :: WebhookHash -> Client -> IO ()
invokeWebhook hash (Client (Config ep _ userName' _) manager) =
  void $
    request
      POST
      (ep </> "users" </> URI.encode userName' </> "webhooks" </> URI.encode hash)
      Nothing
      Nothing
      manager

-- | Delete the webhook.
-- <https://pixe.la/#api-detail-delete-webhook>
deleteWebhook :: WebhookHash -> Client -> IO ()
deleteWebhook hash (Client (Config ep _ userName' token') manager) =
  void $
    request
      DELETE
      (ep </> "users" </> URI.encode userName' </> "webhooks" </> URI.encode hash)
      (Just token')
      Nothing
      manager

request :: StdMethod -> String -> Maybe Token -> Maybe Aeson.Value -> Manager -> IO Aeson.Value
request method' uri maybeToken maybeBody manager =
  requestBSL method' uri maybeToken maybeBody manager >>= decodeJson

requestBSL :: StdMethod -> String -> Maybe Token -> Maybe Aeson.Value -> Manager -> IO BSL.ByteString
requestBSL method' uri maybeToken maybeBody manager = do
  request' <- parseUrlThrow uri
  let
    request'' =
      request'
        { method = renderStdMethod method'
        , requestHeaders =
            ("User-Agent", "Pixela Haskell Client " <> version) :
            case maybeToken of
              Just token' -> [("X-USER-TOKEN", BS.pack token')]
              Nothing     -> []
        , requestBody =
            RequestBodyLBS $
              case maybeBody of
                Just body -> Aeson.encode body
                Nothing   -> ""
        }
  response <- httpLbs request'' manager
  pure $ responseBody response

decodeJson :: BSL.ByteString -> IO Aeson.Value
decodeJson responseBody' =
  case Aeson.decode responseBody' of
    Nothing -> throwIO $ JsonException "failed to parse response body as JSON"
    Just b  -> pure b

(</>) :: (Semigroup p, IsString p) => p -> p -> p
p </> q = p <> "/" <> q
infixr 6 </>

version :: (IsString a) => a
version = "1.0.0"
