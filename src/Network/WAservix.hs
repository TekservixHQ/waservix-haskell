{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.WAservix
-- Description : WAservix WhatsApp API Gateway client by Tekservix
--
-- WAservix WhatsApp API Gateway: send WhatsApp messages from your own number.
--
-- You need a Tekservix account with WAservix and your number's device API key
-- (it starts with @wpk_@, under WAservix > Devices). Sign up at
-- <https://tekservix.com/waservix>. API reference: <https://tekservix.com/waservix/docs/>.
--
-- > import Network.WAservix
-- >
-- > main :: IO ()
-- > main = do
-- >   c <- newClient "wpk_your_device_key"
-- >   r <- sendText c "+250788123456" "Your order has shipped"
-- >   print r
module Network.WAservix
  ( -- * Client
    Client
  , newClient
  , newClientWith
  , defaultBaseUrl
    -- * Sending
  , sendMessage
  , sendText
  , sendImage
  , sendDocument
  , sendLocation
    -- * Messages
  , getMessageInfo
  , editMessage
  , deleteMessage
  , resendMessage
    -- * Numbers, contacts and groups
  , onWhatsApp
  , getStatus
  , listContacts
  , getContact
  , listGroups
  , getGroupMetadata
    -- * Webhooks
  , verifyWebhook
    -- * Anything else
  , request
    -- * Errors
  , WAservixError (..)
  ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (statusCode, urlEncode)

-- | Connection to the WAservix API for one WhatsApp number.
data Client = Client
  { clientKey :: !B.ByteString
  , clientBase :: !String
  , clientManager :: !Manager
  }

-- | @https://tekservix.com/waservix/api@
defaultBaseUrl :: String
defaultBaseUrl = "https://tekservix.com/waservix/api"

-- | Client for the live API.
newClient :: Text -> IO Client
newClient key = newClientWith key defaultBaseUrl

-- | Client with another base URL (for tests or a proxy).
newClientWith :: Text -> String -> IO Client
newClientWith key base = Client (TE.encodeUtf8 key) (reverse (dropWhile (== '/') (reverse base))) <$> newTlsManager

-- | An API error: HTTP status and the message from the API.
data WAservixError = WAservixError
  { errorStatus :: !Int
  , errorMessage :: !Text
  , errorBody :: !BL.ByteString
  }
  deriving (Show)

instance Exception WAservixError

-- | Calls any endpoint and returns the @data@ part of the answer.
--
-- > request c "GET" "/user" Nothing
request :: Client -> B.ByteString -> String -> Maybe Value -> IO Value
request c method path body = do
  req0 <- parseRequest (clientBase c ++ path)
  let req =
        req0
          { method = method
          , requestHeaders =
              [ ("Authorization", "Bearer " <> clientKey c)
              , ("Accept", "application/json")
              , ("User-Agent", "waservix-haskell/1.0.0")
              ]
                ++ maybe [] (const [("Content-Type", "application/json")]) body
          , requestBody = maybe (requestBody req0) (RequestBodyLBS . A.encode) body
          }
  res <- httpLbs req (clientManager c)
  let code = statusCode (responseStatus res)
      raw = responseBody res
      parsed = A.decode raw :: Maybe Value
  if code >= 400
    then throwIO (WAservixError code (maybe (T.pack ("HTTP " ++ show code)) messageOf parsed) raw)
    else pure (maybe Null dataOf parsed)
  where
    dataOf (Object o) | Just d <- KM.lookup "data" o = d
    dataOf v = v
    messageOf (Object o) | Just (String m) <- KM.lookup "message" o = m
    messageOf _ = "request failed"

-- | Sends any message type to a number (+250788123456) or a group ID (…@g.us).
-- Field names follow the API: text, imageUrl, videoUrl, audioUrl, documentUrl,
-- fileName, location, poll, replyTo, mentions, viewOnce.
sendMessage :: Client -> Text -> [(Text, Value)] -> IO Value
sendMessage c to fields =
  request c "POST" "/send-message" . Just . object $
    ("to" .= to) : [(K.fromText k, v) | (k, v) <- fields, v /= Null]

-- | A text message.
sendText :: Client -> Text -> Text -> IO Value
sendText c to t = sendMessage c to [("text", String t)]

-- | An image from a public URL, with an optional caption.
sendImage :: Client -> Text -> Text -> Maybe Text -> IO Value
sendImage c to url caption = sendMessage c to [("imageUrl", String url), ("text", maybe Null String caption)]

-- | A document from a public URL, with an optional file name and caption.
sendDocument :: Client -> Text -> Text -> Maybe Text -> Maybe Text -> IO Value
sendDocument c to url name caption =
  sendMessage c to [("documentUrl", String url), ("fileName", maybe Null String name), ("text", maybe Null String caption)]

-- | A location pin, with an optional place name.
sendLocation :: Client -> Text -> Double -> Double -> Maybe Text -> IO Value
sendLocation c to lat lng name =
  sendMessage c to [("location", object (["latitude" .= lat, "longitude" .= lng] ++ maybe [] (\n -> ["name" .= n]) name))]

-- | Status of a sent message: queued, sent, delivered, read, played or failed.
getMessageInfo :: Client -> Int -> IO Value
getMessageInfo c i = request c "GET" ("/messages/" ++ show i ++ "/info") Nothing

-- | Edits a sent text (WhatsApp allows about 15 minutes).
editMessage :: Client -> Int -> Text -> IO Value
editMessage c i t = request c "PUT" ("/messages/" ++ show i) (Just (object ["text" .= t]))

-- | Deletes a message for everyone.
deleteMessage :: Client -> Int -> IO Value
deleteMessage c i = request c "DELETE" ("/messages/" ++ show i) Nothing

-- | Sends a failed message again.
resendMessage :: Client -> Int -> IO Value
resendMessage c i = request c "POST" ("/messages/" ++ show i ++ "/resend") Nothing

-- | Does this number have WhatsApp?
onWhatsApp :: Client -> Text -> IO Value
onWhatsApp c phone = request c "GET" ("/on-whatsapp/" ++ enc phone) Nothing

-- | Connection status of your number.
getStatus :: Client -> IO Value
getStatus c = request c "GET" "/status" Nothing

-- | Contacts of your number.
listContacts :: Client -> IO Value
listContacts c = request c "GET" "/contacts" Nothing

-- | One contact.
getContact :: Client -> Text -> IO Value
getContact c phone = request c "GET" ("/contacts/" ++ enc phone) Nothing

-- | Groups your number is in.
listGroups :: Client -> IO Value
listGroups c = request c "GET" "/groups" Nothing

-- | One group's details; the ID ends in @\@g.us@.
getGroupMetadata :: Client -> Text -> IO Value
getGroupMetadata c jid = request c "GET" ("/groups/" ++ T.unpack (T.strip jid) ++ "/metadata") Nothing

-- | Checks the @X-Webhook-Signature@ header against your number's webhook secret, in constant time.
verifyWebhook :: Text -> Text -> Bool
verifyWebhook signature secret =
  let a = TE.encodeUtf8 signature
      b = TE.encodeUtf8 secret
   in not (B.null b) && B.length a == B.length b && foldr (.|.) 0 (B.zipWith xor a b) == 0

enc :: Text -> String
enc = T.unpack . TE.decodeUtf8 . urlEncode False . TE.encodeUtf8
