 {-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.UTF8 as U
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe, maybeToList)
import Network.HTTP
import Network.Stream
import Network.URI
import qualified Data.ByteString.Base64 as Base64
import Network.XmlRpc.Server 
import Network.XmlRpc.Internals
import Snap (liftIO)
import Snap.Core hiding (methods, rqMethod, rqURI, POST, Request)
import Snap.Util.FileServe
import Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route
  [ ("xmlrpc/common", commonHandler)
  -- [ ("xmlrpc/common", fallthrough "/xmlrpc/common")
  , ("xmlrpc/object", fallthrough "/xmlrpc/object")
  ]

commonHandler :: Snap ()
commonHandler = do
  body <- LC.unpack <$> readRequestBody (1024 * 1024) -- TODO Is it enough ?
  bs <- liftIO $ handleCall commonMethods body
  writeLBS bs

fallthrough :: String -> Snap ()
fallthrough to = do
  body <- LC.unpack <$> readRequestBody (1024 * 1024) -- TODO Is it enough ?
  c <- liftIO $ handleError (return . Left . Fault 0) (Right <$> parseCall body)
  case c of
    Left e -> writeLBS $ renderResponse e
    Right c' -> do
      response <- liftIO $ post ("http://trunk-27602.runbot.openerp.com:9335" ++ to) [] $ renderCall c'
      -- writeLBS $ renderResponse $ parseResponse response
      writeLBS $ LC.pack response

commonMethods :: MethodCall -> ServerResult
commonMethods = methods
  [ ("login", fun login)
  ]

login :: String -> String -> String -> IO Int
login databaseName login password = return 1


----------------------------------------------------------------------
-- This is code from HaXR, not exported from Network.XmlRpc.Client.
----------------------------------------------------------------------

userAgent :: String
userAgent = "openerpfront/0.0.0 (github.com/noteed/openerpfront)"

handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

post :: String -> [Header] -> LC.ByteString -> IO String
post url headers content = do
    uri <- maybeFail ("Bad URI: '" ++ url ++ "'") (parseURI url)
    let a = uriAuthority uri
    auth <- maybeFail ("Bad URI authority: '" ++ show (fmap showAuth a) ++ "'") a
    post_ uri auth headers content
  where showAuth (URIAuth u r p) = "URIAuth "++u++" "++r++" "++p

post_ :: URI -> URIAuth -> [Header] -> LC.ByteString -> IO String
post_ uri auth headers content = do
    eresp <- simpleHTTP (request uri auth headers (C.concat . LC.toChunks $ content))
    resp <- handleE (fail . show) eresp
    case rspCode resp of
                      (2,0,0) -> return (U.toString (rspBody resp))
                      _ -> fail (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

request :: URI -> URIAuth -> [Header] -> C.ByteString -> Request C.ByteString
request uri auth usrHeaders content = Request{ rqURI = uri,
                                               rqMethod = POST,
                                               rqHeaders = headers,
                                               rqBody = content }
    where
    -- the HTTP module adds a Host header based on the URI
    headers = [Header HdrUserAgent userAgent,
               Header HdrContentType "text/xml",
               Header HdrContentLength (show (C.length content))
              ] ++ maybeToList (uncurry authHdr . parseUserInfo $ auth)
                ++ usrHeaders
    parseUserInfo info = let (u,pw) = break (==':') $ uriUserInfo info
                         in ( if null u then Nothing else Just u
                            , if null pw then Nothing else Just (tail pw))

authHdr :: Maybe String -- ^ User name, if any
        -> Maybe String -- ^ Password, if any
        -> Maybe Header -- ^ If user name or password was given, returns
                        --   an Authorization header, otherwise 'Nothing'
authHdr Nothing Nothing = Nothing
authHdr u p = Just (Header HdrAuthorization ("Basic " ++ base64encode user_pass))
        where user_pass    = fromMaybe "" u ++ ":" ++ fromMaybe "" p
              base64encode = C.unpack . Base64.encode . C.pack

maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return
