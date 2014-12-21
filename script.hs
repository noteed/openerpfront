 {-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.UTF8 as U
import Data.Char (intToDigit)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, maybeToList)
import Network.HTTP
import Network.Stream
import Network.URI
import qualified Data.ByteString.Base64 as Base64
import Network.XmlRpc.Server 
import Network.XmlRpc.Internals
import Snap (liftIO)
import Snap.Core hiding (methods, getHeaders, rqMethod, rqURI, GET, POST, Request, Response)
import qualified Snap.Core as Snap
import Snap.Util.FileServe
import Snap.Http.Server
import Web.Cookie

-- Example call:
-- python oe-xmlrpc-caller.py --host 172.17.1.62 --port 8000 35812-8-0-ebc5cd-all
-- python oe-xmlrpc-caller.py --host 172.17.1.62 --port 8000 35812-8-0-ebc5cd-all res.users read 1
baseUrl = "http://35812-8-0-ebc5cd.runbot.odoo.com:2154"

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route
  [ ("xmlrpc/common", xmlrpcHandler "/xmlrpc/common")
  , ("xmlrpc/object", xmlrpcFallthrough "/xmlrpc/object")
  ] <|> httpFallthrough

-- | Try to handle the XML-RPC call direclty, or forward it to the
-- openerp-server.
xmlrpcHandler :: String -> Snap ()
xmlrpcHandler to = do
  body <- LC.unpack <$> readRequestBody (1024 * 1024) -- TODO Is it enough ?
  mc <- liftIO $ handleError (return . Left . Fault 0) (Right <$> parseCall body)
  case mc of
    Left e -> writeLBS $ renderResponse e
    Right mc' -> case mc' of
      MethodCall "login" _ -> do
        bs <- rpc login mc'
        writeLBS $ renderResponse bs
      _ -> forward mc' to

-- | Forward the XML-RPC call to the openerp-server, without trying to handle
-- it directly.
xmlrpcFallthrough :: String -> Snap ()
xmlrpcFallthrough to = do
  body <- LC.unpack <$> readRequestBody (1024 * 1024) -- TODO Is it enough ?
  mc <- liftIO $ handleError (return . Left . Fault 0) (Right <$> parseCall body)
  case mc of
    Left e -> writeLBS $ renderResponse e
    Right mc' -> forward mc' to

forward c to = do
  response <- liftIO $ post (baseUrl ++ to) [] [] $ renderCall c
  -- writeLBS $ renderResponse $ parseResponse response
  writeBS $ rspBody response

rpc f = liftIO . handleError (return . Fault 0) . (fst . fun) f

httpFallthrough :: Snap ()
httpFallthrough =
  method Snap.POST postFallthrough <|>
  method Snap.GET getFallthrough

postFallthrough :: Snap ()
postFallthrough = do
  uri <- Snap.rqURI <$> Snap.getRequest
  cookies <- Snap.rqCookies <$> Snap.getRequest
  liftIO $ print cookies
  body <- LC.unpack <$> readRequestBody (1024 * 1024) -- TODO Is it enough ?
  let to = C.unpack uri
  response <- liftIO $ post (baseUrl ++ to) [] cookies $ LC.pack body
  addCookies response
  writeBS $ rspBody response

getFallthrough :: Snap ()
getFallthrough = do
  req <- Snap.getRequest
  uri <- Snap.rqURI <$> Snap.getRequest
  cookies <- Snap.rqCookies <$> Snap.getRequest
  liftIO $ print req
  liftIO $ print uri
  liftIO $ print cookies
  let to = C.unpack uri
  response <- liftIO $ get (baseUrl ++ to) [] cookies
  addCookies response
  writeBS $ rspBody response

addCookies response = do
  let cookies' = filter ((HdrSetCookie ==) . hdrName) $ getHeaders response
      mkCookie (Header HdrSetCookie c) = Cookie
        { cookieName = setCookieName c'
        , cookieValue = setCookieValue c'
        , cookieExpires = setCookieExpires c'
        , cookieDomain = setCookieDomain c'
        , cookiePath = setCookiePath c'
        , cookieHttpOnly = setCookieHttpOnly c'
        , cookieSecure = setCookieSecure c'
        }
        where c' = parseSetCookie $ C.pack c
  mapM (modifyResponse . addResponseCookie . mkCookie) cookies'

----------------------------------------------------------------------
-- XML-RPC methods handled directly by openerpfront.
----------------------------------------------------------------------

login :: String -> String -> String -> IO Int
login databaseName login password = return 1


----------------------------------------------------------------------
-- This is (modified) code from HaXR, not exported from Network.XmlRpc.Client.
----------------------------------------------------------------------

userAgent :: String
userAgent = "openerpfront/0.0.0 (github.com/noteed/openerpfront)"

handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

post :: String -> [Header] -> [Cookie] -> LC.ByteString -> IO (Response U.ByteString)
post url headers cookies content = do
    uri <- maybeFail ("Bad URI: '" ++ url ++ "'") (parseURI url)
    let a = uriAuthority uri
    auth <- maybeFail ("Bad URI authority: '" ++ show (fmap showAuth a) ++ "'") a
    post_ uri auth headers' content
  where showAuth (URIAuth u r p) = "URIAuth "++u++" "++r++" "++p
        headers' = Header HdrCookie (concat $ intersperse ";" $ map showCookie cookies) : headers

showCookie c = C.unpack $ C.concat [cookieName c, "=", cookieValue c]

post_ :: URI -> URIAuth -> [Header] -> LC.ByteString -> IO (Response U.ByteString)
post_ uri auth headers content = do
    eresp <- simpleHTTP (request uri auth headers (C.concat . LC.toChunks $ content))
    resp <- handleE (fail . show) eresp
    return resp

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

get :: String -> [Header] -> [Cookie] -> IO (Response U.ByteString)
get url headers cookies = do
    uri <- maybeFail ("Bad URI: '" ++ url ++ "'") (parseURI url)
    let a = uriAuthority uri
    auth <- maybeFail ("Bad URI authority: '" ++ show (fmap showAuth a) ++ "'") a
    get_ uri auth headers'
  where showAuth (URIAuth u r p) = "URIAuth "++u++" "++r++" "++p
        headers' = Header HdrCookie (concat $ intersperse ";" $ map showCookie cookies) : headers

get_ :: URI -> URIAuth -> [Header] -> IO (Response U.ByteString)
get_ uri auth headers = do
    eresp <- simpleHTTP (request' uri auth headers)
    resp <- handleE (fail . show) eresp
    return resp

request' :: URI -> URIAuth -> [Header] -> Request C.ByteString
request' uri auth usrHeaders = Request
  { rqURI = uri
  , rqMethod = GET
  , rqHeaders = headers
  , rqBody = ""
  }
    where
    -- the HTTP module adds a Host header based on the URI
    headers = [Header HdrUserAgent userAgent,
               Header HdrContentType "text/xml"
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
