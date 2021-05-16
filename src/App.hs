{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module App where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString, fromSqlKey)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server  as Servant
import           Model
import           ReadWrite(getUser, insertUser, doingMigration)
import           Api
import           Data.Aeson
import           GHC.Generics
import           Control.Monad.Logger
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import 		 Network.Wai.Application.Static
import           WaiAppStatic.Types (unsafeToPiece)
import           qualified Data.Time as Time
import           Servant.Auth.Server.Internal.ConfigTypes (SameSite (AnySite))
import 		 Servant.Server.StaticFiles
import           Data.ByteString(ByteString)
import           Data.FileEmbed (embedFile)
import           ClassyPrelude
import           Network.Wai
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Servant.Options

fetchUsersHandler :: Int64 -> Servant.Handler User
fetchUsersHandler userId = do
  maybeUser <- liftIO $ getUser userId
  case maybeUser of
    Just user ->  return user
    Nothing   ->  Servant.Handler (throwE $ err401 { errBody = "Could not find the user"})

insertUsersHandler :: User -> Servant.Handler Int64 
insertUsersHandler user = liftIO $ insertUser user

protected :: Servant.Auth.Server.AuthResult User -> Server ProtectedApi
protected (Servant.Auth.Server.Authenticated user) = fetchUsersHandler :<|> insertUsersHandler

checkCreds :: CookieSettings 
           -> JWTSettings 
	   -> Login
	   -> Servant.Handler 
	     (Headers 
	       '[ Header "Set-Cookie" SetCookie 
	        , Header "Set-Cookie" SetCookie ]
	      NoContent)
checkCreds 
  cookies 
  jwts 
  (Login loginUserIdent loginPasswordIdent) = do
  case mUser of
    Nothing -> Servant.Handler (throwE $ err401 { errBody = "auth failed"})
    Just usr -> do
      mApplyCookies <- liftIO $ acceptLogin cookies jwts usr
      case mApplyCookies of
        Nothing -> Servant.Handler (throwE $ err401 { errBody = "session set failed"})
	Just applyCookies -> do
	  return $ applyCookies NoContent 
  where 
    mUser = 
      if rawEmail loginUserIdent == "test@test.com" && rawPassword loginPasswordIdent == "test123"
        then Just $ User "test" "test@test.com" "" 38 "engineer"
	else Nothing 

staticFiles :: [(FilePath, ByteString)]
staticFiles =
  [ ("index.html", $(embedFile "static/index.html")),
    ("elm.js", $(embedFile "static/elm.js"))
  ]

unprotected :: CookieSettings -> JWTSettings -> Server UnprotectedApi 
unprotected cs jwts = 
  loginH :<|> staticH
  where 
    loginH = checkCreds cs jwts 
    staticH = serveDirectoryWith $ set
      where
        set = (defaultWebAppSettings $ error "unused") 
	  { ssLookupFile = ssLookupFile embedded
	  , ssIndices = map unsafeToPiece ["index.html", "elm.js"]
	  }
	embedded = embeddedSettings staticFiles
	

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cs jwts = protected :<|> unprotected cs jwts

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
        policy = simpleCorsResourcePolicy
          { 
              corsMethods = [ "GET", "POST", "PUT", "OPTIONS" ],
              corsOrigins = Just (["http://localhost:8000"], True),
              corsRequestHeaders = [ "authorization", "content-type" ]
          }

runServer :: IO ()
runServer = do
  myKey <- generateKey 
  now <- Time.getCurrentTime
  let jwtCfg = defaultJWTSettings myKey
      xsrfCookieSettings = defaultXsrfCookieSettings 
        { xsrfCookieName = "XSRF-TOKEN"
	, xsrfHeaderName = "X-XSRF-TOKEN"
        }

      cookieSettings = defaultCookieSettings 
        { cookieExpires = Just now { Time.utctDay = Time.addDays 30 (Time.utctDay now) }
	, cookieIsSecure = Servant.Auth.Server.NotSecure 
	, cookieSameSite = AnySite 
	, cookieXsrfSetting = Just xsrfCookieSettings 
	}
      cfg = cookieSettings :. jwtCfg :. EmptyContext 
      api = Proxy :: Proxy (API '[Cookie])
  runStderrLoggingT doingMigration
  run 8000 $ corsPolicy $ serveWithContext api cfg (server cookieSettings jwtCfg)


