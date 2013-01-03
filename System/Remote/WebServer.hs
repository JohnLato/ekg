{-# LANGUAGE OverloadedStrings #-}

module System.Remote.WebServer
    ( startServer
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Char (chr)
import qualified Data.HashMap.Strict as M
import Data.IORef (IORef)
import qualified Data.Text as T
import Paths_ekg (getDataDir)
import Prelude hiding (read)
import Network.Web.HTTP
import Network.Web.Server.Basic
import Network.Web.URI
import System.FilePath ((</>))

import System.Remote.Common

------------------------------------------------------------------------

startServer :: IORef Counters -> IORef Gauges -> IORef Labels
            -> S.ByteString  -- ^ Host to listen on (e.g. \"localhost\")
            -> Int           -- ^ Port to listen on (e.g. 8000)
            -> IO ()
startServer counters gauges labels host port = do
    dataDir <- liftIO getDataDir
    serveHTTP Nothing port host (routes dataDir counters gauges labels)

------------------------------------------------------------------------

routes :: FilePath -> IORef Counters -> IORef Gauges -> IORef Labels
    -> Request -> Path
routes dataDir counters gauges labels req =
    maybe (File (ekgAsset . ix $ path))
    Handler (getPath counters gauges labels req)
    where
        path = S8.unpack (uriPath (reqURI req))
        ix "/" = "/index.html"
        ix x   = x
        ekgAsset x = dataDir </> "assets" ++ x

------------------------------------------------------------------------

ctJSON :: CT
ctJSON = "application/json"

ctText :: CT
ctText = "text/plain"

getPath :: IORef Counters -> IORef Gauges -> IORef Labels
    -> Request -> Maybe (IO Response)
getPath counters gauges labels req
    | j && S.null p        = Just (getAll counters gauges labels)
    | prefix == "combined" = Just (getCombined counters gauges labels mname)
    | prefix == "counters" = Just (getRef counters mname)
    | prefix == "gauges"   = Just (getRef gauges mname)
    | prefix == "labels"   = Just (getRef labels mname)
    | otherwise            = Nothing
    where
        j = acceptCT ctJSON req
        p = S.drop 1 $ uriPath (reqURI req)
        (prefix, mname) = splitAPI (toText p)
        toText = T.pack . map (chr . fromIntegral) . S.unpack

acceptCT :: Comm a => CT -> a -> Bool
acceptCT ct = maybe False (S.isInfixOf ct) . lookupField FkAccept

splitAPI :: T.Text -> (T.Text, Maybe T.Text)
splitAPI path = f components
    where
        f []              = ("", Nothing)
        f [prefix]        = (prefix, Nothing)
        f (prefix:name:_) = (prefix, Just name)
        components = T.split (=='/') path

contentResponse :: CT -> L.ByteString -> Response
contentResponse ct bs = makeResponse2 OK (Just bs) (Just len) [(FkContentType, ct)]
    where
        len = fromIntegral $ L.length bs

notFoundResponse :: Response
notFoundResponse = makeResponse NotFound []

getAll :: IORef Counters -> IORef Gauges -> IORef Labels -> IO Response
getAll counters gauges labels =
    contentResponse ctJSON <$> buildAll counters gauges labels

getCombined :: IORef Counters -> IORef Gauges -> IORef Labels
    -> Maybe T.Text -> IO Response
getCombined _        _      _      (Just _) = return notFoundResponse
getCombined counters gauges labels Nothing =
    contentResponse ctJSON <$> buildCombined counters gauges labels

getRef :: (Ref r t, Show t, ToJSON t) => IORef (M.HashMap T.Text r)
    -> Maybe T.Text -> IO Response
getRef refs Nothing = contentResponse ctJSON <$> buildMany refs
getRef refs (Just name) = do
    mbs <- buildOne refs name
    case mbs of
        Just bs -> return $ contentResponse ctText (L.fromChunks [bs])
        Nothing -> return notFoundResponse
