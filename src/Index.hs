{-# LANGUAGE DeriveGeneric #-}

module Index (initialize, indexDoc, Handler, Record (..), Url (..)) where

import Data.Aeson (ToJSON, genericToJSON)
import qualified Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.MD5 (md5)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.Bloodhound as Bloodhound
import Debug.Trace
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as Http
import System.Environment (getEnv)


newtype Handler = Handler {bhEnv :: Bloodhound.BHEnv}


newtype Url = Url Text deriving (Generic)


instance ToJSON Url where
  toJSON = genericToJSON Data.Aeson.defaultOptions


data Record = Record
  { url :: Url
  , contents :: Text
  }
  deriving (Generic)


instance ToJSON Record where
  toJSON = genericToJSON Data.Aeson.defaultOptions


indexName :: Bloodhound.IndexName
indexName = Bloodhound.IndexName "doc-search"


initialize :: IO Handler
initialize = do
  esHost <- Text.pack <$> getEnv "ES_HOST"
  esUser <- BS.pack <$> getEnv "ES_USER"
  esPassword <- BS.pack <$> getEnv "ES_PASSWORD"
  httpManager <- Http.newManager Http.defaultManagerSettings
  let server = Bloodhound.Server esHost
  let indexSettings = Bloodhound.IndexSettings (Bloodhound.ShardCount 1) (Bloodhound.ReplicaCount 0) Bloodhound.defaultIndexMappingsLimits
  let authHook request = return (Http.applyBasicAuth esUser esPassword request)
  let bhEnv = (Bloodhound.mkBHEnv server httpManager) {Bloodhound.bhRequestHook = authHook}

  putStrLn ("Initializing ElasticSearch index " ++ show indexName)

  Bloodhound.runBH bhEnv $ do
    reply <- Bloodhound.createIndex indexSettings indexName
    True <- Bloodhound.indexExists indexName
    -- TODO: explicitly create mapping
    return ()

  return (Handler bhEnv)


indexDoc :: Handler -> Record -> IO ()
indexDoc (Handler bhEnv) record = do
  let (Url url_) = url record
  let id@(Bloodhound.DocId md5_) = recordId record

  putStrLn ("Indexing " ++ Text.unpack url_ ++ " (" ++ Text.unpack md5_ ++ ")")

  _ <-
    Bloodhound.runBH bhEnv $
      Bloodhound.indexDocument
        indexName
        Bloodhound.defaultIndexDocumentSettings
        record
        id

  return ()


recordId :: Record -> Bloodhound.DocId
recordId record =
  let (Url url_) = url record
   in Bloodhound.DocId (Text.pack (show (md5 (BSL.fromStrict (encodeUtf8 url_)))))
