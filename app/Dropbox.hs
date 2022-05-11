module Dropbox (
  Handler,
  DocId (..),
  Metadata (..),
  initialize,
  docUrl,
  listDocs,
  fetchDocMetadata,
  fetchDocContent,
) where

import Data.Aeson (FromJSON, ToJSON, object, withObject, withText, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString, putStrLn)
import qualified Data.ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (getEnv)


newtype Handler = Handler {token :: Text}


newtype DocId = DocId Text


instance FromJSON DocId where
  parseJSON =
    withText "doc_id" (return . DocId)


newtype Metadata = Metadata
  { title :: Text
  }


instance FromJSON Metadata where
  parseJSON =
    withObject "metadata" $ \o ->
      Metadata <$> (o .: "title")


initialize :: IO Handler
initialize = do
  token <- getEnv "DROPBOX_TOKEN"
  return (Handler {token = Text.pack token})


data FilterBy = DocsCreated | DocsAccessed


instance ToJSON FilterBy where
  toJSON DocsCreated = Aeson.toJSON ("docs_created" :: Text)
  toJSON DocsAccessed = Aeson.toJSON ("docs_accessed" :: Text)


data SortBy = Accessed | Modified | Created


instance ToJSON SortBy where
  toJSON Accessed = Aeson.toJSON ("accessed" :: Text)
  toJSON Modified = Aeson.toJSON ("modified" :: Text)
  toJSON Created = Aeson.toJSON ("creatd" :: Text)


data SortOrder = Ascending | Descending


instance ToJSON SortOrder where
  toJSON Ascending = Aeson.toJSON ("ascending" :: Text)
  toJSON Descending = Aeson.toJSON ("descending" :: Text)


data ListArgs = ListArgs FilterBy SortBy SortOrder Int


instance ToJSON ListArgs where
  toJSON (ListArgs filterBy sortBy sortOrder limit) =
    object
      [ "filter_by" .= filterBy
      , "sort_by" .= sortBy
      , "sort_order" .= sortOrder
      , "limit" .= limit
      ]


newtype ListResponse
  = -- TODO: parse "cursor" and "has_more" as well
    ListResponse [DocId]


instance FromJSON ListResponse where
  parseJSON =
    withObject "list_response" $ \o ->
      ListResponse <$> (o .: "doc_ids")


listDocs :: Handler -> Int -> IO (Maybe [DocId])
listDocs handler limit = do
  response <- apiCall handler "list" (ListArgs DocsAccessed Accessed Descending limit)
  case Aeson.decode response of
    Just (ListResponse docIds) ->
      return (Just docIds)
    Nothing ->
      return Nothing


docUrl :: DocId -> Text
docUrl (DocId docId) =
  Text.concat ["https://paper.dropbox.com/doc/", docId]


newtype MetadataArgs = MetadataArgs DocId


instance ToJSON MetadataArgs where
  toJSON (MetadataArgs (DocId docId)) =
    object ["doc_id" .= docId]


fetchDocMetadata :: Handler -> DocId -> IO (Maybe Metadata)
fetchDocMetadata handler docId = do
  response <- apiCall handler "get_metadata" (MetadataArgs docId)
  return (Aeson.decode response)


data ExportFormat = Html | Markdown


instance ToJSON ExportFormat where
  toJSON Html = Aeson.toJSON ("html" :: Text)
  toJSON Markdown = Aeson.toJSON ("markdown" :: Text)


data DownloadArgs = DownloadArgs DocId ExportFormat


instance ToJSON DownloadArgs where
  toJSON (DownloadArgs (DocId docId) exportFormat) =
    object
      [ "doc_id" .= docId
      , "export_format" .= exportFormat
      ]


fetchDocContent :: Handler -> DocId -> IO (Maybe Text)
fetchDocContent handler docId = do
  response <- apiCall_ handler "download" (DownloadArgs docId Markdown)
  return (Just (toStrict (decodeUtf8 response)))


apiCall :: (ToJSON args) => Handler -> Text -> args -> IO ByteString
apiCall (Handler token) method args = do
  -- FIXME: check response status code
  let url = Text.append "https://api.dropboxapi.com/2/paper/docs/" method
  -- TODO: cache the manager
  manager <- newTlsManager
  initialRequest <- Http.parseRequest (Text.unpack url)
  let request =
        Http.applyBearerAuth (encodeUtf8 token) $
          initialRequest
            { Http.method = "POST"
            , Http.requestBody = Http.RequestBodyLBS $ Aeson.encode args
            , Http.requestHeaders = ("Content-Type", "application/json") : Http.requestHeaders initialRequest
            }
  response <- Http.httpLbs request manager
  return (Http.responseBody response)


apiCall_ :: (ToJSON args) => Handler -> Text -> args -> IO ByteString
apiCall_ (Handler token) method args = do
  -- FIXME: check response status code
  let url = Text.append "https://api.dropboxapi.com/2/paper/docs/" method
  -- TODO: cache the manager
  manager <- newTlsManager
  initialRequest <- Http.parseRequest (Text.unpack url)
  let request =
        Http.applyBearerAuth (encodeUtf8 token) $
          initialRequest
            { Http.method = "POST"
            , Http.requestHeaders = ("Dropbox-API-Arg", Data.ByteString.Lazy.toStrict (Aeson.encode args)) : Http.requestHeaders initialRequest
            }
  response <- Http.httpLbs request manager
  return (Http.responseBody response)
