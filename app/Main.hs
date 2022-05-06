{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding.Base64 (decodeBase64, decodeBase64Lenient)
import qualified Data.Vector
import qualified Database.Bloodhound as Bloodhound
import GitHub (Name, Owner, Repo, Tree, executeRequest)
import qualified GitHub.Endpoints.GitData.Trees as GHTrees
import qualified GitHub.Endpoints.Repos.Contents as GHContents
import qualified Network.HTTP.Client as Http
import System.Environment


newtype Root = GithubRoot GithubRepo


data GithubRepo = GithubRepo (Name Owner) (Name Repo) (Name Tree)
  deriving (Show)


data Doc
  = GithubDoc
      GithubRepo
      Text -- path
  deriving (Show)


newtype DocContent = DocContent Text


roots :: [Root]
roots =
  [ GithubRoot (GithubRepo "juanedi" "dotfiles" "master")
  , GithubRoot (GithubRepo "NoRedInk" "noredink-ui" "master")
  ]


main :: IO ()
main = do
  initializeIndex
  docs <- concat <$> mapM (\(GithubRoot repo) -> docsInGithubRoot repo) roots
  mapM_ processDoc docs


initializeIndex :: IO ()
initializeIndex = do
  esHost <- Text.pack <$> System.Environment.getEnv "ES_HOST"
  esUser <- BS.pack <$> System.Environment.getEnv "ES_USER"
  esPassword <- BS.pack <$> System.Environment.getEnv "ES_PASSWORD"
  httpManager <- Http.newManager Http.defaultManagerSettings
  let server = Bloodhound.Server esHost
  let index = Bloodhound.IndexName "doc-search"
  let indexSettings = Bloodhound.IndexSettings (Bloodhound.ShardCount 1) (Bloodhound.ReplicaCount 0) Bloodhound.defaultIndexMappingsLimits
  let authHook request = return (Http.applyBasicAuth esUser esPassword request)
  let bhEnv = (Bloodhound.mkBHEnv server httpManager) {Bloodhound.bhRequestHook = authHook}
  Bloodhound.runBH bhEnv $ do
    reply <- Bloodhound.createIndex indexSettings index
    True <- Bloodhound.indexExists index
    -- TODO: explicitly create mapping
    return ()


processDoc :: Doc -> IO ()
processDoc doc@(GithubDoc (GithubRepo owner repo commit) path) = do
  maybeContent <- fetchDocContents doc
  case maybeContent of
    Nothing ->
      -- TODO: handle error
      return ()
    Just content ->
      indexDoc doc (DocContent content)


fetchDocContents :: Doc -> IO (Maybe Text)
fetchDocContents (GithubDoc (GithubRepo owner repo commit) path) = do
  let request =
        GHContents.contentsForR
          owner
          repo
          path
          Nothing -- TODO: specify commit
  result <- executeRequest () request
  case result of
    Left error ->
      return Nothing
    Right (GHContents.ContentDirectory _) ->
      -- NOTE: not possible since we only query files
      return Nothing
    Right (GHContents.ContentFile fileData) -> do
      return (Just (decodeBase64Lenient (GHContents.contentFileContent fileData)))


indexDoc :: Doc -> DocContent -> IO ()
indexDoc doc content =
  -- TODO
  return ()


docsInGithubRoot :: GithubRepo -> IO [Doc]
docsInGithubRoot repo = do
  entries <- listFiles repo
  let markdownFiles = filter isMarkdownFile entries
  return
    ( map
        (GithubDoc repo . GHTrees.gitTreePath)
        markdownFiles
    )


isMarkdownFile :: GHTrees.GitTree -> Bool
isMarkdownFile entry =
  GHTrees.gitTreeType entry == "blob" && Text.isSuffixOf ".md" (GHTrees.gitTreePath entry)


listFiles :: GithubRepo -> IO [GHTrees.GitTree]
listFiles (GithubRepo owner repo commit) = do
  let request = GHTrees.nestedTreeR owner repo commit
  result <- executeRequest () request
  case result of
    Left error ->
      -- TODO: handle error
      return []
    Right tree ->
      return (Data.Vector.toList (GHTrees.treeGitTrees tree))
