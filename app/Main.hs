module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate, last)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding.Base64 (decodeBase64, decodeBase64Lenient)
import qualified Data.Vector
import qualified Database.Bloodhound as Bloodhound
import GitHub (Name, Owner, Repo, Tree, executeRequest)
import qualified GitHub.Auth as GHAuth
import GitHub.Data.Name (untagName)
import qualified GitHub.Endpoints.GitData.Trees as GHTrees
import qualified GitHub.Endpoints.Repos.Contents as GHContents
import qualified Index
import System.Environment (getArgs, getEnv)


data GithubRepo = GithubRepo (Name Owner) (Name Repo) (Name Tree)
  deriving (Show)


data Doc
  = GithubDoc
      GithubRepo
      GithubPath
  deriving (Show)


newtype GithubPath = GithubPath Text deriving (Show)


newtype DocContent = DocContent Text deriving (Show)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--index-github"] -> indexGithubRepos
    ["--index-paper"] -> indexPaperDocs
    _ ->
      putStrLn "Please specify a source to index"


repos :: [GithubRepo]
repos =
  [ GithubRepo "NoRedInk" "noredink-ui" "master"
  , GithubRepo "NoRedInk" "NoRedInk" "master"
  , GithubRepo "NoRedInk" "ghost-migrations" "trunk"
  , GithubRepo "NoRedInk" "wiki" "main"
  ]


indexGithubRepos :: IO ()
indexGithubRepos = do
  ghAuth <- GHAuth.OAuth . BS.pack <$> getEnv "GH_TOKEN"
  indexHandler <- Index.initialize
  docs <- concat <$> mapM (docsInGithubRoot ghAuth) repos
  mapM_ (processDoc ghAuth indexHandler) docs


processDoc :: GHAuth.Auth -> Index.Handler -> Doc -> IO ()
processDoc ghAuth indexHandler doc@(GithubDoc (GithubRepo owner repo commit) path) = do
  maybeContent <- fetchDocContents ghAuth doc
  case maybeContent of
    Nothing ->
      -- TODO: handle error
      return ()
    Just content ->
      Index.indexDoc
        indexHandler
        ( Index.Record
            { Index.name = docName doc
            , Index.source = "github"
            , Index.url = docToUrl doc
            , Index.contents = content
            }
        )


docName :: Doc -> Text
docName (GithubDoc (GithubRepo owner repo commit) (GithubPath path)) =
  case Text.splitOn "/" path of
    [] -> path
    segments ->
      last segments


docToUrl :: Doc -> Index.Url
docToUrl (GithubDoc (GithubRepo owner repo commit) (GithubPath path)) =
  Index.Url
    ( Text.concat
        [ "https://github.com/"
        , untagName owner
        , "/"
        , untagName repo
        , "/blob/"
        , untagName commit
        , "/"
        , path
        ]
    )


fetchDocContents :: GHAuth.Auth -> Doc -> IO (Maybe Text)
fetchDocContents ghAuth (GithubDoc (GithubRepo owner repo commit) (GithubPath path)) = do
  let request =
        GHContents.contentsForR
          owner
          repo
          path
          (Just (untagName commit))
  result <- executeRequest ghAuth request
  case result of
    Left error ->
      return Nothing
    Right (GHContents.ContentDirectory _) ->
      -- NOTE: not possible since we only query files
      return Nothing
    Right (GHContents.ContentFile fileData) -> do
      return (Just (decodeBase64Lenient (GHContents.contentFileContent fileData)))


docsInGithubRoot :: GHAuth.Auth -> GithubRepo -> IO [Doc]
docsInGithubRoot ghToken repo = do
  entries <- listFiles ghToken repo
  let markdownFiles = filter isMarkdownFile entries
  return
    ( map
        (GithubDoc repo . GithubPath . GHTrees.gitTreePath)
        markdownFiles
    )


isMarkdownFile :: GHTrees.GitTree -> Bool
isMarkdownFile entry =
  GHTrees.gitTreeType entry == "blob" && Text.isSuffixOf ".md" (GHTrees.gitTreePath entry)


listFiles :: GHAuth.Auth -> GithubRepo -> IO [GHTrees.GitTree]
listFiles ghAuth (GithubRepo owner repo commit) = do
  let request = GHTrees.nestedTreeR owner repo commit
  result <- executeRequest ghAuth request
  case result of
    Left error -> do
      -- TODO: handle error
      putStrLn ("error: " ++ show error)
      return []
    Right tree ->
      return (Data.Vector.toList (GHTrees.treeGitTrees tree))


indexPaperDocs :: IO ()
indexPaperDocs = return ()
