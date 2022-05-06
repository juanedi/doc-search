module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding.Base64 (decodeBase64, decodeBase64Lenient)
import qualified Data.Vector
import qualified Database.Bloodhound as Bloodhound
import GitHub (Name, Owner, Repo, Tree, executeRequest)
import GitHub.Data.Name (untagName)
import qualified GitHub.Endpoints.GitData.Trees as GHTrees
import qualified GitHub.Endpoints.Repos.Contents as GHContents
import qualified Index
import System.Environment


newtype Root = GithubRoot GithubRepo


data GithubRepo = GithubRepo (Name Owner) (Name Repo) (Name Tree)
  deriving (Show)


data Doc
  = GithubDoc
      GithubRepo
      GithubPath
  deriving (Show)


newtype GithubPath = GithubPath Text deriving (Show)


newtype DocContent = DocContent Text deriving (Show)


roots :: [Root]
roots =
  [ GithubRoot (GithubRepo "juanedi" "dotfiles" "master")
  , GithubRoot (GithubRepo "NoRedInk" "noredink-ui" "master")
  ]


main :: IO ()
main = do
  indexHandler <- Index.initialize
  docs <- concat <$> mapM (\(GithubRoot repo) -> docsInGithubRoot repo) roots
  mapM_ (processDoc indexHandler) docs


processDoc :: Index.Handler -> Doc -> IO ()
processDoc indexHandler doc@(GithubDoc (GithubRepo owner repo commit) path) = do
  maybeContent <- fetchDocContents doc
  case maybeContent of
    Nothing ->
      -- TODO: handle error
      return ()
    Just content ->
      Index.indexDoc
        indexHandler
        ( Index.Record
            { Index.url = docToUrl doc
            , Index.contents = content
            }
        )


docToUrl :: Doc -> Index.Url
docToUrl (GithubDoc (GithubRepo owner repo commit) (GithubPath path)) =
  Index.Url
    ( Text.concat
        [ "http://github.com/"
        , untagName owner
        , "/"
        , untagName repo
        , "/blob/"
        , untagName commit
        , "/"
        , path
        ]
    )


fetchDocContents :: Doc -> IO (Maybe Text)
fetchDocContents (GithubDoc (GithubRepo owner repo commit) (GithubPath path)) = do
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


docsInGithubRoot :: GithubRepo -> IO [Doc]
docsInGithubRoot repo = do
  entries <- listFiles repo
  let markdownFiles = filter isMarkdownFile entries
  return
    ( map
        (GithubDoc repo . GithubPath . GHTrees.gitTreePath)
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
