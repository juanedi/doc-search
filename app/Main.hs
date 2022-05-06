{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding.Base64 (decodeBase64, decodeBase64Lenient)
import qualified Data.Vector
import GitHub (Name, Owner, Repo, Tree, executeRequest)
import qualified GitHub.Endpoints.GitData.Trees as GHTrees
import qualified GitHub.Endpoints.Repos.Contents as GHContents


newtype Root = GithubRoot GithubRepo


data GithubRepo = GithubRepo (Name Owner) (Name Repo) (Name Tree)
  deriving (Show)


data Doc
  = GithubDoc
      GithubRepo
      Text -- path
  deriving (Show)


roots :: [Root]
roots =
  [ GithubRoot (GithubRepo "juanedi" "dotfiles" "master")
  , GithubRoot (GithubRepo "NoRedInk" "noredink-ui" "master")
  ]


main :: IO ()
main = do
  docs <- concat <$> mapM (\(GithubRoot repo) -> docsInGithubRoot repo) roots
  mapM_ processDoc docs


processDoc :: Doc -> IO ()
processDoc doc@(GithubDoc (GithubRepo owner repo commit) path) = do
  -- TODO:
  --   - parse links
  --   - enqueue links
  --   - indexing
  --   - do all of the above but in a queue (so we do BFS instead of DFS)
  let request =
        GHContents.contentsForR
          owner
          repo
          path
          Nothing -- todo: specify commit
  result <- executeRequest () request
  case result of
    Left error ->
      -- TODO: handle error
      return ()
    Right (GHContents.ContentDirectory _) ->
      -- NOTE: not possible since we only query files
      return ()
    Right (GHContents.ContentFile fileData) -> do
      putStrLn ""
      putStrLn ""
      putStrLn ""
      putStrLn "============================================================================================================"
      print doc
      putStrLn "============================================================================================================"
      putStrLn ""
      print (decodeBase64Lenient (GHContents.contentFileContent fileData))


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
