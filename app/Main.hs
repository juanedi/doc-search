{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector
import GitHub (Name, Owner, Repo, Tree, executeRequest)
import qualified GitHub.Endpoints.GitData.Trees as Github


newtype Root = GithubRoot GithubRepo


data GithubRepo = GithubRepo (Name Owner) (Name Repo) (Name Tree)


data Doc
  = GithubDoc
      GithubRepo
      -- path
      Text


roots :: [Root]
roots =
  [GithubRoot (GithubRepo "juanedi" "dotfiles" "master")]


main :: IO ()
main = do
  docs <- concat <$> mapM (\(GithubRoot repo) -> docsInGithubRoot repo) roots
  mapM_ processDoc docs


processDoc :: Doc -> IO ()
processDoc (GithubDoc reop path) =
  -- TODO:
  --   - fetch contents
  --   - parse links
  --   - enqueue links
  --   - indexing
  --   - do all of the above but in a queue (so we do BFS instead of DFS)
  putStrLn (Text.unpack path)


docsInGithubRoot :: GithubRepo -> IO [Doc]
docsInGithubRoot repo = do
  files <- listFiles repo
  return (map (GithubDoc repo) files)


listFiles :: GithubRepo -> IO [Text]
listFiles (GithubRepo owner repo commit) = do
  let request = Github.nestedTreeR owner repo commit
  result <- executeRequest () request
  case result of
    (Left error) -> return []
    (Right tree) ->
      return (map Github.gitTreePath (Data.Vector.toList (Github.treeGitTrees tree)))
