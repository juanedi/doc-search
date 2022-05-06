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
processDoc doc =
  -- TODO:
  --   - fetch contents
  --   - parse links
  --   - enqueue links
  --   - indexing
  --   - do all of the above but in a queue (so we do BFS instead of DFS)
  print doc


docsInGithubRoot :: GithubRepo -> IO [Doc]
docsInGithubRoot repo = do
  entries <- listFiles repo
  let markdownFiles = filter isMarkdownFile entries
  return
    ( map
        (GithubDoc repo . Github.gitTreePath)
        markdownFiles
    )


isMarkdownFile :: Github.GitTree -> Bool
isMarkdownFile entry =
  Github.gitTreeType entry == "blob" && Text.isSuffixOf ".md" (Github.gitTreePath entry)


listFiles :: GithubRepo -> IO [Github.GitTree]
listFiles (GithubRepo owner repo commit) = do
  let request = Github.nestedTreeR owner repo commit
  result <- executeRequest () request
  case result of
    (Left error) -> return []
    (Right tree) -> return (Data.Vector.toList (Github.treeGitTrees tree))
