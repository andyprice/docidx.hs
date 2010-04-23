module DocSources
where

import Control.Monad
import Data.Maybe
import Distribution.GhcPkgList (PkgInfo(..), readPkgName)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

-- | Doc sources can be global or local.  On conflict, prefer local.
data DocSrcKind = DocSrcGlobal
                | DocSrcUser
                  deriving (Eq, Ord, Show)

data DocSrc = DocSrc {
    docSrcPath :: FilePath
  , docSrcKind :: DocSrcKind
  } deriving (Eq, Ord, Show)

-- | List of doc sources; hard code for now, parameterise later.
sources :: [DocSrc]
sources = [DocSrc "/usr/local/share/doc" DocSrcGlobal
          ,DocSrc "/Users/gimbo/.cabal/share/doc" DocSrcUser]

-- | Given a path to a doc source, return all the paths corresponding
-- to Haskell packages.
pkgDirs :: FilePath -> IO [PkgInfo]
pkgDirs p = do
  cs <- getDirectoryContents p
  ds <- filterM (\x -> doesDirectoryExist $ p </> x) cs
  return $ map (\(n, v) -> PkgInfo n v False) $ mapMaybe readPkgName ds

test :: IO [[PkgInfo]]
test = mapM (pkgDirs . docSrcPath) sources