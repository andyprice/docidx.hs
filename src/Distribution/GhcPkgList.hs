-- | Get contents of installed packages by calling "ghc-pkg list" and
-- parsing the results.
--
-- Ideally we could re-use existing machinery to do this, but at time
-- of writing (April 2010) it doesn't seem to be exposed as part of
-- the Cabal package as one might expect (e.g. cabal-install uses its
-- own machinery).  It looks like a simple parse anyway, for our
-- purposes.

module Distribution.GhcPkgList (
  PkgDb(..),
  PkgInfo(..),
  listPackages,
  readPkgName
) where

import Control.Monad (when)
import Data.List (intercalate)
import Data.List.Utils (split)
import System.IO
import System.Process
import Text.ParserCombinators.Parsec

-- | A package database has a path and a list of packages.  We expect
-- there to be two: a global one, and a per-user one.
data PkgDb = PkgDb {
    dbPath :: FilePath,
    dbPkgs :: [PkgInfo]
  } deriving (Eq, Ord, Show)

-- | A package has a name and a version, and may be hidden.
data PkgInfo = PkgInfo {
    pkgName :: String,
    pkgVersion :: [Int],
    pkgHidden :: Bool
  } deriving (Eq, Ord)

instance Show PkgInfo where
  show (PkgInfo n v h) = if h then "(" ++ np ++ ")" else np
    where np = n ++ "-" ++ (intercalate "." $ map show v)

-- nb: Haven't bothered with a Read instance yet.

-- | Return a list of package database information as reported by
-- calling "ghc-pkg list".
listPackages :: IO [PkgDb]
listPackages = do
  ghcPkgOut <- runGhcPkg
  case parse pkgDbs "" ghcPkgOut of
    Left err -> error $ show err
    Right x -> return x

-- | Given a package name string, turn it into a name and version.
readPkgName :: String -> Maybe (String, [Int])
readPkgName s = case ds of
                  Just ds' -> Just (n, ds')
                  Nothing -> Nothing
  where n = intercalate "-" $ init parts
        ds :: Maybe [Int]
        ds = sequence $ map readInt $ split "." $ last parts
        parts = split "-" s

        

-- Helpers from here on.

-- | Run "ghc-pkg list" and capture its output.
runGhcPkg :: IO String
runGhcPkg = do
  let cp = (proc "ghc-pkg" ["list"]) {
             std_out = CreatePipe
           }
  (_, Just hOut, _, _) <- createProcess cp
  hGetContents hOut

-- | Parser for "ghc-pkg list"'s entire output.
pkgDbs :: Parser [PkgDb]
pkgDbs = pkgDb `sepEndBy1` (many1 $ char '\n')

-- | Parser for a single package database.
pkgDb :: Parser PkgDb
pkgDb = do
  path <- many1 (noneOf "\n")
  ps <- many1 $ preSpace >> pkgInfo
  return $ PkgDb path ps
    where preSpace = try $ (option ' ' (char '\n')) >> (many1 $ char ' ')

-- | Parser for a single package's info.
pkgInfo :: Parser PkgInfo
pkgInfo = do
  h <- option False (char '(' >> (return True))
  ps <- (many $ choice [letter, digit, char '.']) `sepBy1` (char '-')
  when h $ char ')' >> (return ())
  let pn = readPkgName $ intercalate "-" ps
  case pn of
    Just (n, ds) -> return $ PkgInfo n ds h
    Nothing -> error $ "Can't read package version: " ++ intercalate "-" ps

-- | Safe read of integer string
readInt :: String -> Maybe Int
readInt s = case (reads s) :: [(Int, String)] of
              [(x, "")] -> Just x
              _ -> Nothing
