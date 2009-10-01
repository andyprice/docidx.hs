{-
 - An ugly Haskell port of a "horribly hacked together" Python script to
 - generate an HTML index page for Haddock-generated Haskell package docs. It
 - also works quite nicely as a general index for other docs. Note that the
 - docs directory is hard-coded, below: see the comments there.
 -
 - Requires tagsoup and other non-prelude libraries (see the list of imports).
 -
 - Ported from the Python code located at yonder blog:
 -   http://gimbo.org.uk/blog/2009/09/23/
 -}

import List
import Directory
import Data.Char
import System.FilePath
import qualified Data.Set as Set
import Text.HTML.TagSoup
import Text.Html

{-
 - This is the path to the docs.  Any subdirectory which contains an
 - 'html/index.html' will get an entry in the index (all the other are
 - listed separately at the end).
 -
 - The index is written to 'index.html' at this path.
 -}

docPath = "/usr/share/doc" :: FilePath

data Package = Package { key :: Char
                       , pname :: String
                       , ptitle :: String
                       , path :: FilePath }

-- Generates the full path of a HTML doc index file
htmlPath :: FilePath -> FilePath
htmlPath lib = joinPath [docPath, lib, "html", "index.html"]

-- Recurses over a list of HTML tags to find the text in the title tag
findTitleTag :: [Tag] -> String
findTitleTag []     = ""
findTitleTag (t:ts) = if isTagOpenName "title" t
                  then fromTagText $ head ts
                  else findTitleTag ts

-- Parses a HTML document to find its title tag
packageTitle :: FilePath -> IO String
packageTitle lib = do
    s <- readFile $ joinPath [docPath, htmlPath lib]
    return (findTitleTag $ canonicalizeTags $ parseTags s)

-- Creates a package object
createPackage :: FilePath -> String -> Package
createPackage lib title = Package (toUpper (head lib)) lib title (htmlPath lib)

-- Creates a doc package and categorises it based on whether it has a HTML
-- index in the doc directory. If it has one, it is keyed by its initial
-- letter; if it doesn't, it gets a key of '\0'
categorise :: FilePath -> IO Package
categorise lib = do
    x <- doesFileExist $ joinPath [docPath, (htmlPath lib)]
    if x then do title <- packageTitle lib
                 return (createPackage lib title)
         else return (Package '\0' lib "" (joinPath [docPath, lib]))

-- Finds all the doc packages inside a directory
libs :: FilePath -> IO [Package]
libs path = do
    dirs <- getDirectoryContents path
    mapM categorise $ sort dirs

-- Returns a function which generates a list of initial-letter keys for a list
-- of packages
keys :: [Package] -> [Char]
keys = Set.toAscList . foldl (\u p->Set.insert (key p) u) Set.empty

-- Creates the HTML code for this list of packages
htmlPage :: [Package] -> [Html]
htmlPage pkgs = [ htmlHeader, htmlBody pkgs ]

-- Creates the HTML header
htmlHeader :: Html
htmlHeader =
    header << [ thetitle << "My Haskell Home Page"
              , thelink ![ rel "stylesheet", thetype "text/css"
                 , href "http://hackage.haskell.org/packages/hackage.css"] << noHtml]

-- Generates the HTML body of the index for a list of packages
htmlBody :: [Package] -> Html
htmlBody pkgs =
    body << ([ h2 << "Local packages with docs"
             , p ![ theclass "toc" ] << htmlTOC pkgs ]
            ++ letterListings pkgs (tail $ keys pkgs)
            ++ letterListings pkgs "\0")

-- Generates the table of contents for the index
htmlTOC :: [Package] -> [Html]
htmlTOC pkgs = intersperse bullet anchors
    where anchors = [ anchor ![ href ('#':[k]) ] << [k] |
                            k <- tail $ keys pkgs ]
          bullet = primHtml " &bull; "

-- Generates the initial-letter-grouped sections of the index
letterListings :: [Package] -> [Char] -> [Html]
letterListings _    []       = [noHtml]
letterListings pkgs ['\0']   =
    [ h2 << "Directories without HTML docs"
    , ulist << (map packageItem $ filter (('\0'==) . key) pkgs)
    ]
letterListings pkgs (k:keys) =
    [ h3 ![theclass "category"] <<  anchor ![name [k]] << [k]
    , ulist ![theclass "packages"]
        << (map packageItem $ filter ((k==) . key) pkgs)
    ] ++ letterListings pkgs keys

-- Generates a link to a package's index.html (if it exists) as a <li>
-- Here the key '\0' is used for the packages with no HTML index
-- so we just link to their directories.
packageItem :: Package -> Html
packageItem pkg =
    li << [ anchor ![href $ "file://" ++ path pkg]
                << stringToHtml (pname pkg),
            stringToHtml (mktitle pkg)]
    where mktitle p = if ptitle p == "" then ""
                            else ": " ++ ptitle p

main :: IO ()
main = do
    p <- libs docPath
    writeFile outFile $ show $ htmlPage p
    where outFile = joinPath [docPath, "index.html"]
