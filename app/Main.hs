 module Main where
import Data.List
import System.IO
import System.Directory
import Data.Monoid
import Sound.HTagLib
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import System.FilePath.Posix
import System.Environment (getArgs)
import Data.List.Split
import Lib

{-TODO:
-Add locate function for locating filepath of music, searches for args-}

{-main
Some notes for future improvements over this program:
    -This entire thing is currently using the string class. This DESPERATELY needs to be written using
      Data.Text, which has already been imported as seen above but not used. Would be much faster. 
    -Currently the argument that calls the function type is very unsafe. Needs to be reworked entirely.
    -Rework the tag system so that it follows Artist - Album - Year
    -Rewrite the tag system so that it can filter based off of Artist, Album, Year, or any combination of the three
    -Change how the boolean filter operators work so that the program can accept any number of filters with different boolean arguments. -}

main :: IO()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    (_:command:_) <- getArgs
    args <- getArgs
    --Find input directory, output directory, specific function call, and function parameters from argumments
    readfrom <- firstest getArgs
    writeto  <- lastest getArgs
    tagcall  <- takearg $ safetail $ tail $ init args
    let (Just call) = lookup command funclookup --Unsafe!
    call writeto tagcall readfrom



{-help ignores all parameters passed to it (for now) and just prints a list of instructions to the user. -}

help :: [Char] -> [String] -> FilePath -> IO ()
help xs ys zs = do
    putStrLn "Argument Formatting:"
    putStrLn ""
    putStrLn "'input' 'function' 'arguments' 'arguments' 'arguments...' 'output'"
    putStrLn "Your last argument will always be considered your output. If you would like your output to be the input location, simply type ''"
    putStrLn "If the argument is a literal string, it will be considered as a tag in the form Album - Artist. It is very specific."
    putStrLn "If the argument is a directory, it will read all metadata within that directory and it's tags will be added to args."
    putStrLn "If the argument is a file directory, it will read the file line by line and it's string will be added to args."
    putStrLn ""
    putStrLn "Available functions:"
    putStrLn ""
    putStrLn "-b will build a text file of id3 metadata at location output by searching at location input. This search is recursive."
    putStrLn "-m will move any music file found recursively at input to a destination file given output. All files are sorted by output/Artist/Album."
    putStrLn "Arguments will be taken as a 'whitelist' for all searched files."
    putStrLn "-h will call this help file for information."



{-Function for the -b call, simply calls buildtags and writes it to a file
    after doing some basic parsing and sorting. -}

backlog :: [Char] -> [String] -> FilePath -> IO ()
backlog output arg input = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    putStrLn "Searching..."
    tags <- buildtags tagsort input
    let ftags = if arg == [] then tags else intersect tags arg
    appendFile (output ++ "/backlog") ("\n" ++ lined ftags)

locate :: p -> [[Char]] -> FilePath -> IO ()
locate output arg input = do
    search <- buildtags tagloc input
    let ftags = if arg == [] then search else filter (\x -> elem (head $ splitOn " has been located at " x) arg) search
    mapM_ putStrLn ftags



{-Two functions for -m, one to call and search folders recursively
    and another to use in a MapM_ to move through the directory list
    and call the IO() Function to move the files and sort them.
    Albums are sorted by Artist/Album/Filename. -}

move :: String -> [String] -> FilePath -> IO ()
move output arg input = do
    let folderl b = ((&&) <$> (searchable <$> (getPermissions b))) <*> (readable <$> (getPermissions b))
    let filel   c = ((lastlookup mlookup c) &&) <$> (not <$> (searchable <$> (getPermissions c)))
    a        <- listDirectory input
    listdirs <- filterM (folderl) (map ((input ++ "/") ++) a)
    filedirs <- filterM (filel)   (map ((input ++ "/") ++) a)
    filtfile <- filterM (\x -> ((`elem` arg) <$> (tagsort x))) filedirs
    putStrLn (concat filtfile)
    if arg == [] then 
        mapM_ (tagmove output) filedirs 
    else mapM_ (tagmove output) filtfile
    if listdirs == [] then
        return()
    else mapM_ (move output arg) listdirs

tagmove :: [Char] -> FilePath -> IO ()
tagmove ys xs = do
    nsartist <- getTags xs artistGetter
    nsalbum  <- getTags xs albumGetter
    let name = takeFileName xs
    let artist = init $ drop 8 $ show nsartist
    let album  = init $ drop 7 $ show nsalbum
    createDirectoryIfMissing True (ys ++ "/" ++ artist ++ "/" ++ album)
    copyFileWithMetadata xs (ys ++ "/" ++ artist ++ "/" ++ album ++ "//" ++ name)


{-Functions for calling tags to be recieved from all files within a directory.
    This is an inherently recursive process; all subfolders will be searched.
    ALL. -}

buildtags :: Eq a => ([Char] -> IO a) -> FilePath -> IO [a]
buildtags func xs = do
    let folderl b = ((&&) <$> (searchable <$> (getPermissions b))) <*> (readable <$> (getPermissions b))
    let filel   c = ((lastlookup mlookup c) &&) <$> (not <$> (searchable <$> (getPermissions c)))
    a        <- listDirectory xs
    listdirs <- filterM (folderl) (map ((xs ++ "/") ++) a)
    filedirs <- filterM (filel)   (map ((xs ++ "/") ++) a)
    tagfiles <- mapM func filedirs
    taglists <- mapM (buildtags func) listdirs
    if listdirs == [] then
        return (nub tagfiles)
    else return (concat ((nub tagfiles):taglists))




tagsort xs = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    nsartist <- getTags xs artistGetter
    nsalbum  <- getTags xs albumGetter
    let artist = init $ drop 8 $ show nsartist
    let album  = init $ drop 7 $ show nsalbum
    pure (album ++ " - " ++ artist)

tagloc :: Control.Monad.IO.Class.MonadIO m => FilePath -> m [Char]
tagloc xs = do
    nsartist <- getTags xs artistGetter
    nsalbum  <- getTags xs albumGetter
    let direct = takeDirectory xs
    let artist = init $ drop 8 $ show nsartist
    let album  = init $ drop 7 $ show nsalbum
    return (album ++ " - " ++ artist ++ " has been located at " ++ direct)


{-Functions for finding all needed strings for the program to call the proper main functions.
    firsttest, lastest, and argument are required. -}

firstest :: IO [FilePath] -> IO FilePath
firstest xs = do
    a <- xs
    if (a == []) then do
        putStrLn "No reading directory specified. Will read from current directory."
        getCurrentDirectory
    else do
        t <- doesDirectoryExist (head a)
        if not t then do
            putStrLn "Improper reading directory specified. Will read from current directory."
            getCurrentDirectory
        else return (head a)

lastest :: IO [FilePath] -> IO FilePath
lastest xs = do
    a <- xs
    if ((tail $ tail a) == []) then do
        putStrLn "No writing directory specified. Will write to input directory."
        firstest xs
    else do
        t <- doesDirectoryExist (last a)
        if not t then do
            putStrLn "Improper writing directory specified. Will write to input directory."
            firstest xs
        else return (last a)



takearg :: [String] -> IO [String]
takearg []     = return []
takearg (x:xs) = do
    args <- findarg xs
    if elem x (map fst boollookup) then do
        let (Just org) = lookup x boollookup
        return $ org args
    else return $ nub args

findarg :: [String] -> IO [String]
findarg []     = return []
findarg (x:xs) = do
    t <- ((&& x /= []) <$> doesPathExist x) 
    if t then do
        let folderl b = ((&&) <$> (searchable <$> (getPermissions b))) <*> (readable <$> (getPermissions b))
        t2 <- folderl x
        list <- if t2 then (buildtags tagsort) x else fmap lines (readFile x)
        (list ++) <$> (findarg xs)
    else (x:) <$> (findarg xs)



{-Functions for look (k,v) function values, passed from getArgs to call
    Will perhaps oneday include support for writing tags to files-}

funclookup :: [([Char],[Char] -> [String] -> FilePath -> IO ())]
funclookup = [("-b",backlog), ("-m",move), ("-h", help),("-l",locate)]

boollookup :: [([Char], [String] -> [String])]
boollookup = [("-o", nub),("-a",oand),("-na",nand)]

mlookup :: [String]
mlookup = [".mp4", ".flac", ".mp3", ".wav"]

lastsearch :: Eq a => [a] -> [a] -> Bool
lastsearch s (x:xs) 
    |(length s) == (length (x:xs)) = (s == (x:xs))
    |otherwise                     = lastsearch s xs

lastlookup :: Eq a => [[a]] -> [a] -> Bool
lastlookup (y:ys) xs
    |lastsearch y xs = True
    |(ys) == []      = False
    |otherwise       = lastlookup ys xs



{-Some basic list manipulation functions to sort large lists 
    and remove duplicates, which there are a lot of.
    Unlike the previous generation of the backlogcreator, nub is
    called during the buildtags recursive process, making it a bit
    more efficient. lined is still called only before writing to maintain
    alphabetical order. -}

lined :: [[Char]] -> [Char]
lined xs = concat $ intersperse "\n" $ quicksort xs letterfilter

quicksort :: Ord a1 => [a2] -> (a2 -> a1) -> [a2]
quicksort [] _ = []  
quicksort (x:xs) f =   
    let smallerSorted = quicksort [a | a <- xs, (f a) > (f x)] f
        biggerSorted  = quicksort [a | a <- xs, (f a) <= (f x)] f
    in  biggerSorted ++ [x] ++ smallerSorted
    
letterfilter :: [Char] -> [Char]
letterfilter [] = []
letterfilter (x:xs)
    |elem x ['a'..'z'] = x:(letterfilter xs)
    |elem x ['A'..'Z'] = (maybe ' ' id (lookup x upperlist)):(letterfilter xs)
    |otherwise = letterfilter xs
        where upperlist = zip ['A'..'Z'] ['a'..'z']

safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

oand :: Eq a => [a] -> [a]
oand [] = []
oand (x:xs)
    |elem x xs = x:(oand xs)
    |otherwise = oand xs

nand :: Eq a => [a] -> [a]
nand [] = []
nand (x:xs)
    |elem x xs = nand $ filter (\b -> b /= x) (x:xs)
    |otherwise = x:(nand xs)

--replaced with nub
{-duprem :: Eq a => [a] -> [a]
duprem []     = []
duprem (x:xs) = reverse (go (x:xs) [])
    where
        go [] acc  = acc
        go (x:xs) acc
            |x `elem` acc = go xs acc
            |otherwise    = go xs (x:acc)-}
