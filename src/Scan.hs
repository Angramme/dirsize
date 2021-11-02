module Scan ( 
    scanpath,
    accesstree
) where

import Control.Concurrent.ParallelIO
import Helper

import System.Directory
import System.FilePath.Windows

-- import Control.Exception
import System.IO
import System.IO.Error

import Data.List
import Data.Maybe
import Tree

scanpath :: FilePath -> IO Tree
scanpath path = scanitem
    (takeDirectory path)
    (takeBaseName path)

scanitem :: FilePath -> FilePath -> IO Tree
scanitem parent item = 
    ifM (doesFileExist path) (File item <$> getFileSize path)
    $ ifM (doesDirectoryExist path) (scandir parent item)
    $ return $ File item 0
    where path = combine parent item

scandir :: FilePath -> FilePath -> IO Tree
scandir parent item = do
    let path = combine parent item
    filenames <- catchIOError (listDirectory path)
        (\error -> if isPermissionError error 
            then return [] else print error >> return [])
    mfiles <- parallelInterleaved $ map (scanitem path) filenames
    let totsize = (sum . map size) mfiles
    return $ seq mfiles $ Folder item totsize mfiles

accesstree :: Tree -> FilePath -> FilePath -> Tree
accesstree ptree ppath tpath = 
    let pnt = splitDirectories ppath
        (pnt2, rel) = splitAt (length pnt) $ splitDirectories tpath
        access tree (x:xs) = find ((==) x . name) (files tree) >>= \ntree -> access ntree xs
        access tree [] = Just tree
    in
        if pnt == pnt2
            then fromMaybe (error "couldn't find directory") $ access ptree rel
            else error "parent directory not matching!"


