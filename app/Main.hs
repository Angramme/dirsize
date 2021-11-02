module Main where

import Control.Concurrent.ParallelIO
import Control.Monad
import Helper

import System.Directory
import System.FilePath.Windows

import System.IO
import GHC.IO.Encoding
import Data.Colour.SRGB (sRGB24)
import System.Console.ANSI      

import Scan
import Print
import Tree


main :: IO ()
main = do
    setLocaleEncoding utf8
    setSGR [SetColor Background Vivid Blue]
    putStrLn "dirsize v1.0, (C) Kacper Ozieblowski"
    setSGR [Reset]

    cwd <- getCurrentDirectory
    tree <- scanpath cwd
    seq tree $ go cwd (accesstree tree cwd)

    stopGlobalPool -- ParallelIO stuff

go :: FilePath -> (FilePath -> Tree) -> IO ()
go parentpath access = do
    cwd <- getCurrentDirectory
    let tree = access cwd
    putStrLn ("\nsnapshot: " ++ parentpath)
    putStrLn ("cwd: " ++ cwd)
    printdir tree
    cmd <- getcmd
    case cmd of
        (Cd str) -> do
            setCurrentDirectory str
            cwd2 <- getCurrentDirectory
            when ((>) (length $ splitDirectories cwd) (length $ splitDirectories cwd2)) $ setCurrentDirectory cwd 
            go parentpath access
        CdBack -> do
            cwd <- getCurrentDirectory
            if cwd /= parentpath
                then setCurrentDirectory $ takeDirectory cwd
                else putStrLn "You cannot cd back past the snapshot (exit and start again at the desired directory)"
            go parentpath access
        Quit -> return ()

getcmd :: IO Command
getcmd = do
    putStrLn "\n"
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "type \"<subdirectory_name>\" to cd to that directory or type \"..\" to go up to parent directory"
    putStr "you can use syntax \":<command>\" along with these commands:"
    putStrLn $ concatMap ("\n  - " ++) ["quit", "exit"]
    setSGR [Reset, SetColor Foreground Vivid Yellow]
    putStr "\ndirsize>"
    hFlush stdout
    cmd <- getLine
    setSGR [Reset]
    case cmd of
        ":quit" -> return Quit
        ":exit" -> return Quit
        (':':cmd) -> putStrLn ("unknown command: " ++ cmd) >> getcmd
        ".." -> return CdBack
        path -> ifM (doesDirectoryExist path)
            (return $ Cd $ toLower path) (doesnotexist path)
                
    where 
        doesnotexist path = do
            putStrLn ("directory " ++ path ++ " does not exist!")
            getcmd

data Command = 
    Cd String | Quit | CdBack