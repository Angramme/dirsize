module Tree ( Tree(..) ) where

data Tree =
    File { name:: String, size :: Integer }
    | Folder { name :: String, size :: Integer, files :: [Tree] }