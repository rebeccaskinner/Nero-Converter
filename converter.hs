import Prelude hiding (catch)
import Data.List
import Control.Monad
import System (getArgs)
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error hiding (catch)
import Control.Exception

ultraIsoPath = "C:\\Program Files (x86)\\UltraISO\\UltraISO.exe"
lamePath = "C:\\Users\\rebecca\\bin\\lame\\lame.exe" 

dirChildren d = do
    cont <- catch (getDirectoryContents d) permissionHandler
    return (map (d</>) (filter visible cont))
    where
    visible p = '.' /= (head p)
    permissionHandler e = if isPermissionError e
                          then return []
                          else ioError e

getContentsRecursive top = do
    children <- dirChildren top
    subdirs  <- filterM doesDirectoryExist children
    files    <- filterM fileFilter children
    subFiles <- mapM getContentsRecursive subdirs
    return $ files ++ concat subFiles
    where
    fileFilter f = fmap ((&&) ("nrg" `isSuffixOf` f)) $ doesFileExist f

showHelp = do
    putStrLn "converter: Convert nero audio disk images (.nrg) to mp3"
    putStrLn "usage: convert.exe <input dir> <output dir>"

-- main = fmap head getArgs >>= getContentsRecursive >>= mapM_ putStrLn
main = do
    (srcPath:dstPath:_) <- getArgs
    getContentsRecursive srcPath >>= mapM_ putStrLn
