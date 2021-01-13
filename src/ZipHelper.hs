{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZipHelper (getFile, getAllInDir, getFileDescr, EntryDescription, initState) where

import Codec.Archive.Zip
import Control.Monad.Reader
import Data.List
import Data.List.Split
import qualified Data.Map as MM
import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Posix.Types

import Conduit
import qualified Data.Conduit.Binary as BC

import MegaMonad
import ToStrict



getAllInDir :: FilePath -> MegaMonad [(FilePath, Maybe FileNode)]
getAllInDir path = do
  entries <- asks stArchiveEntries
  let pathLength = length path
  let suitable = nub 
               . map (takeWhile (/= '/') . drop pathLength) 
               . filter (isPrefixOf path) 
               $ M.keys entries
  return $ map (\x -> (x, entries M.!? (path ++ x))) suitable

getFileDescr :: FilePath -> MegaMonad (Maybe FileNode)
getFileDescr path = do
  entries <- asks stArchiveEntries
  return $ entries M.!? path

getSubDirs :: FilePath -> [FilePath]
getSubDirs path = map f [1..l]
  where splitted = init $ splitOn "/" path
        l = length splitted
        f i = init $ concatMap (++ "/") $ take i splitted
getFile :: FilePath -> ByteCount -> FileOffset -> MegaMonad B.ByteString
getFile path count offs = do
  --liftIO $ print $ "trying to find " ++ path ++ "with count: " ++ show count ++ " and offs " ++ show offs
  file <- asks stArchivePath
  src <- withArchive file $ getFileZip path
  --liftIO $ print "FOUND"
  res <- runConduit $ src .| (BC.drop (fromIntegral offs) >> BC.sinkLbs)
  --liftIO $ print $ BL.take (fromIntegral count) res
  return $ toStrict $ BL.take (fromIntegral count) res
getFileZip :: (PrimMonad m, MonadIO m, MonadThrow m, MonadResource m) => FilePath -> ZipArchive (ConduitT () B.ByteString m ())
getFileZip path = do
  selector <- mkEntrySelector path
  getEntrySource selector

initState :: FilePath -> IO MyState
initState path = withArchive path getE
  where 
    getE = do
      entries <- getEntries
      let filePathes = MM.mapKeys unEntrySelector entries
      --let dirs  = MM.toList . MM.map (const Dir) . MM.mapKeys (reverse . drop 1 . dropWhile (/= '/') . reverse) $ filePathes
      let dirs  = concatMap (\(a, _) -> map (, Dir) $ getSubDirs a) $ MM.toList filePathes
      let files = MM.toList $ MM.map File filePathes
      let resMap = M.fromList $ dirs ++ files
      return $ MyState path resMap

