module Fuse where
import System.Fuse
import System.Posix.Types
import System.Posix.Files

import ZipHelper
import MegaMonad

import Codec.Archive.Zip
import qualified Data.ByteString as B
import Lens.Micro

initOps :: FilePath -> IO (FuseOperations ())
initOps path = do
  st <- initState path
  return $ fuseOps st
fuseOps :: MyState -> FuseOperations ()
fuseOps path = defaultFuseOps
        { fuseGetFileStat        = runMegaMonad path . fsGetFileStat
        , fuseOpen               = fsOpen
        , fuseRead               = \a b c d -> runMegaMonad path (fsRead a b c d)
        , fuseOpenDirectory      = \_ -> return eOK
        , fuseReadDirectory      = runMegaMonad path . fsReadDir
        , fuseAccess             = \_ _ -> return eOK
        , fuseRename             = \_ _ -> return eROFS
        , fuseCreateLink         = \_ _ -> return eROFS
        , fuseCreateSymbolicLink = \_ _ -> return eROFS
        , fuseWrite              = \_ _ _ _ -> return $ Left eROFS
        , fuseGetFileSystemStats = getFileSystemStats
        , fuseSetFileTimes       = \_ _ _ -> return eROFS
        , fuseCreateDevice       = \_ _ _ _ -> return eROFS
        , fuseRemoveLink         = \_ -> return eROFS
        , fuseSetFileSize        = \_ _ -> return eROFS
        , fuseRemoveDirectory    = \_ -> return eROFS
        , fuseCreateDirectory    = \_ _ -> return eROFS
        }

fsGetFileStat :: FilePath -> MegaMonad (Either Errno FileStat)
fsGetFileStat "/" = do
  ctx  <- liftIO getFuseContext
  return $ Right $ dirStat ctx
fsGetFileStat path = do
  ctx  <- liftIO getFuseContext
  j <- getFileDescr $ drop 1 path
  return . Right $ case j of
    Just (File x) -> zipFileStat ctx x
    Just Dir -> dirStat ctx
    Nothing -> error "eNoEnt"

fsRead :: FilePath -> () -> ByteCount -> FileOffset -> MegaMonad (Either Errno B.ByteString)
fsRead path _ count offs = do
  let path' = drop 1 path
  res <- getFile path' count offs
  return $ Right res

fsReadDir :: FilePath -> MegaMonad (Either Errno [(FilePath, FileStat)])
fsReadDir "/" = fsReadDir ""
fsReadDir path = do
  ctx  <- liftIO getFuseContext
  files <- getAllInDir $ drop 1 $ path ++ "/"
  let f (Just (File x)) = zipFileStat ctx x
      f (Just Dir) = dirStat ctx
      f Nothing = error "eNoEnt"
  let res = map (over _2 f) files 
  return $ Right res
fsOpen :: Monad m => p1 -> p2 -> p3 -> m (Either a ())
fsOpen _ _ _ = return $ Right ()

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat
  { statEntryType        = Directory
  , statFileMode         = foldr1
                             unionFileModes
                             [ ownerReadMode
                             , ownerExecuteMode
                             , groupReadMode
                             , groupExecuteMode
                             , otherReadMode
                             , otherExecuteMode
                             ]
  , statLinkCount        = 2
  , statFileOwner        = fuseCtxUserID ctx
  , statFileGroup        = fuseCtxGroupID ctx
  , statSpecialDeviceID  = 0
  , statFileSize         = 16384
  , statBlocks           = 1
  , statAccessTime       = 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }

zipFileStat :: FuseContext -> EntryDescription -> FileStat
zipFileStat ctx descr = FileStat
  { statEntryType        = RegularFile
  , statFileMode         = foldr1
                             unionFileModes
                             [ ownerReadMode
                             , ownerWriteMode
                             , groupReadMode
                             , groupWriteMode
                             , otherReadMode
                             , otherWriteMode
                             ]
  , statLinkCount        = 1
  , statFileOwner        = fuseCtxUserID ctx
  , statFileGroup        = fuseCtxGroupID ctx
  , statSpecialDeviceID  = 0
  , statFileSize         = fromIntegral $ edUncompressedSize descr
  , statBlocks           = fromIntegral $ edUncompressedSize descr `div` 16384
  , statAccessTime       = 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }

getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats str = return $ Right $ FileSystemStats
  { fsStatBlockSize       = 16384
  , fsStatBlockCount      = 1
  , fsStatBlocksFree      = 1
  , fsStatBlocksAvailable = 1
  , fsStatFileCount       = 5
  , fsStatFilesFree       = 10
  , fsStatMaxNameLength   = 255
  }
