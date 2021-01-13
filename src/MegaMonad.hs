module MegaMonad (MegaMonad, runMegaMonad, liftIO, MyState(..), FileNode(..)) where
import Control.Monad.Reader
import Conduit
import qualified Data.HashMap.Lazy as M
import Codec.Archive.Zip (EntryDescription)


data FileNode = Dir | File EntryDescription
instance Show FileNode where
  show Dir = "DIR"
  show (File _) = "File"

data MyState = MyState { stArchivePath :: FilePath
                       , stArchiveEntries :: M.HashMap FilePath FileNode
                       }
type MegaMonad a = ResourceT (ReaderT MyState IO) a

runMegaMonad :: MyState -> MegaMonad a -> IO a
runMegaMonad path m = runReaderT readert path
  where readert = runResourceT m
