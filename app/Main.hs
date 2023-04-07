{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE QualifiedDo        #-}
module Main where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.ByteString.Builder    (stringUtf8)
import           Data.List                  (intercalate)
import           Data.List.Singletons       (type (++))
import           Data.Singletons            (SingI)
import           Network.HTTP.Types.Status  (status200)
import           Network.Wai                (Request, Response,
                                             ResponseReceived, responseBuilder)
import           Network.Wai.Handler.Warp   (Port)
import qualified Network.Wai.Handler.Warp   (runEnv)
import qualified System.Directory           (getDirectoryContents)
import           System.OpenBSD.MultiPledge as M
import           System.OpenBSD.Pledge      (Promise (..))

-- Liftings of Wai constructs
type Application zs ps = Request -> (Response -> Pledge IO zs '[ 'Inet] ResponseReceived) -> Pledge IO zs ps ResponseReceived

runEnv :: Port
       -> Application ('Stdio ': zs ++ ps) ps
       -> Pledge IO zs ('Stdio ': ps) ()
runEnv p a = Pledge $ liftIO $ Network.Wai.Handler.Warp.runEnv p a'
  where
    a' req reply = getAction $ a req $ Pledge . reply

-- Lifting of base library component
getDirectoryContents :: (MonadIO m) => FilePath -> Pledge m zs '[ 'Rpath ] [FilePath]
getDirectoryContents = Pledge . liftIO . System.Directory.getDirectoryContents

-- Our pledge enabled application
app :: (SingI zs)
    => Application zs '[ 'Inet, 'Rpath]
app _req reply = M.do
  fs <- getDirectoryContents "."
  reply $ responseBuilder status200 [] $ stringUtf8 $ intercalate "\n" fs

-- Run the application and discharge the pledge annotations
main :: IO ()
main = do
  runPledge $ runEnv 1974 app
