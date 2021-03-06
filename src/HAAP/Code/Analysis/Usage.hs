{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HAAP.Code.Analysis.Usage where

import HAAP.IO
import HAAP.Core
import HAAP.Code.Haskell
import HAAP.Log
import HAAP.Plugin
import HAAP.Shelly

import qualified Data.Foldable as Foldable
import Data.Default
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Set (Set(..))
import qualified Data.Map as Map
import Data.Map (Map(..))
import Data.List as List
import Data.Either
import Data.Csv (header,DefaultOrdered(..),Record(..),ToNamedRecord(..),FromNamedRecord(..),(.:),(.=),namedRecord)
import Data.Maybe

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class

import System.FilePath.Find as FilePath
import System.FilePath

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.ExactPrint
import Language.Haskell.Exts.Extension

import GHC.Generics (Generic)

data Usage = Usage
    { typesUsage :: Int -- number of type declarations
    , datasUsage :: Int -- number of data/newtype declarations
    , preludeUsage :: Int -- number of prelude high-order data types usage
    , baseNonHighOrderUsage :: Int -- number of used non-high-order base definitions
    , baseHighOrderUsage :: Int -- number of used high-order base definitions
    }
  deriving (Show,Generic)

instance NFData Usage where
    
instance DefaultOrdered Usage where
    headerOrder _ = header ["typesUsage","datasUsage","preludeUsage","baseNonHighOrderUsage","baseHighOrderUsage"]

instance ToNamedRecord Usage where
    toNamedRecord (Usage x1 x2 x3 x4 x5) = namedRecord
        ["typesUsage" .= x1,"datasUsage" .= x2,"preludeUsage" .= x3,"baseNonHighOrderUsage" .= x4,"baseHighOrderUsage" .= x5]
instance FromNamedRecord Usage where
    parseNamedRecord m = Usage <$>
        m .: "typesUsage" <*> m .: "datasUsage" <*> m .: "preludeUsage"  <*> m .: "baseNonHighOrderUsage" <*> m .: "baseHighOrderUsage"

instance Default Usage where
    def = Usage (-1) (-1) (-1) (-1) (-1)

runUsage :: (MonadIO m,HaapStack t m) => [FilePath] -> BaseDefs -> Haap t m Usage
runUsage files basedefs = do
    (ts,ds,ps) <- runDatatypes files
    (nho,ho) <- runFunctionUsage basedefs files
    return $ Usage ts ds ps nho ho

runDatatypes :: (MonadIO m,HaapStack t m) => [FilePath] -> Haap t m (Int,Int,Int)
runDatatypes files = do
    ccs <- mapM datatypes files
    let cc = Foldable.foldr (\(a,b,c) (x,y,z) -> (a+x,b+y,c+z)) (0,0,0) ccs
    return cc

datatypes :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m (Int,Int,Int)
datatypes m = do
    let ioargs = def
    orLogDefault (-1,-1,-1) $ runBaseShWith (ioargs) $ do
        x <- liftM (Text.unpack . resStdout) $ shCommandWith ioargs "egrep" ["-R","-w","type",m]
        y <- liftM (Text.unpack . resStdout) $ shCommandWith ioargs "egrep" ["-R","-w","data|newtype",m]
        z <- liftM (Text.unpack . resStdout) $ shCommandWith ioargs "egrep" ["-R"," Maybe| Either",m]
        return (length $ lines x,length $ lines y,length $ lines z)
    
getBaseDefs :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m BaseDefs
getBaseDefs basepath = orLogDefault (Set.empty,Set.empty) $ do
    hs <- hsFiles basepath
    let parse f = do
        --logEvent $ "parsing base usage for " ++ f
        parseHaskellFile f
    mods <- liftM (rights) $ mapM (orLogEither . parse) hs
    let ns = Map.unions $ map moduFunctionNames mods
    let (nonho,ho) = Map.partition not ns
    return (Map.keysSet nonho,Map.keysSet ho)
    
type BaseDefs = (Set (Name ()),Set (Name ()))
    
runFunctionUsage :: (MonadIO m,HaapStack t m) => BaseDefs -> [FilePath] -> Haap t m (Int,Int)
runFunctionUsage (nho,ho) files = orLogDefault (-1,-1) $ do
    let parse f = do
        logEvent $ "parsing usage for " ++ f
        parseHaskellFile f
    ms <- liftM catMaybes $ mapM (orLogMaybe . parse) files
    let fs = Map.unions $ map moduNames ms
    let nonhos = Map.filterWithKey (\k _ -> k `elem` nho) fs
    let hos = Map.filterWithKey (\k _ -> k `elem` ho) fs
    return (Map.size nonhos,Map.size hos)


