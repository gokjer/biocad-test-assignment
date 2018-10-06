{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

-- import Data.List
-- import Data.Text (Text)
-- import Control.Monad (forM_)

import Control.Monad.IO.Class (liftIO)

import Control.Exception (bracket)

import Database.Bolt

import Types
import Database
import Database.Fill (fillBase)

defaultConfig :: BoltCfg
defaultConfig = BoltCfg { magic         = 1616949271
                , version       = 1
                , userAgent     = "hasbolt/1.3"
                , maxChunkSize  = 65535
                , socketTimeout = 5
                , host          = "127.0.0.1"
                , port          = 7687
                , user          = "neo4j"
                , password      = "neo4j"
                , secure        = False
                }


-- Enter non-default data here. If config is default, leave config = defaultConfig
config :: BoltCfg
config = defaultConfig {user = "neo4j", password = "password"}

main :: IO ()
main = do
    putStrLn "Enter random seed:"
    seed <- fmap read getLine
    bracket (connect config) close $ \pipe -> run pipe $ do
        dropBase
        fillBase seed
        nodes <- allNodes
        mapM_ (liftIO . print) nodes
        rels <- allRelations
        mapM_ (liftIO . print) rels
        react1 <- getReaction 1
        mol1 <- getMolecule 1
        rel1 <- case (react1, mol1) of
            (Just r1, Just m1) -> getReagentRel r1 m1
            _ -> return (Nothing :: Maybe REAGENT_IN)
        liftIO $ print rel1