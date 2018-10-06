{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database where

import Prelude hiding (id)
-- import Debug.Trace (traceShowM)

import Data.Text (pack)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (when)
import Data.Maybe (isNothing, listToMaybe)
import Data.Map.Strict (fromList)
import qualified Data.Map.Strict as M

import Database.Bolt hiding (pack)


import Types


-- The following three functions presume that Reaction is present in the database,
-- and add corresponding relation alongside with second node if needed.
addReagent :: Reaction -> Molecule -> REAGENT_IN -> BoltActionT IO ()
addReagent react mol rel = do
        mbMol <- getMolecule $ (id :: Molecule -> Int) mol
        when (isNothing mbMol) $ putMolecule mol
        queryP_ que params
    where
        que = "MATCH (R:Reaction {id: {rid}}), (M:Molecule {id: {mid}}) " <>
            "CREATE (M)-[rel:REAGENT_IN {amount: {ri_amount}}]->(R);"
        params = fromList [
            ("rid", I $ (id :: Reaction -> Int) react),
            ("mid", I  $ (id :: Molecule -> Int) mol),
            ("ri_amount", F . realToFrac $ (amount :: REAGENT_IN -> Float) rel)]

addProduct :: Reaction -> Molecule -> PRODUCT_FROM -> BoltActionT IO ()
addProduct react mol rel = do
        mbMol <- getMolecule $ (id :: Molecule -> Int) mol
        when (isNothing mbMol) $ putMolecule mol
        queryP_ que params
    where
        que = "MATCH (R:Reaction {id: {rid}}), (M:Molecule {id: {mid}}) " <>
            "CREATE (M)<-[rel:PRODUCT_FROM {amount: {pf_amount}}]-(R);"
        params = fromList [
            ("rid", I $ (id :: Reaction -> Int) react),
            ("mid", I  $ (id :: Molecule -> Int) mol),
            ("pf_amount", F . realToFrac $ (amount :: PRODUCT_FROM -> Float) rel)]

addCatalyst :: Reaction -> Catalyst -> ACCELERATE -> BoltActionT IO ()
addCatalyst react cat rel = do
        mbCat <- getCatalyst $ (id :: Catalyst -> Int) cat
        when (isNothing mbCat) $ putCatalyst cat
        queryP_ que params
    where
        que = "MATCH (R:Reaction {id: {rid}}), (C:Catalyst {id: {cid}}) " <>
            "CREATE (C)-[rel:ACCELERATE {temperature: {temperature}, pressure: {pressure}}]->(R);"
        params = fromList [("rid", I $ (id :: Reaction -> Int) react),
            ("cid", I  $ (id :: Catalyst -> Int) cat),
            ("temperature", F . realToFrac $ (temperature :: ACCELERATE -> Float) rel),
            ("pressure", F . realToFrac $ (pressure :: ACCELERATE -> Float) rel)]



-- adds reaction to db (with all surrounding objects)
addReaction :: Reaction -> [(Catalyst, ACCELERATE)] -> [(Molecule, REAGENT_IN)] -> [(Molecule, PRODUCT_FROM)] -> BoltActionT IO ()
addReaction react cats reags prods = do
    putReaction react
    mapM_ (uncurry $ addCatalyst react) cats
    mapM_ (uncurry $ addReagent react) reags
    mapM_ (uncurry $ addProduct react) prods



-- These 3 functions just add corresponding object to the database
putReaction :: Reaction -> BoltActionT IO ()
putReaction react = queryP_ que params
    where
        que = "CREATE (R:Reaction {id: {rid}, name: {name}});"
        params = fromList [
            ("rid", I $ (id :: Reaction -> Int) react),
            ("name", T . pack  $ (name :: Reaction-> String) react)]

putMolecule :: Molecule -> BoltActionT IO ()
putMolecule mol = queryP_ que params
    where
        que = "CREATE (M:Molecule {id: {mid}, smiles: {smiles}, iupacName: {iupacName}});"
        params = fromList [
            ("mid", I $ (id :: Molecule -> Int) mol),
            ("smiles", T . pack  $ (smiles :: Molecule-> String) mol),
            ("iupacName", T . pack $ (iupacName :: Molecule-> String) mol)]

putCatalyst :: Catalyst -> BoltActionT IO ()
putCatalyst cat = 
        case (name :: Catalyst -> Maybe String) cat of
            Just name -> queryP_ (que <> ", name: \"" <> pack name <> "\"});") params 
            Nothing -> queryP_ (que <> "});") params
    where
        que = "CREATE (C:Catalyst {id: {cid}, smiles: {smiles}"
        params = fromList [
            ("cid", I $ (id :: Catalyst -> Int) cat),
            ("smiles", T . pack  $ (smiles :: Catalyst-> String) cat)]


-- these 3 functions retrieve corresponding object from the base by id
getReaction :: Int -> BoltActionT IO (Maybe Reaction)
getReaction id = do
        resp <- queryP "MATCH (R:Reaction {id: {rid}}) RETURN R;" params
        mapM exact $ (M.lookup "R") =<< listToMaybe resp
    where
        params = fromList [("rid",I id)]

getCatalyst :: Int -> BoltActionT IO (Maybe Catalyst)
getCatalyst id = do
        resp <- queryP "MATCH (C:Catalyst {id: {cid}}) RETURN C;" params
        mapM exact $ (M.lookup "C") =<< listToMaybe resp
    where
        params = fromList [("cid",I id)]

getMolecule :: Int -> BoltActionT IO (Maybe Molecule)
getMolecule id = do
        resp <- queryP "MATCH (M:Molecule {id: {mid}}) RETURN M;" params
        mapM exact $ (M.lookup "M") =<< listToMaybe resp
    where
        params = fromList [("mid",I id)]

-- these 3 functions retrieve corresponding relation from the base if it is present
getReagentRel :: Reaction -> Molecule -> BoltActionT IO (Maybe REAGENT_IN)
getReagentRel react mol = do
        resp <- queryP "MATCH (M:Molecule {id: {mid}})-[r:REAGENT_IN]-> (R:Reaction {id: {rid}}) RETURN r;" params
        mapM exact $ (M.lookup "r") =<< listToMaybe resp
    where
        params = fromList [("mid", I $ (id :: Molecule -> Int) mol), ("rid", I $ (id :: Reaction -> Int) react)]

getProductRel :: Reaction -> Molecule -> BoltActionT IO (Maybe PRODUCT_FROM)
getProductRel react mol = do
        resp <- queryP "MATCH (M:Molecule {id: {mid}})<-[r:PRODUCT_FROM]-(R:Reaction {id: {rid}}) RETURN r;" params
        mapM exact $ (M.lookup "r") =<< listToMaybe resp
    where
        params = fromList [("mid", I $ (id :: Molecule -> Int) mol), ("rid", I $ (id :: Reaction -> Int) react)]

getAccelerateRel :: Reaction -> Catalyst -> BoltActionT IO (Maybe PRODUCT_FROM)
getAccelerateRel react cat = do
        resp <- queryP "MATCH (C:Catalyst {id: {cid}})-[r:ACCELERATE]->(R:Reaction {id: {rid}}) RETURN r;" params
        mapM exact $ (M.lookup "r") =<< listToMaybe resp
    where
        params = fromList [("cid", I $ (id :: Catalyst -> Int) cat), ("rid", I $ (id :: Reaction -> Int) react)]



-- shortest path between two molecules
findPath :: Molecule -> Molecule -> BoltActionT IO (Maybe Path)
findPath m1 m2 = do
        recs <- queryP que params
        mapM exact $ (`at` "p") =<< listToMaybe recs
    where
        que = "MATCH (M1:Molecule {id: {id1}}), (M2:Molecule {id: {id2}}), " <>
            "p = shortestPath((M1)-[*]->(M2)) RETURN p;"
        params = fromList [("id1", I $ (id :: Molecule -> Int) m1), ("id2", I $ (id :: Molecule -> Int) m2)]


-- (!!) for testing purposes only, not intended to be present in real project
dropBase :: MonadIO m => BoltActionT m () 
dropBase = do
    query_ "MATCH (n) DETACH DELETE (n);"

allNodes :: MonadIO m => BoltActionT m [Value]
allNodes = do
    recs <- query "MATCH (n) RETURN (n);"
    mapM (`at` "n") recs

allRelations :: MonadIO m => BoltActionT m [Value]
allRelations = do
    recs <- query "MATCH ()-[r]->() RETURN (r);"
    mapM (`at` "r") recs

