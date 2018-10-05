{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Data.Text (unpack)
import qualified Data.Map.Strict as M

import Database.Bolt hiding (unpack)


data Molecule = Molecule {
    id :: Int,
    smiles :: String,
    iupacName :: String
}

    deriving (Show, Eq, Ord)

instance RecordValue Molecule where
    exact (S (Structure {fields = (_:(L labels):(M props):_)})) | elem (T "Molecule") labels =
        do
            id <- exact =<< props `at` "id"
            smiles <- (return . unpack) =<< exact =<< props `at` "smiles"
            iupacName <- (return . unpack) =<< exact =<< props `at` "iupacName"
            return Molecule {id=id, smiles=smiles, iupacName=iupacName}
    exact x = fail $ show x ++ " is not a Molecule"

data Reaction = Reaction {
    id :: Int,
    name :: String
}
    deriving (Show, Eq, Ord)

instance RecordValue Reaction where
    exact (S (Structure {fields = (_:(L labels):(M props):_)})) | elem (T "Reaction") labels =
        do
            id <- exact =<< props `at` "id"
            name <- (return . unpack) =<< exact =<< props `at` "name"
            return Reaction {id=id, name=name}
    exact x = fail $ show x ++ " is not a Reaction"

data Catalyst = Catalyst {
    id :: Int,
    smiles :: String,
    name :: Maybe String
}
    deriving (Show, Eq, Ord)

instance RecordValue Catalyst where
    exact (S (Structure {fields = (_:(L labels):(M props):_)})) | elem (T "Catalyst") labels =
        do
            id <- exact =<< props `at` "id"
            smiles <- (return . unpack) =<< exact =<< props `at` "smiles"
            -- name <- return $ do
            --     mbName <- M.lookup "name" props
            --     (Just . unpack) =<< exact mbName
            name <- (fmap . fmap) unpack $ mapM exact $ M.lookup "name" props
            return Catalyst {id=id, smiles=smiles, name= name}
    exact x = fail $ show x ++ " is not a Catalyst"

data PRODUCT_FROM = PRODUCT_FROM {
    amount :: Float
}
    deriving (Show, Eq)

instance RecordValue PRODUCT_FROM where
    exact (S (Structure {fields = (_:_:_:(T "PRODUCT_FROM"):(M props):_)})) =
        do
            amount :: Double <- exact =<< props `at` "amount"
            return PRODUCT_FROM {amount = realToFrac amount}
    exact x = fail $ show x ++ " is not a PRODUCT_FROM"

data ACCELERATE = ACCELERATE {
    temperature :: Float,
    pressure :: Float
}
    deriving (Show, Eq)

instance RecordValue ACCELERATE where
    exact (S (Structure {fields = (_:_:_:(T "ACCELERATE"):(M props):_)})) =
        do
            temperature :: Double <- exact =<< props `at` "temperature"
            pressure :: Double <- exact =<< props `at` "pressure"
            return ACCELERATE {temperature = realToFrac temperature, pressure = realToFrac pressure}
    exact x = fail $ show x ++ " is not a ACCELERATE"


data REAGENT_IN = REAGENT_IN {
    amount :: Float
}
    deriving (Show, Eq)

instance RecordValue REAGENT_IN where
    exact (S (Structure {fields = (_:_:_:(T "REAGENT_IN"):(M props):_)})) =
        do
            amount :: Double <- exact =<< props `at` "amount"
            return REAGENT_IN {amount = realToFrac amount}
    exact x = fail $ show x ++ " is not a REAGENT_IN"