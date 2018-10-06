{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database.Fill (fillBase) where

import System.Random (randoms, mkStdGen)

import Database.Bolt (BoltActionT)


import Types
import Database (addReaction)



-- = Anoxygenic photosynthesis
react_ap :: Reaction
react_ap = Reaction {id=1, name="Anoxygenic photosynthesis"}

cats_ap :: [(Catalyst, ACCELERATE)]
cats_ap = [(Catalyst {id=1, smiles="Nu", name=Just "Light"}, ACCELERATE {temperature=50, pressure=100}),
    (Catalyst {id=2, smiles="Smile", name=Just "Bacteria"}, ACCELERATE {temperature=50, pressure=100})]

reags_ap :: [(Molecule, REAGENT_IN)]
reags_ap = [(Molecule {id=1, smiles="CO2", iupacName="Carbon dioxide"}, REAGENT_IN {amount=6}),
    (Molecule {id=2, smiles="H2S", iupacName="Hydrogen sulfide"}, REAGENT_IN {amount=12})]

prods_ap :: [(Molecule, PRODUCT_FROM)]
prods_ap = [(Molecule {id=3, smiles="C6H12O6", iupacName="Glucose"}, PRODUCT_FROM {amount=1}),
    (Molecule {id=4, smiles="S", iupacName="Sulfur"}, PRODUCT_FROM {amount=12}),
    (Molecule {id=5, smiles="H2O", iupacName="Water"}, PRODUCT_FROM {amount=6})]


-- = Photosynthesis
react_p :: Reaction
react_p = Reaction {id=2, name="Photosynthesis"}

cats_p :: [(Catalyst, ACCELERATE)]
cats_p = [(Catalyst {id=3, smiles="Nu", name=Just "Light"}, ACCELERATE {temperature=50, pressure=100}),
    (Catalyst {id=4, smiles="Tree", name=Just "Oak"}, ACCELERATE {temperature=50, pressure=100})]

reags_p :: [(Molecule, REAGENT_IN)]
reags_p = [(Molecule {id=1, smiles="CO2", iupacName="Carbon dioxide"}, REAGENT_IN {amount=6}),
    (Molecule {id=5, smiles="H2O", iupacName="Water"}, REAGENT_IN {amount=6})]

prods_p :: [(Molecule, PRODUCT_FROM)]
prods_p = [(Molecule {id=3, smiles="C6H12O6", iupacName="Glucose"}, PRODUCT_FROM {amount=1}),
    (Molecule {id=6, smiles="O2", iupacName="Oxygen"}, PRODUCT_FROM {amount=12})]


-- Generate the rest randomly
reactionCount :: Int
reactionCount = 20
molsCount :: Int
molsCount = 50
catsCount :: Int
catsCount = 15

mols :: [Molecule]
mols = map fst (reags_ap ++ reags_p) ++ map fst (prods_ap ++ prods_p) ++
    [
        Molecule {
            id=i,
            smiles="FAKE",
            iupacName="Fake Molecule #" ++ show i
        }
        | i <- [7..molsCount]
    ]

cats :: [Catalyst]
cats = map fst (cats_ap ++ cats_p) ++
    [
        Catalyst {
            id=i,
            smiles="FAKE",
            name = Just $ "Fake catalyst #" ++ show i
        }
        | i <- [5..catsCount]
    ]

pick :: a -> [a] -> Int -> a
pick _ (a:_) n | n <= 0 = a
pick def (_:as) n = pick def as (n-1)
pick def [] _ = def

defaultMolecule :: Molecule
defaultMolecule = Molecule {id=(-1), smiles="DEFAULT", iupacName="Default Molecule"}

defaultCatalyst :: Catalyst
defaultCatalyst = Catalyst {id=(-1), smiles="DEFAULT", name=Nothing}

genReaction :: Int -> [Int] -> BoltActionT IO [Int]
genReaction id rands =
    let react = Reaction {id=id, name="Fake Reaction #" ++ show id}
        (reag_r, rest1) = splitAt 3 rands
        reags = map (\m -> (m, REAGENT_IN {amount=1})) $
            map (pick defaultMolecule mols . \x-> x `mod` molsCount) reag_r
        (prod_r, rest2) = splitAt 2 rest1
        prods = map (\m -> (m, PRODUCT_FROM {amount=1})) $
            map (pick defaultMolecule mols . \x-> x `mod` molsCount) prod_r
        (cat_r, rest3) = splitAt 2 rest2
        rcats = map (\c -> (c, ACCELERATE {temperature=16, pressure=100})) $
            map (pick defaultCatalyst cats . \x-> x `mod` catsCount) cat_r
    in do
        addReaction react rcats reags prods
        return rest3


fillBase :: Int -> BoltActionT IO ()
fillBase seed = do
    addReaction react_ap cats_ap reags_ap prods_ap
    addReaction react_p cats_p reags_p prods_p
    let rands = randoms $ mkStdGen seed
    _ <- foldl (>>=) (return rands) $ map genReaction [3..reactionCount]
    return ()
