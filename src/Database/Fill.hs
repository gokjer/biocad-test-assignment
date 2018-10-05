{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database.Fill (fillBase) where

-- import Data.Text (Text)
import Control.Monad (forM_)
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

mols :: [Molecule]
mols = [Molecule {id=i, smiles="FAKE", iupacName="Fake Molecule #" ++ show i} | i <- [7..40]]

cats :: [Catalyst]
cats = [Catalyst {id=i, smiles="FAKE", name = Just $ "Fake catalyst #" ++ show i} | i <- [5..14]]

fillBase :: BoltActionT IO ()
fillBase = do
    addReaction react_ap cats_ap reags_ap prods_ap
    addReaction react_p cats_p reags_p prods_p
    forM_ [3..20] $ \ i ->
        let react = Reaction {id=i, name="Fake Reaction #" ++ show i}
            reags = map (\m -> (m, REAGENT_IN {amount=1})) $
                map ((mols!!) . \x-> x `mod` 34) $ take 2 $ (randoms (mkStdGen i) :: [Int])
            prods = map (\m -> (m, PRODUCT_FROM {amount=1})) $
                map ((mols!!) . \x-> x `mod` 34) $ take 1 $ (randoms (mkStdGen i) :: [Int])
            rcats = map (\c -> (c, ACCELERATE {temperature=16, pressure=100})) $
                map ((cats!!) . \x-> x `mod` 10) $ take 1 $ (randoms (mkStdGen i) :: [Int])
        in
        addReaction react rcats reags prods

            