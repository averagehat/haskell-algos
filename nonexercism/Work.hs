import qualified Data.Map.Strict as H
import Data.Foldable (foldr')
import Data.Functor ((<$>))
import Control.Monad (liftM)
import Data.List (permutations, unfoldr, splitAt, concatMap, delete)
degens = H.fromList  [('A', "A"), ('C', "C"), ('G', "G"), ('T', "T"), ('R', "AG"), ('Y',"CT"), ('S', "GC"), ('W', "AT")]

--type OutputRow = { NTpos :: Int 
--                  ,NTs :: Seq
--                  ,Synonymous :: Bool
--                  ,AAs :: Maybe [Seq] -- only output AAs/AA positions if it's non-synonomous (would report a different AA)
--                  ,AApos :: Maybe Int }
main = do
  print $ expand  "ATR" -- "Isoleucine", -- "Methionine Start",
  print $ expand  "ATC"  -- returns its normal AA (synonymous, without degen)
  print $ expand  "zzz"  -- Nothing, not in `degen` list
  print $ expand  "ATRYCSA"  -- Nothing, not divisible by 3
  print (length aas, length codons)

expand xs =  sequence $ map expandTriple $ triples xs
expandTriple xs = do
    degens' <- sequence $ map (`H.lookup` degens) xs
    let perms = sequence degens'
    aas <- sequence $ map (`H.lookup` codonTable) perms
    return aas
    
splitAt' n xs = case (splitAt n xs) of
  ([], []) -> Nothing
  (x, y) -> Just (x, y)
  
triples xs = takeWhile (not . null) $ unfoldr (Just . (splitAt 3)) xs
--triples xs =  unfoldr (maybeEmpty . (splitAt 3)) xs
--  where maybeEmpty (ys, xs') = if (null xs') then Nothing else Just (ys, xs')

perms [] = [""]
perms (x:xs) = concatMap (doPerm $ perms xs) x
  where doPerm xs' x' = map (x':) xs'

--expand xs = sequence $ map expand' $ triples xs
--  where
--    expand' s  = liftM (lookupAA . sequence) $ sequence $ map (`H.lookup` degens) s
--    lookupAA = map (`H.lookup` codonTable)  -- dont sort )

--splitAt' :: Int -> [Int] -> Maybe ([Int], [Int])
--splitAt' n xs = if (n' > n) then (Just (l, r)) else Nothing
--  where
--    (n', (l, r)) = foldr1 f (0, ([], [])) xs
--    f x (i, (ls, rs)) = if (i >= n) then (i, (ls, rs)) else ((i + 1), (x:ls, []))


type CodonTable = H.Map String String

codonTable :: CodonTable
codonTable = foldr f (H.empty) $ zip codons aas
  where
    f :: ([String], String) -> CodonTable -> CodonTable
    f (ks, v) m = foldr (\k m' -> H.insert k v m') m ks
    
codons =  [
    ["GCT", "GCC", "GCA","GCG"],
    ["TGT", "TGC"],
    ["GAT", "GAC"],
    ["GAA", "GAG"],
    ["TTT", "TTC"],
    ["GGT", "GGC","GGA","GGG"],
    ["CAT", "CAC"],
    ["ATT", "ATC", "ATA"],
    ["AAA", "AAG"],
    ["TTA", "TTG", "CTT","CTC","CTA","CTG"],
    ["ATG"],
    ["AAT", "AAC"],
    ["CCT", "CCC", "CCA","CCG"],
    ["CAA", "CAG"],
    ["CGT", "CGC", "CGA","CGG","AGA","AGG"],
    ["TCT", "TCC", "TCA","TCG","AGT","AGC"],
    ["ACT", "ACC", "ACA","ACG"],
    ["GTT", "GTC", "GTA","GTG"],
    ["TGG"],
    ["TAT", "TAC"]]
aas = ["Alanine",
    "Cysteine",
    "Aspartic acid",
    "Glutamic acid",
    "Phenylalanine",
    "Glycine",
    "Histidine",
    "Isoleucine",
    "Lysine",
    "Leucine",
    "Methionine, Start",
    "Asparagine",
    "Proline",
    "Glutamine",
    "Arginine",
    "Serine",
    "Threonine",
    "Valine",
    "Tryptophan",
    "Tyrosine"]
--    "Ochre, Opal, Amber, Stop"]
