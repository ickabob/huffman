module HTree where 
import Data.Monoid
import Data.List as List
import Data.Set as Set  
import Test.HUnit



--Huffman Codes allows data to be represented as sequences of ones and zeros.
--The encoding mechanism uses variable length codes to represent textual data
--taking advantange of the fact that some symbols appear more frequently than
--others.  The most frequent occuring symbols are encoded to bit sequences of
--the shortest length.
  
--One difficulty of variable length code is, when reading a sequence of bits,
--recognizing the end of one symbol and the beggining of the next.  One
--solution would be to use a seperator symbol that signifies the boundries.
--Another solution is to design the encoding scheme such that no complete
--code for any symbol is the beggining (or prefix) of the code for any other
--symbol.

--This module implements David Huffmans encoding scheme called Huffman codes.
--A Huffman Code can be represented as a binary tree whos leaves are the
--symbols that are encoded.  At each nonleaf node there is the set composed
--of all the symbols in the leaves that lie below it.  Additionally each
--leaf node is assigned the weight of its relative frequency and each non -
--leaf node is has a weight that is the sum of the weights of all the leaves
--lying below it.

  
type Symbol = Char
              
type Count = Integer

type Message = [Symbol]

data Tree v a = Leaf v a
                | Branch v (Tree v a) (Tree v a)
                  deriving(Show,Read,Eq)

data HTree a = HLeaf (Int, Set a)
             | HBranch (Int, Set a) (HTree a) (HTree a)
               deriving(Show,Read,Eq)

instance Ord HTree where
  
                       
_partition :: Ord a =>  [a] -> [[a]]
_partition = group . sort

_counts :: [Symbol] -> [Int]
_counts = List.map length . _partition

_symbols :: [Symbol] -> [Symbol]
_symbols = List.map head . _partition

countSymbols :: String -> [(Int,Symbol)]           
countSymbols str = zip counts symbols
   where counts = _counts str
         symbols = _symbols str

constuctLeaf count symbol  = HLeaf (count, Set.insert symbol $ Set.empty)

symbolForest :: [Symbol] -> [HTree Symbol]                             
symbolForest str =  List.map (\(c,s)-> constuctLeaf c s) $ countSymbols str



-- encodingTree :: HForest -> HTree
-- encodingTree f 
--   |Set.null f       = error "Cannot construct Huffman Tree form the empty set."
--   | Set.size f == 2 = merged 
--   | Set.size f >= 2 = encodingTree $ Set.insert merged modified'
--   where
--     (max1,modified) = Set.deleteFindMax f
--     (max2,modified') = Set.deleteFindMax modified
--     merged = merge2 max1 max2

-- merge2 :: HTree -> HTree -> HTree
-- merge2 h1@(Leaf f1) h2@(Leaf f2) =
--   Branch { weight = count f1 + count f2, left = h1, right = h2}
-- merge2 h1@(Leaf f1) h2@(Branch _ _ _) =
--   Branch { weight = count f1 + weight h2, left = h1, right = h2}
-- merge2 h1@(Branch _ _ _) h2@(Leaf f1) =
--     Branch { weight = weight h1 + count f1, left = h1, right = h2}
-- merge2 h1@(Branch _ _ _) h2@(Branch _ _ _) =
--     Branch { weight = weight h1 + weight h2, left = h1, right = h2}

-- encode :: Message -> HTree -> [Bit]
-- encode "" tree = []  
-- encode mes tree = Data.List.concatMap (encodeSymbol $ tree) mes

-- encodeSymbol :: HTree -> Symbol -> [Bit]

-- encodeSymbol tree sym =
--   case tree of
--     Leaf f -> if symbol f == sym then [] else error "symbol not found in encoding tree"
--     Branch w left right ->/ 
  

