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
              
type Count = Int

type Message = [Symbol]

data WeightedSet a =  WeightedSet {weight::Int, symbols :: Set a} deriving (Read,Eq,Ord)
                                                          
instance (Show a) => Show (WeightedSet a) where
  show x = "(" ++ show (weight x) ++ "," ++ show (Set.toList $ symbols x) ++ ")"
                                                          
data HTree a = HLeaf { weightedSet :: WeightedSet a}
             | HBranch { weightedSet :: WeightedSet a,
                         left, right :: HTree a}
               deriving(Read,Eq,Ord)

instance (Show a) => Show (HTree a) where
  show (HLeaf wset) = "{Leaf: " ++ show wset ++ "}"
  show (HBranch wset left right) = "{Branch: " ++ show wset ++ show left ++ show right ++ "}"

data Bit = Zero | One
         deriving (Read, Eq)
instance Show Bit where
  show One = show 1
  show Zero  = show 0
                                   
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

constuctLeaf :: (Ord a) => Int -> a -> HTree a
constuctLeaf count symbol  = HLeaf $ WeightedSet count (Set.singleton symbol)

mergeTrees ::(Ord a)=> HTree a -> HTree a -> HTree a
mergeTrees t1 t2 = HBranch mergedSet t1 t2
  where
    mergedSet = _merge (weightedSet t1) (weightedSet t2)
    _merge x y  = let newWeights = weight x + weight y
                      newSymbols = Set.union (symbols x) (symbols y)
                  in WeightedSet newWeights newSymbols

symbolForest :: [Symbol] -> [HTree Symbol]                             
symbolForest str = List.map (\(c,s)-> constuctLeaf c s) $ countSymbols str

encodingTree :: Set (HTree Symbol) -> HTree Symbol
encodingTree f 
  | Set.null f      = error "Cannot construct Huffman Tree form the empty set."
  | Set.size f == 2 = merged 
  | Set.size f >= 2 = encodingTree $ Set.insert merged modified'
  where
    (min1,modified) = Set.deleteFindMin f
    (min2,modified') = Set.deleteFindMin modified
    merged = mergeTrees min1 min2

encode :: Message -> HTree Symbol  -> [Bit]
encode "" tree = []  
encode mes tree = List.concatMap (encodeSymbol $ tree) mes

encodeSymbol :: HTree Symbol -> Symbol -> [Bit]
encodeSymbol tree symbol = if Set.member symbol (symbolset tree)
                        then choosedirection tree
                        else error "Symbol is not valid, it does not exist in encoding tree."
  where                          
    symbolset tree' = symbols $ weightedSet tree'
    choosedirection (HLeaf x) = []
    choosedirection tree =
      case Set.member symbol $ symbolset (left tree) of
        True -> One : (choosedirection (left tree))
        False -> Zero : (choosedirection (right tree))

   
  

