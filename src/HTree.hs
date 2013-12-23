import Data.List
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
             
type Weight = Int
              
type Message = [Symbol]

data Bit = One | Zero deriving (Show,Read,Eq)

type Encoding = [Bit]
              
data Frequency = Frequency { count :: Int, symbol :: Symbol}
                 deriving (Show,Read,Eq)
                          
instance Ord Frequency where
  compare f1 f2 = compare (count f1) (count f2)
                 
data HTree = Leaf { frequency :: Frequency }
           | Branch { weight :: Weight
             ,left :: HTree
             ,right :: HTree
             }deriving (Show,Read,Eq)
                       
instance Ord HTree where
  compare (Leaf f1) (Leaf f2) = compare f1 f2
  compare (Branch w _ _) (Leaf f1) = compare w (count f1)
  compare (Branch w _ _) (Branch w' _ _) = compare w w'                 

_partition :: Ord a =>  [a] -> [[a]]
_partition = group . sort

_counts :: [Symbol] -> [Int]
_counts = Data.List.map length . _partition

_symbols :: [Symbol] -> [Symbol]
_symbols = Data.List.map head . _partition

countSymbols :: String -> [Frequency]           
countSymbols str = Data.List.map (\(c,s)->Frequency c s) $ zip counts symbols
   where counts = _counts str
         symbols = _symbols str

type HForest = Set HTree
               
symbolForest :: String -> [HTree]
symbolForest  = Data.List.map Leaf . countSymbols 


encodingTree :: HForest -> HTree
encodingTree f 
  |Set.null f       = error "Cannot construct Huffman Tree form the empty set."
  | Set.size f == 2 = merged 
  | Set.size f >= 2 = encodingTree $ Set.insert merged modified'
  where
    (max1,modified) = Set.deleteFindMax f
    (max2,modified') = Set.deleteFindMax modified
    merged = merge2 max1 max2

merge2 :: HTree -> HTree -> HTree
merge2 h1@(Leaf f1) h2@(Leaf f2) =
  Branch { weight = count f1 + count f2, left = h1, right = h2}
merge2 h1@(Leaf f1) h2@(Branch _ _ _) =
  Branch { weight = count f1 + weight h2, left = h1, right = h2}
merge2 h1@(Branch _ _ _) h2@(Leaf f1) =
    Branch { weight = weight h1 + count f1, left = h1, right = h2}
merge2 h1@(Branch _ _ _) h2@(Branch _ _ _) =
    Branch { weight = weight h1 + weight h2, left = h1, right = h2}

encode :: Message -> HTree -> [Bit]
encode "" tree = []  
encode mes tree = Data.List.concatMap (encodeSymbol $ tree) mes

encodeSymbol :: HTree -> Symbol -> [Bit]
encodeSymbol = undefined                

