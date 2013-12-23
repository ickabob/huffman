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
              
type Frequency = (Count, Symbol)

type WeightedAlphabet = Set Frequency

_weight :: WeightedAlphabet -> Weight
_weight = Set.fold (\freq count -> (fst freq) + count) 0

data HTree = Leaf { frequency :: (Int,Char) }
           | Branch { weight :: Weight
             ,left :: HTree
             ,right :: HTree
             }deriving (Show,Read,Eq)
                       
instance Ord HTree where
  compare (Leaf (w, _)) (Leaf (w', _)) = compare w w'
  compare (Branch w _ _) (Leaf (w', _)) = compare w w'
  compare (Branch w _ _) (Branch w' _ _) = compare w w'                 

_partition :: Ord a =>  [a] -> [[a]]
_partition = group . sort

_counts :: [Symbol] -> [Int]
_counts = Data.List.map length . _partition

_symbols :: [Symbol] -> [Symbol]
_symbols = Data.List.map head . _partition

countSymbols :: String -> [(Int,Char)]           
countSymbols str = zip counts symbols
   where counts = _counts str
         symbols = _symbols str

type HForest = Set HTree
               
symbolForrest :: String -> HForrest
symbolForrest = Set.fromList $ Data.List.map Leaf . countSymbols

encodingTree :: HForest -> HTree
encodingTree f 
  |Set.null f       = error "Cannot construct Huffman Tree form the empty set."
  | Set.size f == 2 = merged 
  | Set.size f >= 2 = constructOptimal $ Set.insert merged modified'
  where
    (max1,modified) = Set.deleteFindMax f
    (max2,modified') = Set.deleteFindMax modified
    merged = merge2 max1 max2

merge2 :: HTree -> HTree -> HTree
merge2 (Leaf {frequency = f1} ) (Leaf {frequency = f2})
  =  Branch { weight = (fst f1) + (fst f2)
            , left = Leaf f1
            , right = Leaf f2}
  
merge2 (Leaf {frequency = f1} ) (Branch {weight = w, left = l, right = r})
  = Branch { weight =  (fst f1) + w
           , left = Leaf f1
           , right = Branch w l r}
                                                                                 
merge2 (Branch {weight = w, left = l, right = r}) (Leaf {frequency = f1})
  = Branch { weight = (fst f1) + w
           , right = Branch w l r
           , left = Leaf f1}
                                                                                
merge2 (Branch {weight = w1, left = l1, right = r1}) (Branch {weight = w2, left = l2, right = r2})
  = Branch { weight = w1 + w2
           , left = Branch w1 l1 r1
           , right = Branch w2 l2 r2}
