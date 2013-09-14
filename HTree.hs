import Data.List
import Data.Set as Set  
import Test.HUnit
import Test.QuickCheck


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
              
type Weight = Int
              
type Frequency = (Int, Symbol)

type WeightedAlphabet = Set Frequency

data HuffmanTree = HLeaf {frequency :: Frequency} 
                 | Branch {
                    symbolSet :: WeightedAlphabet
                   ,left              :: HuffmanTree
                   ,right             :: HuffmanTree
                   } deriving (Show,Read,Eq,Ord)
  

encodeText :: [Symbol] -> HuffmanTree
encodeText = undefined

freqmap' :: [Symbol] -> [Frequency]
freqmap' = undefined

--Does it make sense to accept a list of Maybe symbols?
--If there are symbols, then there are frequencies.
frequencyMap :: [Symbol] -> [Frequency]
frequencyMap str =
  let counts = Data.List.map length (group . sort $ str)
      symbols = Data.List.map head (group . sort $ str)
  in zip counts symbols

frequencyForrest :: [Frequency] -> [HuffmanTree]
frequencyForrest  = Data.List.map HLeaf

codeTree :: HuffmanTree -> HuffmanTree -> HuffmanTree
codeTree (HLeaf f1) (HLeaf f2) =
    Branch {
      symbolSet = Set.fromList (f1:f2:[])
      ,left = HLeaf f1
      ,right = HLeaf f2
      }
  
codeTree leftT rightT = 
     Branch {
       symbolSet = Set.union (symbolSet leftT) (symbolSet rightT) 
       , left = leftT
       , right = rightT
       }

weight :: HuffmanTree -> Weight
weight tree = case tree of
  Branch ss l r ->  _weight ss 
  HLeaf f       ->  fst f

_weight :: WeightedAlphabet -> Weight
_weight = Set.fold (\freq count -> (fst freq) + count) 0
  

mergeTrees :: Set HuffmanTree -> HuffmanTree
mergeTrees ts = undefined
-- Set.minView ts >>=
-- (t2, ts_pp) <- Set.minView ts_p
-- return Set.insert (codeTree t1 t2) ts_pp


              
      --TESTS--
testEmptyString = TestCase $ assertEqual
                  "Emptry string returns empty list."
                  []
                  (frequencyMap "")
             
testString = "TESTSTRING" 
forrest = frequencyForrest (frequencyMap testString)
first2 = take 2 forrest
h1 = head first2
h2 = head (tail first2)
     

                  
main = runTestTT $ TestList [ testEmptyString ]
