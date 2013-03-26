import Data.Tree as T
import Data.Map as M
import HTree as HT
  
type Count = Integer           
data OccurenceMap = Map Symbol Count

testcase :: String
testcase = "TEST STRING"
--Count the frequency an character occurs in a String.

--Given a character key and a OccurenceMap, add that key to the map.  If key 
--already exists, increment occurence count.
addToMap   :: Symbol -> Map Symbol Count -> Map Symbol Count
addToMap x = M.insertWith (+) x 1

frequencyMap   :: [Symbol] -> Map Symbol Count
frequencyMap s = Prelude.foldr addToMap M.empty s

frequencyCount   :: (Num a, Ord k) => Map k a -> [(k,a)]
frequencyCount m = M.assocs m

makeForrest    :: [Frequency] -> [HTree]
makeForrest fs = Prelude.map makeLeaf fs

-- Given a list of Trees, Combine the two smallest using makeCodeTree,
-- and place the resulting tree in its proper place in the list

