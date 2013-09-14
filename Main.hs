import Data.Tree as T
import Data.Map as M
import Data.List as L
import HTree as HT
  
type Count = Integer           
data OccurenceMap = Map Symbol Count

testcase :: String
testcase = "TEST STRING"
--Count the frequency an character occurs in a String.

-- Given a list of Trees, Combine the two smallest using makeCodeTree,
-- and place the resulting tree in its proper place in the list
compound :: [HTree] -> [HTree]
compound (x1:x2:xs) = L.insert (makeCodeTree x1 x2) xs
         

                 
