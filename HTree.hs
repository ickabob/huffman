module HTree
    (HTree(..)
    ,Symbol
    ,Weight
    ,Frequency
    ,SymbolSet
    ,makeLeaf
    ,isLeaf
    ,makeCodeTree) where
      
type Symbol = Char
type Weight = Integer
type Frequency = (Symbol, Weight)
type SymbolSet = [Symbol]


data HTree = Leaf Frequency
           | Branch HTree HTree Weight SymbolSet
             deriving (Show )

instance Eq HTree where
  (Leaf x)         == (Leaf y)           = x == y
  (Branch l r _ _) == (Branch l' r' _ _) = l == l' && r == r'
  _                ==  _                 = False

instance Ord HTree where
    (Leaf _ ) <= (Branch _ _ _ _)         = True
    (Leaf x)  <= (Leaf y)                 = (weight x <= weight y)
    (Branch _ _ _ _) <= (Leaf _)          = False
    (Branch _ _ w _) <= (Branch _ _ w' _) = (w <= w')
                      
weight       :: Frequency -> Weight                 
weight (s,w) = w

symbol       :: Frequency -> Symbol               
symbol (s,w) = s

makeLeaf     :: Frequency -> HTree
makeLeaf f = Leaf f

isLeaf (Leaf f) = True
isLeaf _        = False

symbolLeaf          :: HTree -> Symbol
symbolLeaf (Leaf f) = symbol f
symbolLeaf _        = undefined

weightLeaf          :: HTree -> Weight
weightLeaf (Leaf f) = weight f
weightLeaf _        = undefined

append       :: SymbolSet -> SymbolSet -> SymbolSet
append x1 x2 = case null x1 of
                 True -> x2
                 False -> (head x1) : (append (tail x1) x2)

leftBranch                  :: HTree -> HTree
leftBranch (Branch l _ _ _) = l
leftBranch _                = undefined

rightBranch                  :: HTree -> HTree
rightBranch (Branch _ r _ _) = r
rightBranch _                = undefined

symbols          :: HTree -> SymbolSet
symbols (Leaf f)          = (symbol f):[]
symbols (Branch l r _ ss) = ss

weights          :: HTree -> Weight
weights (Leaf f)         = weight f 
weights (Branch _ _ w _) = w

makeCodeTree :: HTree -> HTree -> HTree
makeCodeTree left right = let union =  (symbols left) ++ (symbols right)
                              compoundWeight = (weights left) + (weights right)
                          in
                            Branch left right compoundWeight union


