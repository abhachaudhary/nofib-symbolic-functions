module Digraph where

{-
elem :: Int -> [Int] -> Bool
elem x [] = False
elem x (y:ys) = (x == y) || (elem x ys)

(||) :: Bool -> Bool -> Bool
(||) True  x = True
(||) False x = x

(++) :: [Int] -> [Int] -> [Int]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x) : (map f xs)

snd (x,y) = y
-}

type Edge  vertex = (vertex, vertex)
type Cycle vertex = [vertex]

stronglyConnComp :: Eq vertex => (vertex -> [vertex] -> [vertex]) -> [Edge vertex] -> [vertex] -> [[vertex]]
-- stronglyConnComp :: [Edge Int] -> [Int] -> [[Int]]

stronglyConnComp f es vs
  = snd (span_tree (new_range reversed_edges)
                   ([],[])
                   ( snd (dfs f (new_range es) ([],[]) vs) )
        )
 where
   -- reversed_edges :: [Edge Int]
   reversed_edges = map swap es

   -- swap :: Edge Int -> Edge Int
   swap (x,y) = (y, x)

   -- new_range :: [Edge Int] -> Int -> [Int]
   new_range    []       w = []
   new_range ((x,y):xys) w
       = if x==w
         then (y : (new_range xys w))
         else (new_range xys w)

   {- span_tree :: (Int -> [Int])
		      -> ([Int], [[Int]])
		      -> [Int]
		      -> ([Int], [[Int]]) -}
   span_tree r (vs,ns) []   = (vs,ns)
   span_tree r (vs,ns) (x:xs)
       | x `elem` vs = span_tree r (vs,ns) xs
       | True = span_tree r (vs',(x:ns'):ns) xs
         where
           (vs',ns') = dfs f r (x:vs,[]) (r x)

dfs :: Eq v => (v -> [v] -> [v])
            -> (v -> [v])
            -> ([v], [v])
            -> [v]
            -> ([v], [v])
{-
dfs :: (Int -> [Int])
            -> ([Int], [Int])
            -> [Int]
            -> ([Int], [Int])
-}
dfs f r (vs,ns)   []   = (vs,ns)
dfs f r (vs,ns) (x:xs) | x `elem` vs = dfs f r (vs,ns) xs
-- SYMFUN: The following line makes use of symbolic function
                     | f x vs /= f x ns = dfs f r (f x vs, f x ns) xs
                     | True = dfs f r (vs',(x:ns')++ns) xs
                                   where
                                     (vs',ns') = dfs f r (x:vs,[]) (r x)
