module POV  (fromPOV, tracePathBetween) where
import Data.Maybe (listToMaybe)
import Data.List (inits, tails)
import Data.Tree (Tree(Node))
type Path a = [Tree a]

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV goal tree = inverse <$> findPath goal tree
  where inverse ((Node current children) : []) = Node current children
        inverse ((Node current children) : ls) = Node current $ (inverse ls) : children 

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = fmap extractValue <$> path
  where extractValue (Node v _) = v
        path = fromPOV to tree >>= findPath from

findPath :: Eq a => a -> Tree a -> Maybe (Path a)
findPath goal tree = listToMaybe $ go [tree]
  where
    selectFrom ls = zip ls $ zipWith (++) (inits ls) (tail $ tails ls)
    go path@((Node current children): rest)
      | current == goal = return path
      | null children   = mempty
      | otherwise       = selectFrom children >>= follow
      where 
        follow (selected, children') = go $ selected : Node current children' : rest
