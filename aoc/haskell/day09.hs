import Prelude hiding (round)
import qualified Data.Map as Map

data Deque a = Deque [a] [a]
  deriving (Show)

fromList :: [a] -> Deque a
fromList = Deque []

rotate :: Integer -> Deque a -> Deque a
rotate 0 deque = deque
rotate _ (Deque [] []) = Deque [] []
rotate 1 (Deque x (y:xsb)) = Deque (x ++ [y]) xsb
rotate 1 (Deque a []) = Deque (init a) [last a]
rotate (-1) (Deque [] b) = Deque [] ((last b):(init b))
rotate (-1) (Deque a b) = Deque (init a) ((last a):b)
rotate n deque = if n > 0
  then rotate 1 (rotate (n - 1) deque)
  else rotate (-1) (rotate (n + 1) deque)
  
pop :: Deque a -> Deque a
pop (Deque [] []) = error "Empty deque"
pop (Deque a (x:xs)) = Deque a xs
pop (Deque a []) = Deque (init a) []

push :: a -> Deque a -> Deque a
push a (Deque l r) = Deque l (a:r)

top :: Deque a -> a
top (Deque [] []) = error "Empty deque"
top (Deque _ (x:_)) = x
top (Deque a []) = last a

round :: Integer -> Integer -> Integer -> (Deque Integer, Map.Map Integer Integer) -> (Deque Integer, Map.Map Integer Integer)
round players lastMarble r (deque, billboard)
  | r > lastMarble = (deque, billboard)
  | r `mod` 23 == 0 = let tmp = rotate 7 deque
                          score = top tmp
                          newDeque = rotate (-1) (pop tmp)
                          newBillboard = Map.insertWith (+) (r `mod` players) (r + score) billboard
                      in round players lastMarble (r + 1) (newDeque, newBillboard)
  | otherwise = round players lastMarble (r + 1) ((push r (rotate (-1) deque)), billboard)

solve :: Integer -> Integer -> Integer
solve players lastMarble = let (_, billboard) = round players lastMarble 1 (Deque [] [0], Map.empty)
                           in Map.fold (max) 0 billboard

main :: IO()
main = print (solve 466 71436)
