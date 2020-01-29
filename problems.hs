{-
doubleMe x = x + x
doubleUs x y = x**2 + y**2


doubleSmallNumber x = if x>100
    then x
    else x*2


x = 100

--joinLists :: [a] -> [a] -> [a]

{-joinLists (x:xs) b = x : joinLists xs b
joinLists [] b = b-}


getNitem :: (Num n, Eq n, Ord n) => [a] -> n -> Maybe a

getNitem (x:xs) 0 = Just x
getNitem (x:xs) n = if n>=1 
    then getNitem xs (n-1)
    else Nothing
getNitem [] n = Nothing

------------------------------------------------------


data Shape m = Circle m | Rectangle Bool m deriving Show --Rectangle -- try it with rectable as well - so put 2 different vlaue constructors

instance Functor Shape where  
    fmap f (Rircle m) = Circle (f m)
    fmap f ((Rectangle Bool) m) = ( (Rectangle Bool) (f m))

main = do putStrLn (show (fmap (*2) (Circle 10)))
-}

{-
grid :: (Num n) => [[n]]
grid = [ [0,0,0,0,0] | x<- [1..5] ]
findCombos :: (Num a) => [[a]] -> Int -> Int -> [[a]]
findCombos [] _ _ = error "Empty grid"
findCombos a 0 j = [ (take (j) (a !! 0 )) ++ [1] ++  (drop (j+1) ((a !! 0 )))] ++  (drop 1 a)
findCombos a i 0 = (take (i) a) ++ [([1] ++ tail (a!!(i)))] ++ drop (i+1) a
findCombos a i j = (take i a) ++ [( (take j (a!!i)) ++ [( ((findCombos a (i-1) (j)) !!(i-1) !! (j))) + ( (findCombos a i (j-1)) !! i !! (j-1) )] ++ drop (j+1) (a!!i) )] ++ (drop (i+1) a)

s = [[ ((findCombos grid i j) !! i !! j) | i<-[0..4] ] | j <- [0..4]  ]




findCombos :: (Int, Int) -> Int 
findCombos (_, 0) = 1
findCombos (0, _) = 1
findCombos (a, b) = (grid !! (a-1) !! b)  + (grid !! a !! (b-1))
grid = [[ findCombos (i, j) | i<- [0..100] ] | j<- [0..100]]
-}

bns :: (Ord a) => [a] -> ([a], [a])
bns l@(x:_) = ([c| c<-l, c>x], [c| c<-l, c<=x])
bns [] = ([], []) 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [a] = [a]
quicksort l@(x:_) = (quicksort (tail s)) ++ [x] ++ (quicksort b)
    where (b, s) = bns l

