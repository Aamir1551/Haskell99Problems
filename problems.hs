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
