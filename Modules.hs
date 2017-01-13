addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z 
factorial :: Integer -> Integer  
factorial n = product [1..n]  
factInt :: Int -> Int  
factInt n = product [1..n] 

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  

bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
 | bmi <= 18.5 = "You're underweight, you emo, you!"  
 | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
 | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
 | otherwise   = "You're a whale, congratulations!"  

bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height  
 | weight/height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
 | weight/height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
 | weight/height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
 | otherwise   = "You're a whale, congratulations!"  
 
bmiTell3 :: (RealFloat a) => a -> a -> String  
bmiTell3 weight height  
 | bmi <= 18.5 = "You're underweight, you emo, you!"  
 | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
 | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
 | otherwise   = "You're a whale, congratulations!"  
 where bmi = weight / height ^ 2

bmiTell4 :: (RealFloat a) => a -> a -> String  
bmiTell4 weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
bmiTell5 :: (RealFloat a) => a -> a -> String
bmiTell5 w h
 | bmi <= sk = "under"
 | bmi <= no = "normal"
 | bmi <= fa = "fat"
 | otherwise = "whaleeeee"
 where bmi = w/h ^ 2
       (sk, no, fa) = (18.5, 25.0, 30.0)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
 let sideArea = 2 * pi * r * h
     topArea= pi * r ^ 25
 in sideArea + 2 * topArea

head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  

head2 :: [a] -> a  
head2 xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty list"
                                               [x] -> "a Singleton list"
                                               xs -> "a longer list" 

describeList2 :: [a] -> String  
describeList2 xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

maximum1 :: (Ord a) => [a] -> a  
maximum1 [] = error "maximum of empty list"  
maximum1 [x] = x  
maximum1 (x:xs) = max x (maximum' xs) 

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs 

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

repeat' :: a -> a
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  
