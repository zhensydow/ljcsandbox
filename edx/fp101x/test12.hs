last' [x] = x
last' (_:xs) = last' xs

last'' (_:xs) = last'' xs
last'' [x] = x

checklast = last' [1..5] == last'' [1..5]

foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldr'' f v (x:xs) = f x (foldr'' f v xs)
foldr'' _ v [] = v

checkfoldr = foldr' (+) 1 [1..5] == foldr'' (+) 1 [1..5]

init' [_] = []
init' (x:xs) = x : init' xs

init'' (x:xs) = x : init'' xs
init'' [_] = []

checkinit = init' [1..5] == init'' [1..5]

drop' 0 xs = xs
drop' n [] = []
drop' n (_ : xs) = drop' (n-1) xs

drop'' n (_ : xs) = drop'' (n-1) xs
drop'' n [] = []
drop'' 0 xs = xs

checkdrop = drop' 3 [1..5] == drop'' 3 [1..5]

checks = [checklast,checkfoldr,checkinit,checkdrop]
