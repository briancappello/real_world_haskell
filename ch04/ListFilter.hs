-- file: ch04/ListFilter.hs

import Data.Char (ord)

-- shiftLeft, bitwise-AND, bitwiseOR respectively
import Data.Bits (shiftL, (.&.), (.|.))

odd_list :: [Int] -> [Int]
odd_list (x:xs) | odd x     = x : odd_list xs
                | otherwise = odd_list xs
odd_list []                 = []

all_odds :: [Int]
all_odds = odd_list [1,1,2,3,5,8,13,21] -- [1,1,3,5,13,21]

-- filter on_condition from_this_list
same_odds :: [Int]
same_odds = filter odd [1,1,2,3,5,8,13,21] -- [1,1,3,5,13,21]
