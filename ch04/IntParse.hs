-- file: ch04/IntParse.hs

import Data.Char (digitToInt)

-- convert a STRING of numbers TO an INT eg "123" to 123
-- http://book.realworldhaskell.org/read/functional-programming.html#fp.loop
-- The code in C:
    -- int str_to_int(char *str) {
    --     int acc; /* accumulate the partial result */
    --     for (acc = 0; isdigit(*str); str++) {
    --         acc = (acc * 10) + (*str - '0');
    --     }
    --     return acc;
    -- }

-- And in Haskell:
str_to_int :: String -> Int
str_to_int string = loop 0 string
    where loop :: Int -> String -> Int
          loop acc []     = acc
          loop acc (x:xs) = loop acc' xs where acc' = (acc * 10) + digitToInt x


-- SQUARE every element in a list
-- http://book.realworldhaskell.org/read/functional-programming.html#id592265
-- In C:
    -- void square(double *out, const double *in, size_t length) {
    --     for (size_t i = 0; i < length; i++) {
    --         out[i] = in[i] * in[i];
    --     }
    -- }

-- And in Haskell:
square :: [Double] -> [Double]
square []     = []
square (x:xs) = x*x : square xs -- the : makes a list ie [x*x] ++ square xs

-- Or in Haskell using map (see Map.hs next)
square2 :: [Double] -> [Double]
square2 xs = map tmp_square xs where tmp_square x = x*x
