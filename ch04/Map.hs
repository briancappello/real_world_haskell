-- file: ch04/Map.hs

import Data.Char (toUpper, toLower)

-- MAP
-- simplifies the super-common pattern of applying the same function to every
--  element of a list eg:

-- Example: Convert a string to UPPERCASE
-- http://book.realworldhaskell.org/read/functional-programming.html#comments_x_G8
-- In C:
    -- #include <ctype.h>
    -- char *uppercase(const char *in) {
    --     char *out = strdup(in);
    --     if (out != NULL) {
    --         for (size_t i = 0; out[i] != '\0'; i++) {
    --             out[i] = toupper(out[i]);
    --         }
    --     }
    --     return out;
    -- }

-- And in Haskell:
to_upper :: String -> String
to_upper ""     = []
to_upper (x:xs) = toUpper x : to_upper xs

-- And in Haskell using map:
to_upper_map :: String -> String
to_upper_map str = map toUpper str

-- this is map:
my_map :: (a -> b) -> [a] -> [b]
my_map _  []     = []
my_map fn (x:xs) = fn x : my_map fn xs

-- using our own map now:
my_to_upper_map :: String -> String
my_to_upper_map str = my_map toUpper str

my_to_lower :: String -> String
my_to_lower str = my_map toLower str

negatives :: [Integer]
negatives = map negate [1,2,3] -- [-1,-2,-3]
