-- file: ch4/SplitLines.hs

module SplitLines (splitlines, join_string) where

-- break partitions a list into two parts, splitting by a boolean expression eg:
isfive :: (Eq a, Num a) => a -> Bool
isfive item = (item == 5)

-- break returns a pair (a 2 element tuple)
two_four_five_six :: ([Integer], [Integer])
two_four_five_six = break isfive [2,4,5,6] -- >=> ([2,4], [5,6])

-- now let's write a cross-platform splitlines function
is_line_sep :: Char -> Bool
is_line_sep char = ( char == '\r' ||
                     char == '\n' )

splitlines          :: [Char] -> [[Char]]
splitlines ""       = [] -- "" is strongly-typed-sugar for []
splitlines full_str = line:case rest of
                                ""              -> []
                                ('\r':'\n':rst) -> splitlines rst -- () optional
                                ('\r':rst)      -> splitlines rst -- () optional
                                ('\n':rst)      -> splitlines rst -- () optional
                            where (line, rest) = break is_line_sep full_str

-- from chapter 3
-- join_string only works with list (eg [Char]) data-type delimeters, not Char
join_string              :: [a] -> [[a]] -> [a]
join_string delim []     = []
join_string delim [x]    = x  -- [x] is sugar for (x:[])
join_string delim (x:xs) = x ++ delim ++ join_string delim xs
