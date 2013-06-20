-- ch04/InfixOperatorFunctions.hs

import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- To use these declarations interactively from within ghci:
--   :load InfixOperatorFunctions.hs
-- Or to only load the Data.List module:
--   :module +Data.List    -- can also be shortened using :m +Data.List

-- Haskell allows one to define their own functions that optionally take their
--  arguments on the function's left/right sides eg:
a `plus` b = a + b
c = 2 `plus` 5 -- >=> c == 5

-- THe key is in the `backticks.`
-- `Backticks` must also be used when *calling* the function in infix notation.

-- Data types can also be defined in infix notation:
data a `Pair` b = a `Pair` b
                  deriving (Show)

-- Infix functions, however, can also be used in regular prefix form:
foo = Pair 1 2
bar = 1 `Pair` 2 -- again, note the use of `backticks` for infix form

-- This applies to the math operators too:
result = (*) 5 100 -- >=> 500 (parenthesis reqd for prefix, no backticks for infix)

-- Introducing the elem expression (function)
-- Imported by default. Indicates whether a value is present in a list:
letter_c = elem 'c' "a string with c in it" -- >=> True
-- or more clearly used in infix form:
letter_d = 'd' `elem` "a string with c in it" -- >=> False

-- list.startswith("str"), "str" in list, and list.endswith("str"):
startswith = "start" `isPrefixOf` "starting my day"                 -- >=> True
infix_ = "needle" `isInfixOf` "haystack full of needles"            -- >=> True
suffix = "end" `isSuffixOf` "the world isn't soon coming to an end" -- >=> True
