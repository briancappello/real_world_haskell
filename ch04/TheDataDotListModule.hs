-- file: ch4/TheDataDotListModule.hs

-- import all of Data.List
import Data.List

-- to import specific expressions within it:
-- import Data.List (func1, func2, funcN)


-- BASIC LIST MANIPULATION

-- length: how many elements in a list
zero :: Int
one  :: Int
two  :: Int
zero = length []
one = length [0]
two = length "hi"

-- null: determines if list is empty
is_null  :: Bool
not_null :: Bool
is_null = null []
not_null = null "hi"


-- HEAD, TAIL, LAST and INIT
--    NOTE: head [], tail [], last [], and init [] all raise exceptions

-- head: the first element of a list
list_head :: Char
list_head = head "a list"  -- 'a'

-- tail: everything but the head
list_tail :: [Char]
list_tail = tail "a list"  -- " list"

-- last: the last element of a list
list_last :: Char
list_last = last "a list"  -- 't'

-- init: everything but the last
list_init :: [Char]
list_init  = init "a list" -- "a lis"


-- CHECK for EMPTY LISTS
an_example :: [Char] -> Char
an_example xs = if not (null xs)
                   then head xs
                   else 'z' -- that's a horrible return value; just an example

another_example :: [Char] -> Char
another_example (x:_) = x
another_example []    = 'z' -- same note as above

smart_example :: [a] -> Maybe a
smart_example (x:_) = Just x
smart_example []    = Nothing



