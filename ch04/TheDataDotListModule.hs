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

    -- PARTIAL and TOTAL FUNCTIONS
    --   partial: only returns valid values for a subset of all valid inputs
    --   total: always returns valid values for all valid inputs

an_example :: [Char] -> Char
an_example xs = if not (null xs)
                   then head xs
                   else 'z' -- that's a horrible return value; just an example

another_example :: [Char] -> Char
another_example (x:_) = x
another_example []    = 'z' -- same note as above

-- the full smart_example is apparently covered later, in the chapter for Monads
smart_example :: [a] -> Maybe a
smart_example (x:_) = Just x
smart_example []    = Nothing

-- it goes something like this:
safe_from_empty :: ([a] -> b) => [a] -> Maybe b
safe_from_empty fn list
    | null list = Nothing
    | otherwise = Just $ fn list

safe_tail = safe_from_empty tail
safe_init = safe_from_empty init


-- MORE SIMPLE LIST MANIPULATIONS

-- append (++)
append_example = [1] ++ [2,3] -- [1,2,3]

-- concat: make a single list from a list of lists
concat_example = concat [[1,2], [3,4], [5,6]] -- [1,2,3,4,5,6]

-- reverse: reverses the order of a list
reverse_example = reverse "hello" -- "olleh"

-- AND & OR: for lists with Bool type contents, these walk over the list:
-- and
and_is_true = and []
and_is_also_true = and [True, True]
and_is_false = and [True, False, True]

-- or
or_is_true = or [True]
or_is_also_true = or [True, False, False]
or_is_false = or []


