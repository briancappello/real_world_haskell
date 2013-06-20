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
    | otherwise = Just $ fn list -- $ opens a ( and closes it at the line's end

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


-- WORKING WITH SUBLISTS
-- take: take n_elements from_the_beginning_of_this_list
take_six = take 6 "foobar is a favorite today" -- "foobar"
take_still_works = take 2 [] -- [] (overflows get silently ignored)

-- drop: drop n_elements off_the_beginning_of_this_list
still_fubar = drop 3 "hi foobar" -- "foobar"
drop_still_works = drop 3 [] -- [] (overflows get silently ignored)

-- splitAt an_index in_this_list: returns the tuple pair (take idx, drop idx)
foo_and_bar = splitAt 3 "foobar" -- ("foo", "bar")

-- takeWhile a_condition from_this_list: takes indexes from list while condition
three_elements = takeWhile odd [1,3,5,6,7] -- [1,3,5]

-- dropWhile a_condition from_this_list: drops indexes from list while condition
fifth_element = dropWhile even [2,4,6,8,10,5,4,3,2,1] -- [5,4,3,2,1]

-- break: partitions a list into a pair, splitting by a boolean expression eg:
isfive :: (Eq a, Num a) => a -> Bool
isfive item = (item == 5)

-- break by_this_condition this_list_into_a_pair
two_four_five_six :: ([Integer], [Integer])
two_four_five_six = break isfive [2,4,5,6] -- ([2,4], [5,6])

-- span break_when_condition_is_false this_list_into_a_pair
two_four_six = span even [2,4,6,5,7,9] -- ([2,4,6], [5,7,9])


-- SEARCHING LIISTS
-- elem: indicates whether a value is present in a list
two_is_true = 2 `elem` [1,2,3] -- True
no_one_found = elem 1 [2,3,4]  -- False

-- notElem: true if a value is NOT in a list
three_wasnt_there = 3 `notElem` [1,2,4] -- True
but_four_was = notElem 4 [1,2,4]        -- False

-- filter: get every element where a condition evaluates to true
-- filter on_this_condition every_matching_element_in_this_list
odd_three = filter odd [1,2,3,4,5,6] -- [1,3,5]

-- isPrefixOf, isInfixOf, isSuffixOf (see InfixOperatorFunctions.hs)
startswith = "start" `isPrefixOf` "starting my day"                 -- >=> True
infix_ = "needle" `isInfixOf` "haystack full of needles"            -- >=> True
suffix = "end" `isSuffixOf` "the world isn't soon coming to an end" -- >=> True


-- ZIPPING LISTS
-- zip this_list with_this_one
paired_up = zip [1,2,3] "abc" -- [(1,'a'), (2, 'b'), (3, 'c')]

-- zipWith this_function this_list and_this_one
added_pairs = zipWith (+) [1,2,3] [3,4,5] -- [4,6,8]

-- to get triplets, 4-tuples, 5-tuples, 6-tuples or 7-tuples, use:
--        zip3,     zip4,     zip5,     zip6,       zip7
--        zipWith3, zipWith4, zipWith5, zipWith6,   zipWith7


-- LINES and UNLINES
-- lines split_this_string_by_unix_newline_characters
foo_bar_pair = lines "foo\nbar" -- ("foo", "bar")

-- unlines join_this_list_by_newline_and_append_an_extra_newline
foo_bar_line = unlines foo_bar_pair -- "foo\nbar\n"


-- WORDS and UNWORDS
-- words split_this_string_by_whitespace_chunks
a_list = words "the \r quick \t  brown\n \nfox" -- ["the","quick","brown","fox"]

-- unwords join_this_list_of_strings_with_spaces
a_readable_list = unwords a_list -- "the quick brown fox"

