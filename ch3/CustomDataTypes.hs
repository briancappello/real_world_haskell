-- file: ch3/CustomDataTypes.hs aka BookStore.hs

-- TYPE/VALUE CONSTRUCTORs: defining custom data types
-- data: the ``data`` keyword is used to declare a new type
-- BookInfo: TYPE constructor (name of the new type)
-- Book: VALUE CONSTRUCTOR. must have first letter capitolized. (aka data
--        constructor; call to create a value of the BookInfo type)
-- Int String [String]: the components of the new type (each a field in struct,
--   and the "function signature" of the value constructor) eg:
--  to create a new BookInfo, call ``Book 0 "title" ["author0"]``

data BookInfo = Book Int String [String]
    deriving (Show) -- needed for ghci pretty printing

-- DECONSTRUCTION / TUPLE PATTERN MATCHING
--  to access BookInfo's fields, first we need the following expressions:
--  the (Book ...) single argument is sort-of like type-verified tuple unpacking
book_id      (Book id title authors) = id -- we can reduce confusion* like so:
book_title   (Book _  title _      ) = title -- the underscore is a wildcard
book_authors (Book _  _     authors) = authors -- nothing gets bound using _
-- then, each of these acts as an accessor to any BookInfo object:
-- this_id = book_id a_BookInfo
-- this_title = book_title a_BookInfo
-- this_authors = book_authors a_BookInfo
-- (*) There is an even more condensed form further below, under RECORD SYNTAX

-- CONSTRUCTOR NAMING CONVENTION (optional)
-- the type constructor name (BookInfo) and the value constructor name (Book)
--   are typically made to be the same name, but were not above for clarity
-- Magazine has the same signature as Book, but it is a different type
data Magazine = Magazine Int String [String]
    deriving (Show)

-- INITIALIZATION
-- create a new BookInfo
-- must use ``let my_book_info = ...`` when in ghci, but can't here
my_book_info = Book 0 "Hello World" ["Brian Cappello"]
-- remember that in ghci, if you simply do ``Book 0 "title" ["authors"]``,
-- then you can access the most recently created Book with ghci's ``it``

-- TYPE SYNONYMS
-- instead of just using generic type names for the expression signature, 
--  we can instead use the ``type`` keyword to assign names to generic types:
type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody

-- a BookRecord is a named tuple of (Book, BookReview) but cannot be created
type BookRecord = (BookInfo, BookReview)
-- so, testing (Book, BookReview) == BookRecord will work but one can't 
--  explicitly create a BookRecord

-- ALGEBRAIC DATA TYPES
--  any type that can have more than one value constructor, separated by ``|``
--  eg Bool can be implemented as ``data Bool = True | False`` (Read | as "or")
type CardNumber = String
type CardHolder = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                                      -- accept CC: String String [String]
                 | CashOnDelivery     -- or cash: (no params)
                 | Invoice CustomerID -- or billable to an invoice: Int
                   deriving (Show)
-- Three ways to create a BillingInfo type:
--   billing = CreditCard "CCnumber" "Holders Name" ["their", "address", "USA"]
--   or billing = CashOnDelivery
--   or billing = Invoice customersID

-- DATA TYPES vs TUPLES
-- Generally speaking, except for limited local-scope instances, one should 
--   generally prefer explicitly defining new data types over using tuples eg:
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show) -- the Eq allows for Equality comparisons
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)
--   is better than:
-- type Cartesian2D = (Double, Double)
-- type Polar2D = (Double, Double)
--   because, obviously, having the same definitions, and without a way to
--    create specific Cart/Polar2Ds, trouble arises doing type checks.

-- Enumeration: Unlike C, these are not Ints
data Color = Red
            | Orange
            | Yellow
            | Green
            | Blue
            | Indigo
            | Violet
              deriving (Eq, Show)

-- examples of deconstruction (type-verified tuple unpacking) functions
third (a, b, c) = c
-- let t = (2, 3, 4)
-- third t ==> returns 4
complicated (True, a, x:xs, 5) = (a, xs)
-- let t = (True, 6, [1,2,3], 5)
-- complicated t ==> returns (6, [2,3])


-- RECORD SYNTAX
-- the better way to write data types, constructors and accessors

data Customer = Customer
--    accessor_fn_name :: Type
    { customerID       :: CustomerID
    , customerName     :: String
    , customerAddress  :: Address
    } deriving (Show)

-- this is essentially equivalent to the following commented block of code:

-- data Customer = Customer Int String [String]
--                 deriving (Show)

-- customerID :: Customer -> Int
-- customerID (Customer id _ _) = id

-- customerName :: Customer -> String
-- customerName (Customer _ name _) = name

-- customerAddress :: Customer -> [String]
-- customerAddress (Customer _ _ address) = address

-- Keyword Arguments (unordered) are also supported:
customer2 = Customer
    { customerID = 2
    , customerAddress = ["their address"]
    , customerName = "John Q. Citizen"
    }

-- PARAMETERIZED TYPES
data ATypeWithNonePossible a = HasParam a
                             | HasNoParam
                               deriving (Show)
-- To make ATypeWithNonePossible, use ``HasParam some_value`` or ``HasNoParam``
-- The real implementation is called Maybe and looks like this:
-- data Maybe a = Nothing | Just a

-- RECURSIVE TYPES
-- our own version of a list (they're implemented recursively in haskell too)
data MyList a = Cons a (MyList a)
              | Nil
                deriving (Show)

-- to convert from a normal [list] into MyList, use from_list:
from_list (x:xs) = Cons x (from_list xs)
from_list []     = Nil

-- and to convert back, use to_list:
to_list (Cons x xs) = (x:(to_list xs))
to_list Nil = []

-- here's a binary tree (type Tree value constructor Node)
data Tree a = Node a (Tree a) (Tree a)
            | Leaf -- aka Null/Void, but we need to define that as a type
              deriving (Show)
-- this could have been written like this:
-- data Tree a = Tree a (Tree a) (Tree a)
--             | Empty
--               deriving (Show)

-- a super-simple tree instance example:
simpleTree = Node "parent" (Node "left" Leaf Leaf) (Node "right" Leaf Leaf)

-- ERROR
-- here we try to return the second value of a list, but only if there are
--   enough elements
another_second :: [a] -> a
another_second xs = if null (tail xs)
                    then error "list too short"
                    else head (tail xs)

-- error, however, completely terminates execution, so instead we should use
--   Maybe when errors are recoverable:
safe_second :: [a] -> Maybe a
safe_second [] = Nothing
safe_second xs = if null (tail xs)
                 then Nothing
                 else Just (head (tail xs))
-- remember, this is just one function, with two cases defined: an empty list 
--   followed by the case of handling a populated list
-- this is the same thing, shortened by using (_:x:_)
tidy_second :: [a] -> Maybe a
tidy_second (_:x:_) = Just x
tidy_second _       = Nothing
-- let's break down the (_:x:_)
--  the parenthesis are just grouping everything into a single parameter
--  the _ underscores are wildcards, and the ``:`` colons are list constructors
-- SO, _:x:_ assigns x to the second element of a len >= 2 list
--     otherwise if the input to tidy_second doesn't match that, return Nothing

-- LOCAL VARIABLES
-- To assign a new local variable inside a function, use ``let ... in ...``
lend amount balance = let reserve = 100 -- multiple variables separated by '\n'
                          new_balance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just new_balance
-- this translates roughly to:
-- reserve = 100
-- new_balance = balance - reserve
-- if balance < reserve:
--     return Nothing
-- return Just new_balance
-- HOWEVER, it's important to note that reserve and new_balance are only
--   expression bindings; they only get evaluated on upon needed execution

-- SHADOWING local variables (aka nested let blocks)\
-- enable GHC warnings for this with ``--fwarn-name-shadowing``
shadow_three = let a = 1
               in let b = 2
                  in a + b
-- or, even uglier, the a/b names can be the same (even tho they ARE different)
shadow_outer = let x = 1
               in ((let x = "foo" in x), x)
-- the inner 'x' is shadowing (hiding) the outer 'x' (legal; not recommended)
-- this "returns" the tuple ("foo", 1)

-- even more shadowing; this time unnecessarily:
quux a = let a = "foo"
        in a ++ "eek"
-- here, we're overriding the input, always setting it to "foo" and
-- always returning "fooeek"
-- the same thing; no shadowing:
-- quux a = let b = "foo" in b ++ "eek"

-- the WHERE clause (local variables cont)
-- whitespace matters here! the "where" is like let, applied post expression
lend2 amount balance = if amount < reserve * 0.5
                       then Just new_balance
                       else Nothing
   where reserve     = 100
         new_balance = balance - amount
-- although this seems backwards at first, it lets the dev place the most
--  import expression first and foremost, followed by the inner 'support'
--  expressions needed to partial calculations.

-- LOCAL FUNCTIONS
pluralize :: String -> [Int] -> [String]
pluralize word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"




