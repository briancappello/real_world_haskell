#!/usr/bin/env runghc

-- InteractWith
-- runghc InteractWith.hs "lorem.txt" "output.txt"

-- This mini-program reads the input file (must be in the same/current dir?)
-- and splits it by lines. it then joins all the lines up with spaces and saves
-- the resulting wall of text to the specified output filename.

-- Optionally, mark this file as executable with ``chmod +x InteractWith.hs``
--   and run it with:
--     ./InteractWith.hs "input_file_name.txt" "output_file_name.txt"
-- Or, even better, compile it with:
--     ghc --make InteractWith
--   and run it with:
--     ./InteractWith "input_file_name.txt" "output_file_name.txt"

-- NOTE: the filename "double quotes" are required when calling this program!

-- THE CODE
-- all imports must go at the top of the file

-- from the System.Environment library, import the getArgs expression (function)
import System.Environment (getArgs)

-- from our SplitLines file, import the splitlines and join_string expressions
import SplitLines (splitlines, join_string)

interactWith the_function inputFile outputFile = do
    input <- readFile inputFile -- do "give input <- the result of readFile"
    -- and write to output_file the result of (the_function input_file_str)
    --   (this is the only place where 'my/the_function' actually get called)
    writeFile outputFile (the_function input)

-- set my_function (which ends up as 'the_function' above):
--   (my_function is the function we're going to use to bang on the input file)

-- by default, we can just leave the contents of the file intact by using:
-- my_function = id
-- id is a builtin: the identity function (it returns exactly what it's given)

-- instead, we'll use our own function (defined below) to process the input text
my_function = splitlines_to_str

-- input_file -> [Char] -> split -> [[Char]] -> join_string -> [Char] -> outfile
splitlines_to_str long_str = join_string " " (splitlines long_str) ++ "\n"

main = mainWith my_function
    where mainWith the_function = do -- the_function == my_function
          -- code after 'where ... do ...' must be indented to *at least* after
          --   the end of the 'where' statement!
          --   http://stackoverflow.com/questions/3223291/newbie-question-what-role-does-the-indentation-play-here-and-why-one-indent-do/3223332
          
          args <- getArgs -- do "give args <- the result of getArgs"
          case args of
               -- if two input args were given, call interactWith
               [input,output] -> interactWith the_function input output
               -- otherwise, exit with error
               _ -> error "Error:\nExactly two \"quoted\" filenames required."
