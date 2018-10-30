module RunTM (
  TM
, run_TM
, use
, trace
) where

import Data.Char

{------------------------------------------------------------------------
    A Turing machine simulator written in Haskell

    Harald Sondergaard
    The University of Melbourne
    September 2016
------------------------------------------------------------------------}

--  Usage: 
--  
--  `use m' where m is a Turing machine 
--  (or rather a list that encodes the machine's transition function).
--  The function `use' will query for input to m, then print output,
--  provided simulation terminates.  The function will exit the read
--  loop when the user types `Quit'.

--  The function `trace' is similar to `use' and useful for testing.
--  Rather than printing just the final tape content, it prints the 
--  whole computation history.

--  An example of a Turing machine, tex, in the appropriate format, 
--  is given below.

-------------------------------------------------------------------------

--  The simulator simulates a two-way Turing machine, that is, the tape
--  is unbounded in both directions.  Initially the tape is blank.  
--  Any input is placed on the tape, and the tape head is positioned 
--  over the blank cell immediately to the left of input.

--  The transition function is represented as a list.  An element
--  ((p,s), (q,t,d)) reads: If, in state p, we read symbol s, replace
--  s by t, move the tape head according to d, and go to state q.
--  The Turing machine must move the tape head, so d can only indicate
--  left (L) or right (R).

data Dir = L | R
type TM = [ ((State, Symbol), (State, Symbol, Dir)) ]

--  States are assumed to be positive integers, with 1 being the start
--  state.  Symbols are characters, but the input alphabet is assumed 
--  to contain only digits and lower case letters.

type State = Int
type Symbol = Char
type Symbols = [Symbol]

--  The combination of the tape (contents) and the tape head position is
--  captured by a triple (left, mid, right) where `mid' is the symbol
--  under the tape head, `left' is the part of the tape to its left
--  (reversed), and `right' is the part of the tape to its right.

type Tape = (Symbols, Symbol, Symbols)

--  A configuration is a combination of state and tape 
--  (the tape itself keeps track of the position of the tape head):

type Configuration = (State, Tape)
type History = [Configuration]

--  We assume the convention that the TM starts in state 1, with the 
--  tape head reading a blank. The input begins immediately to the 
--  right of the tape head.

start_state :: Int
start_state = 1

blank_tape :: Symbols
blank_tape = repeat ' '

run_TM :: TM -> Symbols -> Configuration
run_TM m input
  = head history
    where
      history = run m (start_state, initial_tape)
      initial_tape = (blank_tape, ' ', input ++ blank_tape)

trace_TM :: TM -> Symbols -> History
trace_TM m input
  = reverse history
    where
      history = run m (start_state, initial_tape)
      initial_tape = (blank_tape, ' ', input ++ blank_tape)

run :: TM -> Configuration -> History
run m config
  = until final (step m) [config]
    where
      final ((state, (_,mid,_)) : _) = (state, mid) `notElem` map fst m

--  The function `step' does a single step of the simulation.  It
--  takes as input the TM's transition function, and the (reverse) 
--  computation history. This means it has the current configuration 
--  at the head of the history record. Given this configuration, it
--  looks at the transition function to see if there is a transition 
--  for the combination of state and symbol under the tape head, and
--  if so, it updates the configuration according to the transition
--  function.

step :: TM -> History -> History
step m history@((state, (left, mid, right)) : _)
  = case lookup (state, mid) m of
      Just (new_state, new_sym, L) ->
        (new_state, (tail left, head left, new_sym : right)) : history
      Just (new_state, new_sym, R) ->
        (new_state, (new_sym : left, head right, tail right)) : history
      Nothing ->
        error "step found no possible transition"

--  The function `tapeContents' is for printing the result of the tape 
--  at the end of a simulation.

tapeContents :: Configuration -> String
tapeContents (_, (left, mid, right))
  = reverse trimmedLeft ++ [mid] ++ trimmedRight ++ "\n"
                      ++ [' ' | c <- trimmedLeft] ++ "^"
    where
      trimmedLeft  = takeWhile (/= ' ') left
      trimmedRight = takeWhile (/= ' ') right

--  The function `use' takes a machine m and enters a dialogue with the 
--  user, asking for input to m, and printing the corresponding output,
--  if m terminates on the input.

use :: TM -> IO ()
use m
  = do
      putStr "Input: "
      input <- getLine
      if compliant input
        then do {putStrLn (tapeContents (run_TM m input)); use m}
        else if (head input) == 'Q' 
          then return ()
          else do
            putStrLn "Input must be digits and/or lower case letters"
            use m

--  The function `trace' is like `use', except it prints the TM's
--  computation history for the given input.

trace :: TM -> IO ()
trace m
  = do
      putStr "Input: "
      input <- getLine
      if compliant input
        then do 
          {mapM putStrLn (map tapeContents (trace_TM m input)); trace m}
        else if (head input) == 'Q' 
          then return ()
          else do
            putStrLn "Input must be digits and/or lower case letters"
            use m

--  The function `compliant' checks that input consists entirely of 
--  digits and lower case letters.

compliant :: Symbols -> Bool
compliant
  = all comply
    where
      comply c = isLower c || isDigit c 

-------------------------------------------------------------------------

--  Here is an example TM which converts a to b and vice versa:

tex :: TM
tex 
  = [ ((1,' '), (2,' ',R))
    , ((2,'a'), (2,'b',R))
    , ((2,'b'), (2,'a',R))
    , ((2,' '), (3,' ',L))
    , ((3,'a'), (3,'a',L))
    , ((3,'b'), (3,'b',L))
    ]

-------------------------------------------------------------------------

