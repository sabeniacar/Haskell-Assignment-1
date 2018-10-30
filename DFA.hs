module DFA (
  State
, Sym
, Input
, Trans
, DFA
, symbols
, check_DFA
) where

import Data.List (nub, sort, (\\))
import System.Exit

{------------------------------------------------------------------------
    A type definition for DFAs, and some functions that check 
    well-formedness of DFAs.

    Harald Sondergaard
    The University of Melbourne
    September 2013, revised September 2016
------------------------------------------------------------------------}

--  Usage:
--
--  `check_DFA d' where d is a DFA.
--

type State = Int
type Sym   = Char
type Input = [Sym]
type Trans = ((State,Sym),State)
type DFA   = ([State], [Sym], [Trans], State, [State])

states_of :: [Trans] -> [State]
states_of delta
  = concatMap states_of_tran delta
    where
      states_of_tran ((st,_),st') = [st,st']

non_dets :: [Trans] -> [(State,Sym)]
non_dets delta
  = domain \\ nub domain
    where
      domain = map fst delta

symbols :: [Trans] -> [Sym]
symbols delta
  = nub (sort syms)
    where
      domain = map fst delta
      syms = map snd domain

check_DFA :: DFA -> IO ()
check_DFA (states, alphabet, delta, start, accept) 
  = do
      let nondets = non_dets delta
      let used_states = start : (accept ++ states_of delta)
      let sorted_states = nub (sort states)
      let sorted_used_states = nub (sort used_states)
      let non_declared_states = sorted_used_states \\ sorted_states
      check_valid states
      check_declared non_declared_states
      check_nondets nondets
      check_symbols (symbols delta) alphabet

check_valid :: [State] -> IO ()
check_valid states
  = do
      let illegal_states = filter (<0) states
      if null illegal_states then
        return ()
      else
        do
          putStrLn "ERROR:"
          putStrLn ("Illegal states: " ++ show illegal_states)
          exitWith (ExitFailure 1)

check_declared :: [State] -> IO ()
check_declared states
  = if null states then
      return ()
    else
      do
        putStrLn "ERROR:"
        putStrLn ("States used but not declared: " ++ show states)
        exitWith (ExitFailure 1)

check_nondets :: [(State,Sym)] -> IO ()
check_nondets entries
  = if null entries then
      return ()
    else
      do
        putStrLn "ERROR:"
        putStrLn ("Non-deterministic entries: " ++ show entries)
        exitWith (ExitFailure 1)

check_symbols :: [Sym] -> [Sym] -> IO ()
check_symbols syms alphabet
  = do
      let bad_syms = syms \\ alphabet
      if null bad_syms then
        return ()
      else
        do
          putStrLn "ERROR:"
          putStr ("Symbol used, not in alphabet: ")
          putStrLn (show (head bad_syms))
          exitWith (ExitFailure 1)

