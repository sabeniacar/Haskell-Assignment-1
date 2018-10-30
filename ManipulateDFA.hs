module ManipulateDFA 
where

import DFA
import Data.Char (isDigit, isLower)
import Data.List (sort, nub, (\\))

{------------------------------------------------------------------------
    Selim Abeniacar
    The University of Melbourne
    September 2016
------------------------------------------------------------------------}

normaliseHelper :: DFA -> DFA
normaliseHelper ([],sym,t,s,ys) = ([],sym,t,s,ys)
normaliseHelper ((x:xs),sym,t,s,ys)
  = conc ([len],sym,t,s,ys) (normaliseHelper (xs,sym,trans,start,accept))                 
  where conc (ls,_,_,_,_) (xs,sym,t,s,ys) = (ls++xs,sym,t,s,ys)
        len = (length (x:xs))
        start = if x == s then len else s
        accept = [if a == x then len else a | a <- ys ]
        trans = [if (a == x && b == x )then ((len, c), len) 
                 else if a == x then ((len, c), b) 
                 else if b == x then ((a, c), len)
                 else ((a, c), b)| ((a, c), b) <- t]


--  Keep lists sorted and without duplicates.

tidy :: Ord a => [a] -> [a]
tidy xs
  = nub (sort xs)


--  Calculate the set of reachable states in a given DFA.

reachable :: DFA -> [State]
reachable (states, alphabet, delta, start_state, accept_states)
  = new
    where
      (old, new) = until stable explore ([], [start_state])
      explore (old_reach, cur_reach) = (cur_reach, expand cur_reach)
      expand reach = tidy (reach ++ successors reach)
      successors reach = [y | ((x,_),y) <- delta, x `elem` reach]
      stable (xs, ys) = xs == ys


--  Calculate the set of generating states in a given DFA.

generating :: DFA -> [State]
generating (_,_,[],_,_) = []
generating (xs,sym,t,s,ys)
  = if (stuck ys) == False then sort (nub ((search ys) ++ (generating (xs,sym,(removeDelta ys),s,(search ys))))) else []
  where search r = [ a | ((a, c), b) <- t, b `elem` r ]
        removeDelta r = [ ((a, c), b) | ((a, c), b) <- t, (b `elem` r) == False ]    
        stuck r =  (length [((a, c), b) | ((a, c), b) <- t, b `elem` r ]) == 0

--  Trim a DFA, that is, keep only reachable, generating states
--  (the start state should always be kept).  

trim :: DFA -> DFA
trim (xs,sym,t,s,ys)
  = (newStates, sym, newDelta, s, newAccept)
  where new = [y | y <- xs, y `elem` (reachable (xs,sym,t,s,ys)), y `elem` (generating (xs,sym,t,s,ys)) ]
        newStates = if s `elem` new then new else (s:new)
        newDelta = [ ((a, c), b) | ((a, c), b) <- t, a `elem` newStates, b `elem` newStates]
        newAccept = [y | y <- ys, y `elem` (reachable (xs,sym,t,s,ys)), y `elem` (generating (xs,sym,t,s,ys)) ]


-------------------------------------------------------------------------

--  Complete a DFA, that is, make all transitions explict.  For a DFA,
--  the transition function is always understood to be total.

complete :: DFA -> DFA
complete (xs,[],t,s,ys) = (xs,[],t,s,ys)
complete (xs,(cc:cs),t,s,ys)
  = if ifcomplete then (complete (xs,cs,t,s,ys)) else (complete (xs,cs,(t ++ addT),s,ys))
  where ifcomplete = (length xs) == (length (nub [a | ((a, c), b) <- t, c == cc]))
        addT = [((z, cc), z) | z <- xs, (z `elem` [a | ((a, c), b) <- t, z <- xs, c == cc]) == False]
-------------------------------------------------------------------------

--  Systematically replace the names of states in a DFA with 1..n.

normalise :: DFA -> DFA
normalise ([],sym,t,s,ys) = ([],sym,t,s,ys)
normalise (xs,sym,t,s,ys)
  = (normaliseHelper (states,sym,trans,start,accept))                 
  where len = (length xs)+1
        states = [y+len | y <- xs]
        trans = [((a+len, c), b+len)| ((a, c), b) <- t]
        start = s+len
        accept = [y+len |y <- ys]
-------------------------------------------------------------------------
  

--  To complete and then normalise a DFA:

full :: DFA -> DFA
full
  = normalise . complete


--  For a given DFA d, generate a DFA d' so that the languages of d
--  and d' are complementary.

complement :: DFA -> DFA
complement (xs,sym,t,s,ys)
  = (xs,sym,t,s,accept)            
  where accept = [x | x <- xs, (x `elem` ys) == False]

-------------------------------------------------------------------------

--  Given DFAs d1 and d', generate a DFA for the intersection of the
--  languages recognised by d1 and d2.

prod :: DFA -> DFA -> DFA
prod (xs,sym,t,s,ys) (gs,symbol,tt,ss,hs)
  = traverseStates states (newStates ,sym,[],start,acceptStates)        
  
  where stateNum = (maximum [(maximum xs),(maximum gs)])  
        states = [(a,b) | a <- xs, b <- gs ]
        newStates = [stateNum+1..(stateNum+(length states))] 
        oldToNew r old new  = if r == (head old) then (head new) else (oldToNew r (tail old) (tail new))
        newTrans (k,l) =  [ (((oldToNew (k,l) states newStates),c),(oldToNew (b,p) states newStates))
                             | c <- sym, ((a, m), b) <- t, ((v, n), p) <- tt, a == k, m == c, n == c, v == l] 
        accept =  [(a,b) | a <- ys, b <- hs ]
        acceptStates = [oldToNew (a,b) states newStates | (a,b) <- accept]
        start = head [(oldToNew (a,b) states newStates) | (a,b) <- states, a == s, b == ss]
        traverseStates st (xs,sym,t,s,ys) = if (length st) == 0 then (xs,sym,t,s,ys) else (traverseStates (tail st) (xs,sym, (t ++ (newTrans (head st))),s,ys))
-------------------------------------------------------------------------

--  Here is an example (trimmed) DFA; it recognises a*ab*c*

dex :: DFA 
dex 
  = ([0,1,2,3], "abc", t1, 0, [1,2,3])
    where 
      t1 = [ ((0,'a'), 1)
           , ((1,'a'), 1)
           , ((1,'b'), 2)
           , ((1,'c'), 3)
           , ((2,'b'), 2)
           , ((2,'c'), 3)
           , ((3,'c'), 3)
           ]

-------------------------------------------------------------------------

