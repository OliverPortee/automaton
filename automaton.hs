{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Set (Set, fromList, member, disjoint, empty, union, singleton)
import Data.List (foldl')

------------------------- TODO -------------------------
-- DFATrans/NFATrans als Relation anstatt Funktion?
-- NFA mit e-Übergängen; NFA aus NFA mit e-Übergängen
-- Potenzmengen-Konstruktion :: NFA -> DFA
-- Vereinigung, Schnitt, Komplement, Konkatenation, Kleene-Stern von NFAs
-- Visualisierung von Automaten mit Haskell Diagrams

------------------------- Automaton -------------------------
type Alphabet = Set Char

class (Ord s) => Automaton a s where
    statesOf :: a -> Set s
    alphabetOf :: a -> Alphabet
    acceptsWord :: a -> String -> Bool


------------------------- DFA -------------------------
data DFA state = DFA { statesOfDfa :: Set state
                     , alphabetOfDfa :: Alphabet
                     , transOfDfa :: state -> Char -> Maybe state
                     , startOfDfa :: state
                     , acceptStatesOfDfa :: Set state
                     }

instance (Ord state) => Automaton (DFA state) state where
    statesOf = statesOfDfa
    alphabetOf = alphabetOfDfa
    acceptsWord dfa word = case foldl' next start word of
                        Just state -> member state (acceptStatesOfDfa dfa)
                        Nothing -> False
        where start = Just (startOfDfa dfa)
              next maybeState char = maybeState >>= (flip $ transOfDfa dfa) char


------------------------- NFA -------------------------
data NFA state = NFA { statesOfNfa :: Set state
                     , alphabetOfNfa :: Alphabet
                     , transOfNfa :: state -> Char -> Set state
                     , startsOfNfa :: Set state
                     , acceptStatesOfNfa :: Set state
                     }

instance (Ord state) => Automaton (NFA state) state where
    statesOf = statesOfNfa
    alphabetOf = alphabetOfNfa
    acceptsWord nfa word = not $ disjoint (foldl' next start word) (acceptStatesOfNfa nfa)
        where start = startsOfNfa nfa
              next states char = foldl' (next' char) empty states
              next' char states state = union states ((transOfNfa nfa) state char)


------------------------- Conversions -------------------------

--dfaToNfa :: DFA -> NFA
--dfaToNfa (DFA states alphabet next start accepts) = NFA states alphabet next' start' accepts
    --where start' = singleton start
          --next' state char = case next state char of
                                --Just st -> singleton st
                                --Nothing -> empty
 
 

--nfaToDfa :: NFA -> DFA
--nfaToDfa (NFA states alphabet next start accepts) = DFA states' alphabet next' start' accepts'

------------------------- examples -------------------------
dfa1 :: DFA Int
dfa1 = DFA states alphabet next start acceptStates
states = fromList [1, 2]
alphabet = fromList "ab"
next 1 'a' = Just 2
next 2 'b' = Just 1
next _  _  = Nothing
start = 1
acceptStates = fromList [1]


main = print $ dfa1 `acceptsWord` "abab"


