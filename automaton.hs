
import Data.Set (Set, fromList, member, disjoint, empty, union, singleton)
import Control.Exception (assert)
import Data.List (foldl')

------------------------- TODO -------------------------
-- DFATrans/NFATrans als Relation anstatt Funktion?
-- NFA mit e-Übergängen; NFA aus NFA mit e-Übergängen
-- Potenzmengen-Konstruktion :: NFA -> DFA
-- Vereinigung, Schnitt, Komplement, Konkatenation, Kleene-Stern von NFAs
-- Visualisierung von Automaten mit Haskell Diagrams

------------------------- Automaton -------------------------
class Automaton a where
    accepts :: a -> String -> Bool

type State = Int
type States = Set State
type Alphabet = Set Char
type StartState = State
type StartStates = Set State
type AcceptStates = States

------------------------- DFA -------------------------
type DFATrans = State -> Char -> Maybe State

data DFA = DFA { dfaStates :: States
               , dfaAlphabet :: Alphabet
               , dfaNext :: DFATrans
               , dfaStart :: StartState
               , dfaAcceptStates :: AcceptStates
               }

instance Automaton DFA where
    accepts dfa word = case foldl' next start word of
                        Just state -> member state (dfaAcceptStates dfa)
                        Nothing -> False
        where start = Just (dfaStart dfa)
              next :: Maybe State -> Char -> Maybe State
              next maybeState char = maybeState >>= (flip $ dfaNext dfa) char
 

------------------------- NFA -------------------------
type NFATrans = State -> Char -> States

data NFA = NFA { nfaStates :: States
               , nfaAlphabet :: Alphabet
               , nfaNext :: NFATrans
               , nfaStarts :: StartStates
               , nfaAcceptStates :: AcceptStates
               }

instance Automaton NFA where
    accepts nfa word = not $ disjoint (foldl' next start word) (nfaAcceptStates nfa)
        where start = nfaStarts nfa
              next :: States -> Char -> States
              next states char = foldl' (next' char) empty states
              next' :: Char -> States -> State -> States
              next' char states state = union states ((nfaNext nfa) state char)


------------------------- Conversions -------------------------
dfaToNfa :: DFA -> NFA
dfaToNfa (DFA states alphabet next start accepts) = NFA states alphabet next' start' accepts
    where start' = singleton start
          next' state char = case next state char of
                                Just st -> singleton st
                                Nothing -> empty


------------------------- examples -------------------------
dfa1 :: DFA
dfa1 = DFA states alphabet next start acceptStates
states = fromList [1, 2]
alphabet = fromList "ab"
next 1 'a' = Just 2
next 2 'b' = Just 1
next _  _  = Nothing
start = 1
acceptStates = fromList [1]


main = print $ dfa1 `accepts` "abab"


