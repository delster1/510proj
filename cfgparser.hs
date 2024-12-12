-- setting up basic types, in my head l

-- Data structure for the automaton with stack

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- data type to create transitions
data Transition = Transition
  { fromState :: String
  , inputSymbol :: String
  , stackTop :: String
  , toState :: String
  , stackOps :: [String] 
} deriving (Show, Eq)

-- where we store transitions
transitionsList :: [Transition]
transitionsList =
  [ Transition "q_0" "lambda" "S" "q_0" ["O", "F"]
  , Transition "q_0" "lambda" "S" "q_0" ["O", "C", "O", "F"]
  , Transition "q_0" "just put the O in the bag bro" "F" "q_0" []
  , Transition "q_0" "pause" "F" "q_0" []
  , Transition "q_0" "lambda" "O" "q_0" ["DO", "N", "ohio"]
  ]

-- turns transitions list into dictionary w/ key [state, symbol, stacktopo] val: [nextState, stack symbols]
buildTransitionMap :: [Transition] -> Map.Map (String, String, String) (String, [String])
buildTransitionMap ts = Map.fromList
  [ ((fromState t, inputSymbol t, stackTop t), (toState t, stackOps t)) | t <- ts ]

-- automaton data type
data Automaton = Automaton
  { states :: [String]                 -- states
  , symbols :: [String]                -- input symbols, can be multi-word
  , startState :: String               -- Start state
  , acceptStates :: [String]           -- list of accepting states - do we need a jail state for invalid?
  , transitions :: Map.Map (String, String, String) (String, [String]) -- dictionary of transitions (key: (state, input, stackTop), value: (nextState, stackOperations))
} deriving (Show, Eq)



outAutomaton :: Automaton 
outAutomaton = Automaton 
    {
	states = ["q_0", "q_f", "q_j"] -- yap ab if we're using a jail state
	, symbols = ["just put the O in the bag bro", "pause", "ohio", "pluh", "rizz", "and", "skibidi", "H tuah", "toilet", "hawk"] -- add more 
	, startState = "q_0"
	, acceptStates = ["q_f"] -- q_j if we're using it 
	 , transitions = buildTransitionMap transitionsList 
    }

simulate :: Automaton -> [String] -> Bool 
-- takes an automaton and input string(s)

simulate automaton [] = False 
simulate automaton input = simulateHelper automaton (startState automaton) input ["S"]

simulateHelper :: Automaton -> String -> [String] -> [String] -> Bool
-- (automata) (current state) (current input) (current stack)
simulateHelper automata currentState [] stack  -- empty input string
	| null stack && currentState `elem` acceptStates automata = True
	| otherwise = False 	

simulateHelper automata currentState input [] = False -- empty stack but still input - invalid

simulateHelper automaton currentState (x:xs) stack = 
	case Map.lookup (currentState, x, stackTop) (transitions automaton) of
		-- if a valid transition is found, apply it
		Just (nextState, stackOps) ->
			let newStack = applyStackOps stackOps (tail stack) in
		    simulateHelper automaton nextState xs newStack
		-- handle lambda transitions if available
		Nothing -> tryLambdaTransitions automaton currentState (x:xs) stack
  where
    stackTop = if null stack then "" else head stack

tryLambdaTransitions :: Automaton -> String -> [String] -> [String] -> Bool
tryLambdaTransitions automaton currentState input stack =
  case Map.lookup (currentState, "lambda", stackTop) (transitions automaton) of
    Just (nextState, stackOps) ->
      let newStack = applyStackOps stackOps (tail stack) in
      simulateHelper automaton nextState input newStack
    Nothing -> False
  where
    stackTop = if null stack then "" else head stack

-- helper to apply stack operations
applyStackOps :: [String] -> [String] -> [String]
applyStackOps ops stack = reverse ops ++ stack
