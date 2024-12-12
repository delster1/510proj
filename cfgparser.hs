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
  , Transition "q_0" "and" "C" "q_0" []
  , Transition "q_0" "lambda" "C" "q_0" []
  , Transition "q_0" "lambda" "O" "q_0" ["DO"]
  , Transition "q_0" "lambda" "O" "q_0" ["N"]
  , Transition "q_0" "ohio" "O" "q_0" []
  , Transition "q_0" "pluh" "O" "q_0" []
  , Transition "q_0" "rizz" "O" "q_0" []
  , Transition "q_0" "skibidi" "N" "q_0" ["T"]
  , Transition "q_0" "H tuah" "N" "q_0" ["H"]
  , Transition "q_0" "lambda" "N" "q_0" []
  , Transition "q_0" "toilet" "T" "q_0" []
  , Transition "q_0" "lambda" "T" "q_0" []
  , Transition "q_0" "hawk" "H" "q_0" []
  , Transition "q_0" "lambda" "H" "q_0" []
  , Transition "q_0" "lambda" "D" "q_0" ["D", "ahh", "O"]
  , Transition "q_0" "lambda" "D" "q_0" ["N", "ahh", "O"]
  , Transition "q_0" "lambda" "D" "q_0" ["CD"]
  , Transition "q_0" "lambda" "D" "q_0" ["DO"]
  , Transition "q_0" "glaze" "D" "q_0" []
  , Transition "q_0" "cooked" "D" "q_0" []
  , Transition "q_0" "sus" "D" "q_0" []
  , Transition "q_0" "mid" "D" "q_0" []
  , Transition "q_0" "crashout" "D" "q_0" []
  , Transition "q_0" "goated" "D" "q_0" []
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


-- Example automaton
outAutomaton :: Automaton
outAutomaton = Automaton
  { states = ["q_0", "q_f", "q_j"]
  , symbols = ["just put the O in the bag bro", "pause", "ohio", "pluh", "rizz", "and", "skibidi", "H tuah", "toilet", "hawk"]
  , startState = "q_0"
  , acceptStates = ["q_f"]
  , transitions = buildTransitionMap transitionsList
  }

-- Simulate function with logging
simulate :: Automaton -> [String] -> IO Bool
simulate automaton input = simulateHelper automaton (startState automaton) input ["S"]

simulateHelper :: Automaton -> String -> [String] -> [String] -> IO Bool
simulateHelper automaton currentState [] stack = do
  putStrLn $ "Reached end of input. State: " ++ currentState ++ ", Stack: " ++ show stack
  if null stack && currentState `elem` acceptStates automaton
    then do
      putStrLn "Accepted!"
      return True
    else do
      putStrLn "Rejected: Input consumed but stack or state is invalid."
      return False
simulateHelper automaton currentState (x:xs) stack = do
  putStrLn $ "Current state: " ++ currentState ++ ", Input: " ++ x ++ ", Stack: " ++ show stack
  case Map.lookup (currentState, x, stackTop) (transitions automaton) of
    Just (nextState, stackOps) -> do
      let newStack = applyStackOps stackOps (tail stack)
      putStrLn $ "Transitioning to state: " ++ nextState ++ ", New stack: " ++ show newStack
      simulateHelper automaton nextState xs newStack
    Nothing -> tryLambdaTransitions automaton currentState (x:xs) stack
  where
    stackTop = if null stack then "" else head stack

tryLambdaTransitions :: Automaton -> String -> [String] -> [String] -> IO Bool
tryLambdaTransitions automaton currentState input stack = do
  putStrLn $ "Trying lambda transitions. State: " ++ currentState ++ ", Stack: " ++ show stack
  case Map.lookup (currentState, "lambda", stackTop) (transitions automaton) of
    Just (nextState, stackOps) -> do
      let newStack = applyStackOps stackOps (tail stack)
      putStrLn $ "Lambda transition to state: " ++ nextState ++ ", New stack: " ++ show newStack
      simulateHelper automaton nextState input newStack
    Nothing -> do
      putStrLn "No lambda transition available."
      return False
  where
    stackTop = if null stack then "" else head stack

-- Helper to apply stack operations
applyStackOps :: [String] -> [String] -> [String]
applyStackOps ops stack = reverse ops ++ stack
