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
  , stackOps :: [[String]] -- all possible stackops that can be applied 
} deriving (Show, Eq)

-- where we store transitions
transitionsList :: [Transition]
transitionsList = [
    Transition "q_0" "lambda" "S" "q_0" [["P", "F"]],
	Transition "q_0" "lambda" "P" "q_0" [["O"], ["O","C","P"], ["lambda"]],
	Transition "q_0" "lambda" "F" "q_0" [["just put the", "O", "in the bag bro"], ["pause"], ["lambda"]],
	Transition "q_0" "lambda" "N" "q_0" [["skibidi","T"],["H", "tuah"]],
	Transition "q_0" "lambda" "T" "q_0" [["toilet"], ["lambda"]],
	Transition "q_0" "lambda" "H" "q_0" [["hawk"], ["lambda"]],
	Transition "q_0" "lambda" "D" "q_0" [["lambda"], ["cooked"],["sus"],["mid"],["crashout"],["C","D"],["N"], ["N", "ahh"], ["D", "ahh"],["D","D"]],
	Transition "q_0" "lambda" "O" "q_0" [["ohio"],["pluh"], ["rizz"], ["mewer"], ["edge"], ["mewing"], ["twin"], ["unc"],["N"],["D","O"]],
	Transition "q_0" "lambda" "C" "q_0" [["and"]],
	Transition "q_0" "ohio" "ohio" "q_0" [[]],
	Transition "q_0" "hawk" "hawk" "q_0" [[]],
	Transition "q_0" "tuah" "tuah" "q_0" [[]],
	Transition "q_0" "ahh" "ahh" "q_0" [[]],
	Transition "q_0" "and" "and" "q_0" [[]],
	Transition "q_0" "skibidi" "skibidi" "q_0" [[]],
	Transition "q_0" "toilet" "toilet" "q_0" [[]],
	Transition "q_0" "just put the" "just put the" "q_0" [[]],
	Transition "q_0" "in the bag bro" "in the bag bro" "q_0" [[]],
	Transition "q_0" "unc" "unc" "q_0" [[]],
	Transition "q_0" "crashout" "crashout" "q_0" [[]],
	Transition "q_0" "cooked" "cooked" "q_0" [[]],
	Transition "q_0" "sus" "sus" "q_0" [[]],
	Transition "q_0" "mid" "mid" "q_0" [[]],
	Transition "q_0" "rizz" "rizz" "q_0" [[]],
	Transition "q_0" "mewer" "mewer" "q_0" [[]],
	Transition "q_0" "mewing" "mewing" "q_0" [[]],
	Transition "q_0" "edge" "edge" "q_0" [[]],
	Transition "q_0" "twin" "twin" "q_0" [[]],
	Transition "q_0" "pluh" "pluh" "q_0" [[]],
	Transition "q_0" "pause" "pause" "q_0" [[]]
	]

-- turns transitions list into dictionary w/ key [state, symbol, stacktopo] val: [nextState, stack symbols]
buildTransitionMap :: [Transition] -> Map.Map (String, String, String) (String, [[String]])
buildTransitionMap ts = Map.fromList
  [ ((fromState t, inputSymbol t, stackTop t), (toState t, stackOps t)) | t <- ts ]


-- automaton data type
data Automaton = Automaton
  { states :: [String]                 -- states
  , symbols :: [String]                -- input symbols, can be multi-word
  , startState :: String               -- Start state
  , acceptStates :: [String]           -- list of accepting states - do we need a jail state for invalid?
  , transitions :: Map.Map (String, String, String) (String, [[String]]) -- dictionary of transitions (key: (state, input, stackTop), value: (nextState, stackOperations))
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

reverseList :: [String] -> [String]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
-- Simulate function with logging
simulate :: Automaton -> [String] -> IO Bool
simulate automaton input = simulateHelper automaton (startState automaton) input ["S"]

-- Helper to apply stack operations
applyStackOps :: [String] -> [String] -> [String]
applyStackOps ops stack = stack ++ ops  -- Append new operations at the end (stack grows down)

popStackOps :: [String] -> [String] -> [String]
popStackOps ops stack = reverseList (tail (reverseList stack)) ++ reverseList ops

simulateHelper :: Automaton -> String -> [String] -> [String] -> IO Bool
simulateHelper automaton currentState [] stack = do
  putStrLn $ "Reached end of input. State: " ++ currentState ++ ", Stack: " ++ show stack
  if stack == ["lambda"]-- && currentState `elem` acceptStates automaton
    then do
      putStrLn "Accepted!"
      return True
    else do
      putStrLn "Rejected: Input consumed but stack or state is invalid."
      return False

simulateHelper automaton currentState (x:xs) stack = do
  putStrLn $ "Current state: " ++ currentState ++ ", Input: " ++ x ++ ", Stack: " ++ show stack
  case Map.lookup (currentState, x, stackTop) (transitions automaton) of
    Just (nextState, stackOpsList) -> tryStackOps automaton nextState stackOpsList xs stack
    Nothing -> tryLambdaTransitions automaton currentState (x:xs) stack
  where
    stackTop = if null stack then "" else last stack -- Top of stack is the last element

tryLambdaTransitions :: Automaton -> String -> [String] -> [String] -> IO Bool
tryLambdaTransitions automaton currentState input stack = do
  putStrLn $ "Trying lambda transitions. State: " ++ currentState ++ ", Stack: " ++ show stack
  case Map.lookup (currentState, "lambda", stackTop) (transitions automaton) of
    Just (nextState, stackOpsList) -> tryStackOps automaton nextState stackOpsList input stack
    Nothing -> do
      putStrLn "No lambda transition available."
      return False
  where
    stackTop = if null stack then "" else last stack -- Top of stack is the last element

tryStackOps :: Automaton -> String -> [[String]] -> [String] -> [String] -> IO Bool
tryStackOps automaton nextState [] remainingInput stack = do
  putStrLn "No valid stack operations left to try."
  return False
tryStackOps automaton nextState (ops:remainingOps) remainingInput stack = do
  let newStack = popStackOps ops stack
  putStrLn $ "Trying stack operations: " ++ show ops ++ ", New stack: " ++ show newStack
  result <- simulateHelper automaton nextState remainingInput newStack
  if result
    then return True
    else tryStackOps automaton nextState remainingOps remainingInput stack

