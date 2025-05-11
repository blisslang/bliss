module Compiler.Categorizer (categorize) where

import Compiler.Mod (CompilerError (UnexpectedNodeError), Node (ListNode, NumberNode, StringNode, SymbolNode, ValueListNode), addToNode)
import Data.List (unsnoc)
import Text.Read (readMaybe)
import Utils (throw)

data State = State
  { tokens :: [String],
    stack :: [Node],
    current :: Node
  }
  deriving (Show)

toNode :: State -> String -> Node
toNode _state token = case readMaybe token of
  Just number -> NumberNode number
  Nothing -> SymbolNode token

categorizeString :: State -> State
categorizeString state = case tokens state of
  stringContent : "\"" : newTokens ->
    let newCurrent = addToNode (current state) $ StringNode stringContent
     in state {tokens = newTokens, current = newCurrent}
  _ -> throw $ UnexpectedNodeError "Unexpected node in string"

categorizeComment :: State -> State
categorizeComment state =
  let newTokens = drop 1 $ dropWhile (/= ";") $ tokens state
   in state {tokens = newTokens}

categorizeClosing :: State -> String -> State
categorizeClosing state strRep = case unsnoc $ stack state of
  Just (newStack, newCurrent) ->
    let modifiedNewCurrent = addToNode newCurrent $ current state
     in state {stack = newStack, current = modifiedNewCurrent}
  Nothing -> throw $ UnexpectedNodeError $ "Unexpected node: " ++ strRep

categorizeOpening :: State -> Node -> State
categorizeOpening state newCurrent =
  let newStack = stack state ++ [current state]
   in state {stack = newStack, current = newCurrent}

categorizeElse :: State -> String -> State
categorizeElse state token =
  let newCurrent = addToNode (current state) $ toNode state token
   in state {current = newCurrent}

categorizeToken :: State -> String -> State
categorizeToken state "\"" = categorizeString state
categorizeToken state ";" = categorizeComment state
categorizeToken state ")" = categorizeClosing state ")"
categorizeToken state "]" = categorizeClosing state "]"
categorizeToken state "(" = categorizeOpening state $ ListNode []
categorizeToken state "[" = categorizeOpening state $ ValueListNode []
categorizeToken state token = categorizeElse state token

categorize :: [String] -> Node
categorize inTokens =
  let aux state = case tokens state of
        [] -> current state
        token : rest ->
          let restState = state {tokens = rest}
              categorizedState = categorizeToken restState token
              newState = categorizedState {tokens = tokens categorizedState}
           in aux newState
      initialState = State {tokens = inTokens, stack = [], current = ListNode []}
   in aux initialState