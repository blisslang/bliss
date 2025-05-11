module Compiler.Mod (CompilerError (..), addToNode, Node (..), isNumberNode, isSymbolNode, isStringNode, isValueListNode, isListNode) where

import Utils (throw)

data CompilerError
  = InvalidInputFileError String
  | UnexpectedNodeError String
  | InvalidNodeTypeError String
  deriving (Show)

data Node
  = ListNode [Node]
  | ValueListNode [Node]
  | StringNode String
  | SymbolNode String
  | NumberNode Float
  deriving (Show, Eq)

addToNode :: Node -> Node -> Node
addToNode (ListNode contents) newNode = ListNode (contents ++ [newNode])
addToNode (ValueListNode contents) newNode = ValueListNode (contents ++ [newNode])
addToNode _ newNode = throw $ InvalidNodeTypeError $ "Expected one of (ListNode, ValueListNode, StringNode), got: " ++ show newNode

isListNode :: Node -> Bool
isListNode (ListNode _) = True
isListNode _ = False

isValueListNode :: Node -> Bool
isValueListNode (ValueListNode _) = True
isValueListNode _ = False

isStringNode :: Node -> Bool
isStringNode (StringNode _) = True
isStringNode _ = False

isSymbolNode :: Node -> Bool
isSymbolNode (SymbolNode _) = True
isSymbolNode _ = False

isNumberNode :: Node -> Bool
isNumberNode (NumberNode _) = True
isNumberNode _ = False