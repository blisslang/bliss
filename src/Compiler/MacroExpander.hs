{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Compiler.MacroExpander (expand) where

import Compiler.Mod (Node (ListNode, SymbolNode, ValueListNode), isSymbolNode)
import Data.List (elemIndex, uncons)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Utils (fixedPoint)

data MacroDefinition = MacroDefinition
  { replacementArgs :: [String],
    macroNodes :: [Node]
  }
  deriving (Show)

registerMacroDefinitions :: [Node] -> ([(String, MacroDefinition)], [Node])
registerMacroDefinitions [] = ([], [])
registerMacroDefinitions (node : rest) =
  let -- (defmacro name [...] ...)
      processNode (ListNode (SymbolNode "defmacro" : SymbolNode name : ValueListNode args : bodyNodes))
        | not $ null args && all isSymbolNode args =
            let stringArgs = map (\(SymbolNode s) -> s) args
                newMacroDefinition = (name, MacroDefinition {replacementArgs = stringArgs, macroNodes = bodyNodes})
             in ([newMacroDefinition], Nothing)
      -- invalid macro
      processNode (ListNode (SymbolNode "defmacro" : _)) = error $ "Invalid macro definition: " ++ show node
      -- continue downwards to find more macros
      processNode (ListNode childNodes) =
        let (macrosFromChildren, remainingChildren) = registerMacroDefinitions childNodes
         in (macrosFromChildren, Just (ListNode remainingChildren))
      -- anything else
      processNode node' =
        ([], Just node')
      (macrosFromNode, maybeNode) = processNode node
      (macrosFromRest, nodesFromRest) = registerMacroDefinitions rest
   in (macrosFromNode ++ macrosFromRest, maybeToList maybeNode ++ nodesFromRest)

expandMacroUsagesPass :: [(String, MacroDefinition)] -> [Node] -> [Node]
expandMacroUsagesPass macroDefinitions astNodes =
  let -- found variadic arg, expand it
      expandArg macroDefinition args (SymbolNode ('&' : argName))
        | ("&" ++ argName) `elem` replacementArgs macroDefinition =
            let replacementIdx = fromMaybe (-1) $ elemIndex ("&" ++ argName) $ replacementArgs macroDefinition
                (_, replacementNodes) = splitAt replacementIdx args
             in ListNode $ SymbolNode "do" : replacementNodes -- wrap with (do ...) just in case
            -- found arg, expand it
      expandArg macroDefinition args (SymbolNode argName)
        | argName `elem` replacementArgs macroDefinition =
            let replacementIdx = fromMaybe (-1) $ elemIndex argName $ replacementArgs macroDefinition
                replacementNode = args !! replacementIdx
             in replacementNode
      -- continue deeper to find more args to expand
      expandArg macroDefinition args (ListNode exprs) = ListNode $ map (expandArg macroDefinition args) exprs
      expandArg macroDefinition args (ValueListNode exprs) = ValueListNode $ map (expandArg macroDefinition args) exprs
      -- anything else, dont do anything
      expandArg _ _ node = node
      isLastArgVariadic args = case uncons (last args) of
        Just ('&', _) -> True
        _ -> False
      -- found a macro usage, expand it node by node
      processNode (ListNode (SymbolNode name : args))
        | isJust $ lookup name macroDefinitions =
            let defaultMacroDefinition = (MacroDefinition {replacementArgs = [], macroNodes = []})
                macroDefinition = fromMaybe defaultMacroDefinition $ lookup name macroDefinitions
                newNode = ListNode $ SymbolNode "do" : macroNodes macroDefinition -- wrap with (do ...) just in case
                expandedNode = expandArg macroDefinition args newNode
                -- validate args to be the defined amount
                definedArgs = replacementArgs macroDefinition
                numDefinedArgs = length definedArgs
                numProvidedArgs = length args
                isVariadic = isLastArgVariadic definedArgs
             in case () of
                  _
                    | isVariadic && numProvidedArgs >= numDefinedArgs -> expandedNode
                    | not isVariadic && numProvidedArgs == numDefinedArgs -> expandedNode
                    | otherwise -> error $ "Invalid macro usage: " ++ name ++ " " ++ show args
      -- go deeper to find more macro definitions
      processNode (ListNode exprs) = ListNode $ map processNode exprs
      processNode (ValueListNode exprs) = ValueListNode $ map processNode exprs
      -- anything else, do nothing
      processNode node = node
   in map processNode astNodes

expandMacroUsages :: [(String, MacroDefinition)] -> [Node] -> [Node]
expandMacroUsages macroDefinitions astNodes =
  let expandPass = expandMacroUsagesPass macroDefinitions
   in fixedPoint expandPass astNodes

expand :: [Node] -> [Node]
expand astNodes =
  let (macroDefinitions, nonMacroDefinitionNodes) = registerMacroDefinitions astNodes
      expandedAst = expandMacroUsages macroDefinitions nonMacroDefinitionNodes
   in expandedAst