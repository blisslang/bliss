module Compiler.Emitter (emit) where

import Compiler.MacroExpander (expand)
import Compiler.Mod (Node (ListNode, NumberNode, StringNode, SymbolNode, ValueListNode), isListNode, isSymbolNode)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Utils (escapeInvalidChars, snakeCasify)

operators :: [(String, String)]
operators =
  [ -- Math
    ("+", "+"),
    ("-", "-"),
    ("*", "*"),
    ("/", "/"),
    -- Equality
    ("<", "<"),
    ("<=", "<="),
    (">", ">"),
    (">=", ">="),
    ("=", "=="),
    ("not=", "!="),
    -- Logical
    ("and", "&&"),
    ("or", "||")
  ]

specialFormsNotTerminatedWithSemicolons :: [String]
specialFormsNotTerminatedWithSemicolons = ["do"]

------------------------- Helper emission functions --------------------------

emitCommaSeparatedItems :: [Node] -> String
emitCommaSeparatedItems list = intercalate ", " $ map emitExpr list

emitStatementBody :: [Node] -> String
emitStatementBody bodyNodes =
  let emitInner node =
        let exprStr = emitExpr node
            identifier = case node of
              ListNode (SymbolNode str : _) -> str
              ValueListNode (SymbolNode str : _) -> str
              _ -> ""
            shouldTerminateWithSemicolon = identifier `notElem` specialFormsNotTerminatedWithSemicolons
         in exprStr ++ (if shouldTerminateWithSemicolon then ";" else "")
   in intercalate "\n" $ map emitInner bodyNodes

------------------ Special form (keyword/syntax) emissions -------------------

emitModuleDefinition :: [Node] -> String
emitModuleDefinition (SymbolNode name : bodyNodes) =
  let nameStr = emitSymbol $ SymbolNode name
      bodyStr = emitStatementBody bodyNodes
   in "module " ++ nameStr ++ " = {\n" ++ bodyStr ++ "\n}"
emitModuleDefinition nodes = error $ "Invalid module definition: " ++ show nodes

emitModuleImport :: [Node] -> String
-- (use List ...) regular module import (variadic)
emitModuleImport names
  | all isSymbolNode names =
      let aux name =
            let nameStr = emitSymbol name
             in "open " ++ nameStr
          importsStr = intercalate "\n" $ map aux names
       in importsStr
-- (use List (sort (cons 0 [1 2 3]))) shorthand for importing a module into one statement
emitModuleImport [SymbolNode name, bodyNode] =
  let nameStr = emitSymbol $ SymbolNode name
      bodyStr = emitExpr bodyNode
   in nameStr ++ ".(" ++ bodyStr ++ ")"
emitModuleImport nodes = error $ "Invalid module import: " ++ show nodes

emitFunctionDefinition :: [Node] -> Bool -> Bool -> String
emitFunctionDefinition nodes recursive lambda =
  let aux name args bodyNodes =
        let nameStr = if null name then "" else emitSymbol $ SymbolNode name
            argsStr = if null args then "" else emitCommaSeparatedItems args
            bodyStr = emitStatementBody bodyNodes
            maybeRecursiveStr = if not lambda && recursive then " rec " else " "
            keywordStr = if not lambda then "let" ++ maybeRecursiveStr ++ nameStr ++ " = " else ""
         in keywordStr ++ "(" ++ argsStr ++ ") => {\n" ++ bodyStr ++ "\n}"
   in case nodes of
        -- Regular function
        SymbolNode name : ValueListNode args : bodyNodes | all isSymbolNode args -> aux name args bodyNodes
        -- Regular function with no argument list shorthand
        SymbolNode name : bodyNodes -> aux name [] bodyNodes
        -- Anonymous/lambda function
        ValueListNode args : bodyNodes | lambda && all isSymbolNode args -> aux "" args bodyNodes
        -- Anonymous/lambda function with argument list shorthand
        bodyNodes | lambda -> aux "" [] bodyNodes
        _ -> error $ "Invalid function declaration: " ++ show nodes

emitLetBinding :: [Node] -> String
emitLetBinding [name, value]
  | not $ isListNode name =
      let nameStr = emitExpr name
          valueStr = emitExpr value
       in "let " ++ nameStr ++ " = " ++ valueStr
emitLetBinding nodes =
  error $ "Invalid let binding: " ++ show nodes

emitCondStatement :: [Node] -> String
emitCondStatement nodes =
  let aux (ListNode (predicate : bodyNodes)) isFirst =
        let keywordStr = if isFirst then "if" else "else if"
            predicateStr = emitExpr predicate
            bodyStr = emitStatementBody bodyNodes
         in keywordStr ++ " (" ++ predicateStr ++ ") {\n" ++ bodyStr ++ "\n}"
      aux branch _ = error $ "Invalid branch in cond statment: " ++ show branch
      isFirstBranch _ [] = error "Invalid branches in isFirstBranch: []"
      isFirstBranch branch [firstBranch] = branch == firstBranch
      isFirstBranch branch (firstBranch : _) = branch == firstBranch
   in case reverse nodes of
        -- (cond (... ...) ... (else ...)) with else branch
        ListNode (SymbolNode "else" : elseBodyNodes) : reversedIfBranches ->
          let ifBranches = reverse reversedIfBranches
              ifBranchesStr = intercalate "\n" $ map (\el -> aux el (isFirstBranch el ifBranches)) ifBranches
              elseBodyStr = emitStatementBody elseBodyNodes
           in ifBranchesStr ++ "\nelse {\n" ++ elseBodyStr ++ "\n}"
        -- (cond (... ...) ...) only if branches no else branch
        reversedIfBranches
          | not $ null reversedIfBranches ->
              let ifBranches = reverse reversedIfBranches
                  ifBranchesStr = intercalate "\n" $ map (\el -> aux el (isFirstBranch el ifBranches)) ifBranches
               in ifBranchesStr
        _ -> error $ "Invalid cond statement: " ++ show nodes

emitNegation :: [Node] -> String
emitNegation [ListNode (SymbolNode operator : restNodes)]
  | isJust $ lookup operator operators =
      let exprStr = emitOperator operator restNodes
       in "!(" ++ exprStr ++ ")"
emitNegation [negationNode] =
  let exprStr = emitExpr negationNode
   in "!" ++ exprStr
emitNegation nodes = error $ "Invalid negation: " ++ show nodes

emitDoBlock :: [Node] -> String
emitDoBlock nodes =
  let bodyStr = emitStatementBody nodes
   in bodyStr

emitOperator :: String -> [Node] -> String
emitOperator operator args
  | isJust (lookup operator operators) && length args >= 2 =
      let aux (ListNode (SymbolNode operator' : restNodes))
            | isJust $ lookup operator' operators =
                let exprStr = emitOperator operator' restNodes
                 in "(" ++ exprStr ++ ")"
          aux argNode =
            let exprStr = emitExpr argNode
             in exprStr
          operatorStr = fromJust $ lookup operator operators
          argsStr = intercalate (" " ++ operatorStr ++ " ") $ map aux args
       in argsStr
emitOperator _ nodes = error $ "Invalid use of operator: " ++ show nodes

emitFunctionCall :: String -> [Node] -> String
emitFunctionCall name nodes =
  let nameStr = emitSymbol $ SymbolNode name
      argsStr = emitCommaSeparatedItems nodes
   in nameStr ++ "(" ++ argsStr ++ ")"

------------------------------ Atomic emissions ------------------------------

emitNumber :: Node -> String
emitNumber (NumberNode num) = show num
emitNumber node = error $ "Invalid number: " ++ show node

emitSymbol :: Node -> String
emitSymbol (SymbolNode sym) = escapeInvalidChars $ snakeCasify sym
emitSymbol node = error $ "Invalid symbol: " ++ show node

emitString :: Node -> String
emitString (StringNode str) = "\"" ++ str ++ "\""
emitString node = error $ "Invalid string: " ++ show node

emitValueList :: Node -> String
emitValueList (ValueListNode nodes) =
  let exprs = emitCommaSeparatedItems nodes
   in "[" ++ exprs ++ "]"
emitValueList node = error $ "Invalid value list: " ++ show node

emitList :: Node -> String
-- (defmod IO ...)
emitList (ListNode (SymbolNode "defmod" : nodes)) = emitModuleDefinition nodes
-- (use IO)
emitList (ListNode (SymbolNode "use" : nodes)) = emitModuleImport nodes
-- (def puts [str] ...)
emitList (ListNode (SymbolNode "def" : nodes)) = emitFunctionDefinition nodes False False
-- (defrec puts [str] ...)
emitList (ListNode (SymbolNode "defrec" : nodes)) = emitFunctionDefinition nodes True False
-- (fn [str] ...)
emitList (ListNode (SymbolNode "fn" : nodes)) = emitFunctionDefinition nodes False True
-- (let x 5)
emitList (ListNode (SymbolNode "let" : nodes)) = emitLetBinding nodes
-- (cond (true ...) (false ...) ... (else ...))
emitList (ListNode (SymbolNode "cond" : nodes)) = emitCondStatement nodes
-- (not true)
emitList (ListNode (SymbolNode "not" : nodes)) = emitNegation nodes
-- (do ...)
emitList (ListNode (SymbolNode "do" : nodes)) = emitDoBlock nodes
-- (+ 5 3)
emitList (ListNode (SymbolNode identifier : nodes)) | isJust $ lookup identifier operators = emitOperator identifier nodes
-- everything else
emitList (ListNode (SymbolNode identifier : nodes)) = emitFunctionCall identifier nodes
emitList exprs = error $ "Invalid list in emitList: " ++ show exprs

emitExpr :: Node -> String
emitExpr (NumberNode num) = emitNumber $ NumberNode num
emitExpr (SymbolNode sym) = emitSymbol $ SymbolNode sym
emitExpr (StringNode str) = emitString $ StringNode str
emitExpr (ValueListNode nodes) = emitValueList $ ValueListNode nodes
emitExpr (ListNode nodes) = emitList $ ListNode nodes

emit :: Node -> String
emit (ListNode astNodes) =
  let expandedAst = expand astNodes
   in emitStatementBody expandedAst
emit _ = error "AST has to be wrapped with a ListNode"