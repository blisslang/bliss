import 'package:bliss/compiler/macro_expander.dart';
import 'package:bliss/compiler/node.dart';
import 'package:bliss/utils.dart';

const _operators = {
  /* Math */
  "+": "+",
  "-": "-",
  "*": "*",
  "/": "/",
  /* Equality */
  "<": "<",
  "<=": "<=",
  ">": ">",
  ">=": ">=",
  "=": "==",
  "not=": "!=",
  /* Logical */
  "and": "&&",
  "or": "||",
};

const moduleSeparator = "#";

const Set<String> _specialFormsNotTerminatedWithSemicolons = {};

class Emitter {
  /*================================== Data ==================================*/

  /*======================= Helper emission functions ========================*/

  String _emitCommaSeparatedItems(List<Node> itemNodes) {
    return itemNodes.map(_emitExpr).join(", ");
  }

  String _emitStatementBody(List<Node> bodyNodes) {
    return bodyNodes
        .map((node) {
          final exprStr = _emitExpr(node);

          final id =
              node.contents is List
                  ? ((node.contents[0] as SymbolNode).contents as String)
                  : "";
          final shouldTerminateWithSemicolon =
              !_specialFormsNotTerminatedWithSemicolons.contains(id);

          return "$exprStr${shouldTerminateWithSemicolon ? ";" : ""}";
        })
        .join("\n");
  }

  // List<Node> _maybeWrapWithMainFunction(List<Node> nodes) {
  //   return _hasExplicitMainFunction(nodes)
  //       ? nodes
  //       : [
  //         ListNode([SymbolNode("def"), SymbolNode("main"), ...nodes]),
  //       ];
  // }

  // bool _hasExplicitMainFunction(List<Node> nodes) {
  //   return nodes.any(
  //     (node) => switch (node) {
  //       ListNode(
  //         contents: [
  //           SymbolNode(contents: "def"),
  //           SymbolNode(contents: "main"),
  //           ...,
  //         ],
  //       ) =>
  //         true,
  //       _ => false,
  //     },
  //   );
  // }

  /*==================== Special form (keyword) emissions ====================*/

  String _emitFunctionDefinition(
    List<Node> nodes, {
    bool private = false,
    bool lambda = false,
  }) {
    String emitFunction({
      SymbolNode? name,
      List<Node>? args,
      required List<Node> bodyNodes,
    }) {
      switch (bodyNodes) {
        // case [final Node bodyNode]
        //     when bodyNode.contents is! List ||
        //         bodyNode.contents[0].contents != "ret":
        //   final nameStr =
        //       name != null ? _emitSymbol(name, private: private) : "";
        //   final argsStr = args != null ? _emitCommaSeparatedItems(args) : "";
        //   final bodyStr = _emitExpr(bodyNode);
        //   return "$nameStr($argsStr) => $bodyStr${!lambda ? ";" : ""}";
        default:
          final nameStr =
              name != null ? _emitSymbol(name, private: private) : "";
          final argsStr = args != null ? _emitCommaSeparatedItems(args) : "";
          final bodyStr = _emitStatementBody(bodyNodes);
          final keywordStr = !lambda ? "let $nameStr = " : "";
          return "$keywordStr($argsStr) => {\n$bodyStr\n}";
      }
    }

    return switch (nodes) {
      // Regular function
      // (def[p] run [...] ...) (regular function with arguments etc.)
      [
        final SymbolNode name,
        ValueListNode(contents: final args as List<Node>),
        ...final bodyNodes,
      ]
          when args.every((x) => x is SymbolNode) =>
        emitFunction(name: name, args: args, bodyNodes: bodyNodes),
      // (def[p] run ...) (shorthand for a function with no arguments)
      [final SymbolNode name, ...final bodyNodes] => emitFunction(
        name: name,
        bodyNodes: bodyNodes,
      ),
      // Anonymous/lambda function
      // (fn [...] ...) (anonymous/lambda function with arguments etc.)
      [ValueListNode(contents: final args as List<Node>), ...final bodyNodes]
          when lambda && args.every((x) => x is SymbolNode) =>
        emitFunction(args: args, bodyNodes: bodyNodes),
      // (fn ...) (shorthand for an anonymous/lambda with no arguments)
      [...final bodyNodes] when lambda => emitFunction(bodyNodes: bodyNodes),
      _ => throw "Invalid function declaration $nodes",
    };
  }

  // String _emitReturn(List<Node> nodes) {
  //   switch (nodes) {
  //     case []:
  //       return "return";
  //     case [final Node returnNode]:
  //       final exprStr = _emitExpr(returnNode);
  //       return "return $exprStr";
  //     case [...final returnNodes] when nodes.isNotEmpty:
  //       final exprsStr = _emitCommaSeparatedItems(returnNodes);
  //       return "return ($exprsStr)";
  //     default:
  //       throw "Invalid return $nodes";
  //   }
  // }

  String _emitVariable(
    List<Node> nodes, {
    bool mutable = false,
    bool private = false,
  }) {
    switch (nodes) {
      case [final SymbolNode name, final value]:
        final nameStr = _emitSymbol(name, private: private);
        final valueStr = _emitExpr(value);
        final maybeMutableValueStr = mutable ? "ref($valueStr)" : valueStr;
        return "let $nameStr = $maybeMutableValueStr";
      default:
        throw "Invalid variable declaration $nodes";
    }
  }

  String _emitVariableMutation(List<Node> nodes) {
    switch (nodes) {
      case [final name, final value]:
        void addCaretToVariableNameInValue(List<Node> valueNodes, String name) {
          for (final node in valueNodes) {
            switch (node) {
              case ListNode(contents: final nodes) ||
                  ValueListNode(contents: final nodes):
                addCaretToVariableNameInValue(nodes, name);
              case SymbolNode(contents: final String str) when str == name:
                value.contents = "$str^";
              default:
                null;
            }
          }
        }

        final nameStr = _emitExpr(name);
        addCaretToVariableNameInValue([value], nameStr);
        final valueStr = _emitExpr(value);
        return "$nameStr := $valueStr";
      default:
        throw "Invalid variable mutation $nodes";
    }
  }

  String _emitCondStatement(List<Node> nodes) {
    bool throwIfAnyIfBranchIsNotCorrect(List<Node> branches) {
      for (var branch in branches) {
        switch (branch) {
          case ListNode(contents: [Node(), ...]):
            null;
          default:
            throw "Invalid branch: $branch in cond statement: $nodes";
        }
      }
      return true;
    }

    String emitIfBranches(List<Node> ifBranches) {
      return ifBranches.indexed
          .map((elem) {
            final ListNode(contents: [Node predicate, ...bodyNodes]) =
                elem.$2 as ListNode;
            final keywordStr = elem.$1 > 0 ? "else if" : "if";
            final predicateStr = _emitExpr(predicate);
            final bodyStr = _emitStatementBody(bodyNodes as List<Node>);
            return "$keywordStr ($predicateStr) {\n$bodyStr\n}";
          })
          .join("\n");
    }

    switch (nodes) {
      // (cond (... ...) (... ...) (else ...)) (cond with an else branch)
      case [
            ...final ifBranches,
            ListNode(
              contents: [SymbolNode(contents: "else"), ...final elseBodyNodes],
            ),
          ]
          when throwIfAnyIfBranchIsNotCorrect(ifBranches):
        final ifBranchesStr = emitIfBranches(ifBranches);
        final elseBodyStr = _emitStatementBody(elseBodyNodes as List<Node>);
        return "$ifBranchesStr\nelse {\n$elseBodyStr\n}";
      // (cond (... ...) (... ...)) (cond with only if branches and no else branch)
      case [...final ifBranches]
          when throwIfAnyIfBranchIsNotCorrect(ifBranches):
        final ifBranchesStr = emitIfBranches(ifBranches);
        return ifBranchesStr;
      default:
        throw "Invalid cond statement: $nodes";
    }
  }

  String _emitNegation(List<Node> nodes) {
    switch (nodes) {
      case [
            ListNode(
              contents: [
                SymbolNode(contents: final op as String),
                ...final restNodes as List<Node>,
              ],
            ),
          ]
          when _operators.containsKey(op):
        final exprStr = _emitOperator(op, restNodes);
        return "!($exprStr)";
      case [final Node negationNode]:
        final exprStr = _emitExpr(negationNode);
        return "!$exprStr";
      default:
        throw "Invalid negation: $nodes";
    }
  }

  String _emitDoBlock(List<Node> nodes) {
    final blockStr = _emitStatementBody(nodes);
    return blockStr;
  }

  // String _emitIndexing(List<Node> nodes, {bool set = false}) {
  //   switch (nodes) {
  //     case [final Node collection, final Node at]:
  //       final collectionStr = _emitExpr(collection);
  //       final atStr = _emitExpr(at);
  //       return "$collectionStr[$atStr]";
  //     case [final Node collection, final Node at, final Node value]:
  //       final collectionStr = _emitExpr(collection);
  //       final atStr = _emitExpr(at);
  //       final valueStr = _emitExpr(value);
  //       return "$collectionStr[$atStr] = $valueStr";
  //     default:
  //       throw "Invalid indexing $nodes";
  //   }
  // }

  String _emitOperator(String op, List<Node> nodes) {
    switch ([_operators[op], nodes]) {
      case [null, _]:
        throw "Non-operator used as operator: $op";
      case [final symbol!, [final lhs!, final rhs!]]:
        final lhsStr = _emitExpr(lhs as Node);
        final rhsStr = _emitExpr(rhs as Node);
        return "$lhsStr $symbol $rhsStr";
      default:
        throw "Invalid use of operator: $op";
    }
  }

  // String _emitMethodCall(SymbolNode name, List<Node> nodes) {
  //   switch (nodes) {
  //     case [final Node object, ...final args]:
  //       final objectStr = _emitExpr(object);
  //       final nameStr = _emitSymbol(name);
  //       final argsStr = _emitCommaSeparatedItems(args);
  //       return "$objectStr$nameStr($argsStr)";
  //     default:
  //       throw "Invalid method call: $name $nodes";
  //   }
  // }

  String _emitPropertyAccess(SymbolNode name, List<Node> nodes) {
    switch (nodes) {
      case [final Node object]:
        final objectStr = _emitExpr(object);
        final nameStr = _emitSymbol(name);
        return "$objectStr$nameStr";
      default:
        throw "Invalid property use: $name $nodes";
    }
  }

  String _emitFunctionCall(SymbolNode name, List<Node> nodes) {
    final nameStr = _emitSymbol(name);
    final argsStr = _emitCommaSeparatedItems(nodes);
    return "$nameStr($argsStr)";
  }

  /*============================ Atomic emissions ============================*/

  String _emitNumber(NumberNode number) {
    return (number.contents as num).toString();
  }

  String _emitSymbol(SymbolNode symbol, {bool private = false}) {
    final validIndentifier = escapeInvalidChars(
      snakeCasify(symbol.contents as String),
    );
    return private ? "_$validIndentifier" : validIndentifier;
  }

  String _emitString(StringNode string) {
    return "\"${string.contents}\"";
  }

  String _emitValueList(ValueListNode valueList) {
    final exprs = _emitCommaSeparatedItems(valueList.contents);
    return "[$exprs]";
  }

  String _emitList(ListNode list) {
    final exprs = list.contents as List<Node>;
    final idNode = exprs[0] as SymbolNode;

    switch (exprs) {
      case [SymbolNode(contents: final id as String), ...final nodes]:
        return switch (id) {
          // (mod IO ...)
          "mod" => throw UnimplementedError(),
          // (def[p] puts [str] ...)
          _ when id.startsWith("def") && id.length >= 3 && id.length <= 4 =>
            _emitFunctionDefinition(nodes, private: id.contains("p")),
          // (fn [str] ...)
          "fn" => _emitFunctionDefinition(nodes, lambda: true),
          // (let[mp] x 5)
          _ when id.startsWith("let") && id.length >= 3 && id.length <= 5 =>
            _emitVariable(
              nodes,
              mutable: id.contains("m"),
              private: id.contains("p"),
            ),
          // (ret x)
          // "ret" => _emitReturn(nodes),
          // (set y 8)
          "set" => _emitVariableMutation(nodes),
          // (cond ((x > 5) ... ...) ((true) ... ...) (else ...))
          "cond" => _emitCondStatement(nodes),
          // (not false)
          "not" => _emitNegation(nodes),
          // (do ... ... ...) (groups multiple exprs together for use in (if ... ...) etc.)
          "do" => _emitDoBlock(nodes),
          // (cget lst 0) (get using indexing)
          // "cget" => _emitIndexing(nodes),
          // (cset lst 0 5) (set using indexing)
          // "cset" => _emitIndexing(nodes, set: true),
          // (+ x y)
          _ when _operators.containsKey(id) => _emitOperator(id, nodes),
          // (#for-each lst print) (calls for-each as a method on lst, the first argument)
          // _ when id.startsWith("#") => _emitMethodCall(idNode, nodes),
          // (@is-even i) / (set (@length lst) lst 1) ()
          _ when id.startsWith("@") => _emitPropertyAccess(idNode, nodes),
          // anything else (not a keyword)
          _ => _emitFunctionCall(idNode, nodes),
        };
      default:
        throw "Invalid list in _emitList: $list";
    }
  }

  String _emitExpr(Node expr) {
    return switch (expr) {
      NumberNode() => _emitNumber(expr),
      SymbolNode() => _emitSymbol(expr),
      StringNode() => _emitString(expr),
      ValueListNode() => _emitValueList(expr),
      ListNode() => _emitList(expr),
    };
  }

  String emit(Node ast, {Node? stdlibAst}) {
    // final formatter = DartFormatter(languageVersion: Version(3, 7, 3));

    final astWithStdlib =
        stdlibAst != null
            ? (stdlibAst.contents as List<Node>) + (ast.contents as List<Node>)
            : ast.contents as List<Node>;

    final expandedAst = MacroExpander().expand(astWithStdlib);

    final ir = _emitStatementBody(expandedAst);

    return ir;

    // try {
    //   return formatter.format(emittedCode);
    // } on ArgumentError catch (e) {
    //   print("Error while formatting: ${e.message}");
    //   return emittedCode;
    // } on FormatterException catch (e) {
    //   print("Error while formatting: ${e.message(color: true)}");
    //   return emittedCode;
    // }
  }
}
