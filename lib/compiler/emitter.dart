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

const _specialFormsNotTerminatedWithSemicolons = {"do"};

class Emitter {
  /*================================== Data ==================================*/

  /*========================= General utilities etc. =========================*/

  Future<String> _tryFormatIr(String ir) async {
    // Try to format using both refmt and bsrefmt

    final (
      bsrefmtRes,
      bsrefmtExitCode,
    ) = await runCommandWithStdinAndFallbackValueOnError(
      "bsrefmt",
      [],
      valueOnError: ir,
      stdin: [ir],
    );

    if (bsrefmtExitCode == 0) return bsrefmtRes;

    final (
      refmtRres,
      refmtExitCode,
    ) = await runCommandWithStdinAndFallbackValueOnError(
      "refmt",
      [],
      valueOnError: ir,
      stdin: [ir],
    );

    if (refmtExitCode == 0) return refmtRres;

    return ir;
  }

  /*======================= Helper emission functions ========================*/

  String _emitCommaSeparatedItems(List<Node> itemNodes) {
    return itemNodes.map(_emitExpr).join(", ");
  }

  String _emitStatementBody(List<Node> bodyNodes) {
    return bodyNodes
        .map((node) {
          final exprStr = _emitExpr(node);

          final id =
              node.contents is List && node.contents.isNotEmpty
                  ? ((node.contents[0] as SymbolNode).contents as String)
                  : "";
          final shouldTerminateWithSemicolon =
              !_specialFormsNotTerminatedWithSemicolons.contains(id);

          return "$exprStr${shouldTerminateWithSemicolon ? ";" : ""}";
        })
        .join("\n");
  }

  /*==================== Special form (keyword) emissions ====================*/

  String _emitModuleDefinition(List<Node> nodes) {
    switch (nodes) {
      case [final SymbolNode name, ...final bodyNodes]:
        final nameStr = _emitSymbol(name);
        final bodyStr = _emitStatementBody(bodyNodes);
        return "module $nameStr = {\n$bodyStr\n}";
      default:
        throw "Invalid module definition $nodes";
    }
  }

  String _emitModuleImport(List<Node> nodes) {
    switch (nodes) {
      // (use List (sort (cons 0 [1 2 3])))
      case [final SymbolNode name, final ListNode bodyNode]:
        final nameStr = _emitSymbol(name);
        final bodyStr = _emitList(bodyNode);
        return "$nameStr.($bodyStr)";
      // List.(sort(cons(0, [1, 2, 3])))
      // (use List)
      case [final SymbolNode name]:
        final nameStr = _emitSymbol(name);
        return "open $nameStr";
      default:
        throw "Invalid module import $nodes";
    }
  }

  String _emitFunctionDefinition(
    List<Node> nodes, {
    bool private = false,
    bool lambda = false,
    bool recursive = false,
  }) {
    String emitFunction({
      SymbolNode? name,
      List<Node>? args,
      required List<Node> bodyNodes,
    }) {
      switch (bodyNodes) {
        default:
          final nameStr =
              name != null ? _emitSymbol(name, private: private) : "";
          final argsStr = args != null ? _emitCommaSeparatedItems(args) : "";
          final bodyStr = _emitStatementBody(bodyNodes);
          final maybeRecursiveStr = !lambda && recursive ? " rec " : " ";
          final keywordStr = !lambda ? "let$maybeRecursiveStr$nameStr = " : "";
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

  String _emitVariable(
    List<Node> nodes, {
    bool mutable = false,
    bool private = false,
  }) {
    switch (nodes) {
      case [final Node name, final value] when name is! ListNode:
        final nameStr = _emitExpr(name);
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
      // (not (+ 5 3))
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
      // (not false)
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

  String _emitOperator(String op, List<Node> nodes) {
    String emitArg(Node arg) {
      // (+ 5 3)
      switch (arg) {
        case ListNode(
              contents: [
                SymbolNode(contents: final op as String),
                ...final restNodes as List<Node>,
              ],
            )
            when _operators.containsKey(op):
          final exprStr = _emitOperator(op, restNodes);
          return "($exprStr)";
        // false
        case final Node argNode:
          final exprStr = _emitExpr(argNode);
          return exprStr;
      }
    }

    switch (nodes) {
      case [...final args] when args.length >= 2:
        final opStr = _operators[op]!;
        final argsStr = args.map(emitArg).join(" $opStr ");
        return argsStr;
      default:
        throw "Invalid use of operator $nodes";
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
          "defmod" => _emitModuleDefinition(nodes),
          // (use IO)
          "use" => _emitModuleImport(nodes),
          // (def[p] puts [str] ...)
          _ when id.startsWith("def") && id.length >= 3 && id.length <= 4 =>
            _emitFunctionDefinition(
              nodes,
              private: id.contains("p"),
              recursive: false,
            ),
          // (defrec[p] puts [str] ...)
          _ when id.startsWith("defrec") && id.length >= 6 && id.length <= 7 =>
            _emitFunctionDefinition(
              nodes,
              private: id.contains("p"),
              recursive: true,
            ),
          // (fn [str] ...)
          "fn" => _emitFunctionDefinition(nodes, lambda: true),
          // (let[mp] x 5)
          _ when id.startsWith("let") && id.length >= 3 && id.length <= 5 =>
            _emitVariable(
              nodes,
              mutable: id.contains("m"),
              private: id.contains("p"),
            ),
          // (set y 8)
          "set" => _emitVariableMutation(nodes),
          // (cond ((x > 5) ... ...) ((true) ... ...) (else ...))
          "cond" => _emitCondStatement(nodes),
          // (not false)
          "not" => _emitNegation(nodes),
          // (do ... ... ...) (groups multiple exprs together for use in (if ... ...) etc.)
          "do" => _emitDoBlock(nodes),
          // (+ x y)
          _ when _operators.containsKey(id) => _emitOperator(id, nodes),
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

  Future<String> emit(Node ast, {Node? stdlibAst}) async {
    final astWithStdlib =
        stdlibAst != null
            ? (stdlibAst.contents as List<Node>) + (ast.contents as List<Node>)
            : ast.contents as List<Node>;

    final expandedAst = MacroExpander().expand(astWithStdlib);

    final ir = _emitStatementBody(expandedAst);
    print("NONFORMATTED IR: $ir");
    final maybeFormattedIr = await _tryFormatIr(ir);

    return maybeFormattedIr;
  }
}
