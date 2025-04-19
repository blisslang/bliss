import 'package:bliss/compiler/node.dart';
import 'package:bliss/utils.dart';
import 'package:dart_style/dart_style.dart';
import 'package:pub_semver/pub_semver.dart';

const _operators = {
  "+": "+",
  "-": "-",
  "*": "*",
  "/": "/",
  "<": "<",
  "<=": "<=",
  ">": ">",
  ">=": ">=",
  "=": "=",
  "not=": "!=",
  "and": "&&",
  "or": "||",
};

class Emitter {
  /*================================== Data ==================================*/

  // Node? parentNode;
  // final _modules = <String>{};

  /*======================= Helper emission functions ========================*/

  String _emitFunctionArguments(List<Node> argNodes) {
    return argNodes.map(_emitExpr).join(", ");
  }

  String _emitStatementBody(List<Node> bodyNodes) {
    return "${bodyNodes.map(_emitExpr).join(";\n")};";
  }

  /*==================== Special form (keyword) emissions ====================*/

  String _emitFunctionDefinition(List<Node> nodes, {bool private = false}) {
    String aux({
      required SymbolNode name,
      List<Node>? args,
      required List<Node> bodyNodes,
    }) {
      final nameStr = _emitSymbol(name, private: private);
      final argsStr = args != null ? _emitFunctionArguments(args) : "";
      final bodyStr = _emitStatementBody(bodyNodes);
      return "$nameStr($argsStr) {\n$bodyStr\n}";
    }

    return switch (nodes) {
      // (def[p] run [...] ...) (regular function with arguments etc.)
      [
        final SymbolNode name,
        ValueListNode(contents: final args as List<Node>),
        ...final bodyNodes,
      ]
          when args.every((x) => x is SymbolNode) =>
        aux(name: name, args: args, bodyNodes: bodyNodes),
      // (def[p] run ...) (shorthand for a function with no arguments)
      [final SymbolNode name, ...final bodyNodes] => aux(
        name: name,
        bodyNodes: bodyNodes,
      ),
      _ => throw "Invalid function declaration $nodes",
    };
  }

  String _emitVariable(
    List<Node> nodes, {
    bool mutable = false,
    bool private = false,
  }) {
    switch (nodes) {
      case [final SymbolNode name, final value]:
        final keywordStr = mutable ? "var" : "final";
        final nameStr = _emitSymbol(name, private: private);
        final valueStr = _emitExpr(value);
        return "$keywordStr $nameStr = $valueStr";
      default:
        throw "Invalid variable declaration $nodes";
    }
  }

  String _emitVariableMutation(List<Node> nodes) {
    switch (nodes) {
      case [final SymbolNode name, final value]:
        final nameStr = _emitSymbol(name, private: false);
        final valueStr = _emitExpr(value);
        return "$nameStr = $valueStr";
      default:
        throw "Invalid variable mutation $nodes";
    }
  }

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

  String _emitFunctionCall(SymbolNode name, List<Node> args) {
    final nameStr = _emitSymbol(name);
    final argsStr = _emitFunctionArguments(args);
    return "$nameStr($argsStr)";
  }

  /*============================ Atomic emissions ============================*/

  String _emitNumber(NumberNode number) {
    return (number.contents as num).toString();
  }

  String _emitSymbol(SymbolNode symbol, {bool private = false}) {
    final validIndentifier = escapeInvalidChars(
      camelize(symbol.contents as String),
    );
    return private ? "_$validIndentifier" : validIndentifier;
  }

  String _emitString(StringNode string) {
    return "\"${string.contents}\"";
  }

  String _emitValueList(ValueListNode valueList) {
    final exprs = (valueList.contents as List<Node>).map(_emitExpr).join(", ");
    return "[$exprs]";
  }

  String _emitList(ListNode list) {
    final exprs = list.contents as List<Node>;

    switch (exprs) {
      case [SymbolNode(contents: final id as String), ...final nodes]:
        return switch (id) {
          // (mod IO ...)
          "mod" => throw UnimplementedError(),
          // (def[p] puts [str] ...)
          _ when id.startsWith("def") && id.length >= 3 && id.length <= 4 =>
            _emitFunctionDefinition(nodes, private: id.contains("p")),
          // (let[mp] x 5)
          _ when id.startsWith("let") && id.length >= 3 && id.length <= 5 =>
            _emitVariable(
              nodes,
              mutable: id.contains("m"),
              private: id.contains("p"),
            ),
          // (set y 8)
          "set" => _emitVariableMutation(nodes),
          // (if (< n 2) ... ...)
          "if" => throw UnimplementedError(),
          // (when (not= x 5) ...)
          "when" => throw UnimplementedError(),
          // (+ x y)
          _ when _operators.containsKey(id) => _emitOperator(id, nodes),
          // anything else (not a keyword)
          _ => _emitFunctionCall(exprs[0] as SymbolNode, nodes),
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

  String emit(Node ast) {
    final formatter = DartFormatter(languageVersion: Version(3, 7, 3));
    final emittedCode = _emitExpr(ast.contents[0]);
    return formatter.format(emittedCode);
  }
}
