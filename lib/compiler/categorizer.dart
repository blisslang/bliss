import 'package:bliss/compiler/errors.dart';
import 'package:bliss/compiler/node.dart';

class Categorizer {
  final List<Node> _stack = [];
  Node _current = ListNode([]);

  Node _toNode(String token) {
    if (_current is StringNode) {
      return StringNode(token);
    }

    return switch (num.tryParse(token)) {
      null => SymbolNode(token),
      final number => NumberNode(number),
    };
  }

  void _categorizeOpening(Node newCurrent) {
    _stack.add(_current);
    _current = newCurrent;
  }

  void _categorizeClosing({required String stringRepr}) {
    try {
      final newCurrent = _stack.removeLast();
      newCurrent.add(_current);
      _current = newCurrent;
    } on StateError {
      throw UnexpectedNodeError("Unexpected node: $stringRepr");
    }
  }

  void _categorizeElse(String token) {
    _current.add(_toNode(token));
  }

  List<String> _categorizeComment(List<String> tokens) {
    return tokens.skipWhile((x) => x != ";").skip(1).toList();
  }

  Node categorize(List<String> inTokens) {
    var tokens = inTokens;

    while (tokens.isNotEmpty) {
      final token = tokens.removeAt(0);

      // print(
      //   "TOKENS: $tokens\nTOKEN: $token\nSTACK: $_stack\nCURRENT: $_current\n\n",
      // );

      switch (token) {
        case ";":
          tokens = _categorizeComment(tokens);

        case ")":
          _categorizeClosing(stringRepr: ")");
        case "]":
          _categorizeClosing(stringRepr: "]");
        case "\""
            when token is StringNode &&
                (_current.contents as String).endsWith("\\"):
          _categorizeClosing(stringRepr: "\"");

        case "(":
          _categorizeOpening(ListNode([]));
        case "[":
          _categorizeOpening(ValueListNode([]));
        case "\"":
          _categorizeOpening(StringNode(""));

        default:
          _categorizeElse(token);
      }
    }

    return _current;
  }
}
