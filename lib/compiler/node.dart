import 'package:bliss/compiler/errors.dart';

sealed class Node {
  dynamic contents;
  Node(this.contents);

  void add(Node newNode) {
    switch (this) {
      case ListNode() || ValueListNode():
        (contents as List<Node>).add(newNode);

      case StringNode():
        if (newNode is! StringNode) {
          throw InvalidNodeTypeError("Expected a `String` node, got $newNode");
        }
        contents += " ${newNode.contents}";

      default:
        throw InvalidNodeTypeError(
          "Expected either a `List`, `ValueList` or `String` node, got $newNode",
        );
    }
  }

  String format({int indentLevel = 0}) {
    final indent = "    " * indentLevel;

    switch (this) {
      case ListNode() || ValueListNode():
        final buffer = StringBuffer();

        buffer.writeln("$indent$runtimeType: [");

        for (final node in (contents as List<Node>)) {
          buffer.write(node.format(indentLevel: indentLevel + 1));
        }

        buffer.writeln("$indent],");

        return buffer.toString();

      case SymbolNode() || StringNode():
        return "$indent$runtimeType: \"$contents\",\n";

      default:
        return "$indent$runtimeType: $contents,\n";
    }
  }

  @override
  String toString() {
    return "$runtimeType($contents)";
  }
}

class ListNode extends Node {
  ListNode(List<Node> super.contents);
}

class ValueListNode extends Node {
  ValueListNode(List<Node> super.contents);
}

class StringNode extends Node {
  StringNode(String super.contents);
}

class SymbolNode extends Node {
  SymbolNode(String super.contents);
}

class NumberNode extends Node {
  NumberNode(num super.contents);
}
