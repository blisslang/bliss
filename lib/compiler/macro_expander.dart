import 'package:bliss/compiler/node.dart';

class _MacroDefinition {
  final List<String> replacementArgs;
  final List<Node> bodyNodes;
  _MacroDefinition({required this.replacementArgs, required this.bodyNodes});

  @override
  String toString() {
    return "$runtimeType(replacementArgs: $replacementArgs, bodyNodes: $bodyNodes)";
  }
}

class MacroExpander {
  final _macroDefinitions = <String, _MacroDefinition>{};

  void _registerMacroDefinitions(List<Node> nodes) {
    final macroDefinitionNodesToRemove = <ListNode>[];

    for (final node in nodes) {
      switch (node) {
        case ListNode(
              contents: [
                SymbolNode(contents: "defmacro"),
                SymbolNode(contents: final String name),
                ValueListNode(contents: final List<Node> args),
                ...final List<Node> bodyNodes,
              ],
            )
            when args.isNotEmpty && args.every((x) => x is SymbolNode):
          _macroDefinitions[name] = _MacroDefinition(
            replacementArgs: args.map((x) => x.contents as String).toList(),
            bodyNodes: bodyNodes,
          );
          macroDefinitionNodesToRemove.add(node);

          _registerMacroDefinitions(node.contents);
        case ListNode(contents: [SymbolNode(contents: "defmacro"), ...]):
          throw "Invalid macro definition $node";
        case ListNode():
          _registerMacroDefinitions(node.contents);
        default:
          null;
      }
    }

    // Removes any macro definition nodes from the ast as they are not of use to us anymore and would be unnecessary to send to the emitter etc.
    for (var nodeToRemove in macroDefinitionNodesToRemove) {
      nodes.remove(nodeToRemove);
    }
  }

  void _expandMacroUsages(List<Node> nodes) {
    (List<Node>, bool) expandMacroUsage(
      String name,
      List<Node> args,
      List<String> replacementArgs,
      List<Node> bodyNodes,
    ) {
      var hasExpanded = false;

      final expandedBodyNodes =
          bodyNodes.map((macroBodyNode) {
            switch (macroBodyNode) {
              // An expansion is happening
              case SymbolNode(contents: final String name)
                  when replacementArgs.contains(name):
                hasExpanded = true;

                final replacementIdx = replacementArgs.indexOf(name);
                return args[replacementIdx];
              // We have a list so we need to go deeper
              case ListNode(contents: final List<Node> contents):
                final (
                  expandedContents,
                  didExpandInInnerList,
                ) = expandMacroUsage(name, args, replacementArgs, contents);

                if (!hasExpanded && didExpandInInnerList) hasExpanded = true;

                return ListNode(expandedContents);
              // We have a value list so we need to go deeper
              case ValueListNode(contents: final List<Node> contents):
                final (
                  expandedContents,
                  didExpandInInnerList,
                ) = expandMacroUsage(name, args, replacementArgs, contents);

                if (!hasExpanded && didExpandInInnerList) hasExpanded = true;

                return ValueListNode(expandedContents);
              // No match found, we dont do anything
              default:
                return macroBodyNode;
            }
          }).toList();

      return (expandedBodyNodes, hasExpanded);
    }

    for (final node in nodes) {
      var didExpandInLastAttempt = false;

      do {
        didExpandInLastAttempt = false;

        switch (node) {
          case ListNode(
                contents: [
                  SymbolNode(contents: final name),
                  ...final List<Node> args,
                ],
              )
              when _macroDefinitions.containsKey(name):
            final macroDefinition = _macroDefinitions[name]!;
            final (expandedBodyNodes, didExpandInAttempt) = expandMacroUsage(
              name,
              args,
              macroDefinition.replacementArgs,
              macroDefinition.bodyNodes,
            );

            didExpandInLastAttempt = didExpandInAttempt;

            if (macroDefinition.bodyNodes.length == 1 &&
                macroDefinition.bodyNodes[0] is ListNode) {
              node.contents = (expandedBodyNodes[0] as ListNode).contents;
            } else {
              node.contents = expandedBodyNodes;
            }
          case ListNode() || ValueListNode():
            _expandMacroUsages(node.contents);
          default:
        }
      } while (didExpandInLastAttempt);
    }
  }

  List<Node> expand(List<Node> astNodes) {
    _registerMacroDefinitions(astNodes);
    print("MACROS: $_macroDefinitions");

    _expandMacroUsages(astNodes);

    print("EXPANDED AST:");
    for (final node in astNodes) {
      print(node.format());
    }

    return astNodes;
  }
}
