import 'package:bliss/compiler/node.dart';
import 'package:embed_annotation/embed_annotation.dart';

import 'package:bliss/compiler/categorizer.dart';
import 'package:bliss/compiler/tokenizer.dart';

part 'stdlib.g.dart';

@EmbedStr("../bliss/stdlib.bliss")
const _stdlib = _$_stdlib;

Node generateStdlibAst() {
  final tokens = tokenize(_stdlib);
  final ast = Categorizer().categorize(tokens);
  return ast;
}
