import 'package:embed_annotation/embed_annotation.dart';

import 'package:bliss/compiler/categorizer.dart';
import 'package:bliss/compiler/emitter.dart';
import 'package:bliss/compiler/tokenizer.dart';

part 'stdlib.g.dart';

@EmbedStr("../bliss/stdlib.bliss")
const _stdlib = _$_stdlib;

String compileStdlib() {
  final tokens = tokenize(_stdlib);
  final ast = Categorizer().categorize(tokens);
  final emittedStdlib = Emitter().emit(ast, noMain: true);
  return emittedStdlib;
}
