import 'dart:io';

import 'package:bliss/compiler/categorizer.dart';
import 'package:bliss/compiler/emitter.dart';
import 'package:bliss/compiler/tokenizer.dart';
import 'package:bliss/constants.dart';
import 'package:bliss/functions/stdlib.dart';
import 'package:cli_spin/cli_spin.dart';

(String, String) generate(String inputFileName, {bool noStdlib = false}) {
  final inputFile = File(inputFileName);
  final contents = inputFile.readAsStringSync();

  final lastDotIdx = inputFileName.lastIndexOf(".");
  final inputFileNameWithoutExt = inputFileName.substring(0, lastDotIdx);
  final outputFileNameWithoutExt = "$buildDir/$inputFileNameWithoutExt";

  final irOutputFileName = "$outputFileNameWithoutExt.re";
  final exeOutputFileName = "$outputFileNameWithoutExt.exe";

  print("==> Generating: $inputFileName -> $irOutputFileName");

  final spinner = CliSpin(spinner: CliSpinners.dots);
  spinner.start();

  spinner.text = "Tokenizing source...";

  final tokens = tokenize(contents);

  spinner.text = "Parsing tokens...";

  final stdlibAst = noStdlib ? null : generateStdlibAst();
  final ast = Categorizer().categorize(tokens);

  spinner.text = "Emitting IR...";

  final ir = Emitter().emit(ast, stdlibAst: stdlibAst);
  final irWithStdlib = ir;

  File(irOutputFileName)
    ..createSync(recursive: true)
    ..writeAsStringSync(irWithStdlib);

  spinner.success("Generated IR: $colorCyan$irOutputFileName$colorReset");

  return (irOutputFileName, exeOutputFileName);
}

String generateDry(String input, {bool noStdlib = false}) {
  final tokens = tokenize(input);

  final stdlibAst = noStdlib ? null : generateStdlibAst();
  final ast = Categorizer().categorize(tokens);

  final ir = Emitter().emit(ast, stdlibAst: stdlibAst, noFormatting: true);

  return ir;
}
