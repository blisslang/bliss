import 'dart:io';

import 'package:bliss/compiler/categorizer.dart';
import 'package:bliss/compiler/emitter.dart';
import 'package:bliss/compiler/tokenizer.dart';
import 'package:bliss/constants.dart';
import 'package:bliss/functions/stdlib.dart';
import 'package:cli_spin/cli_spin.dart';

Future<(String, String)> generate(
  String inputFileName, {
  bool noStdlib = false,
}) async {
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

  spinner.text = "Emitting Dart...";

  final emittedCode = await Emitter().emit(ast, stdlibAst: stdlibAst);

  final codeWithStdlib = emittedCode;

  File(irOutputFileName)
    ..createSync(recursive: true)
    ..writeAsStringSync(codeWithStdlib);

  spinner.success(
    "Generated Dart code: $colorCyan$irOutputFileName$colorReset",
  );

  return (irOutputFileName, exeOutputFileName);
}
