import 'dart:io';

import 'package:bliss/compiler/categorizer.dart';
import 'package:bliss/compiler/emitter.dart';
import 'package:bliss/compiler/tokenizer.dart';
import 'package:bliss/constants.dart';
import 'package:bliss/functions/stdlib.dart';
import 'package:cli_spin/cli_spin.dart';

(String, String) generate(String inputFileName, {bool noStdlib = false}) {
  final contents = File(inputFileName).readAsStringSync();

  final lastDotIdx = inputFileName.lastIndexOf(".");
  final inputFileNameWithoutExt = inputFileName.substring(0, lastDotIdx);
  final outputFileNameWithoutExt = "$buildDir/$inputFileNameWithoutExt";

  final dartOutputFileName = "$outputFileNameWithoutExt.dart";
  final exeOutputFileName = "$outputFileNameWithoutExt.exe";

  print("==> Generating: $inputFileName -> $dartOutputFileName");

  final spinner = CliSpin(spinner: CliSpinners.dots);
  spinner.start();

  spinner.text = "Tokenizing source...";

  final tokens = tokenize(contents);

  spinner.text = "Parsing tokens...";

  final categorizer = Categorizer();
  final ast = categorizer.categorize(tokens);

  spinner.text = "Emitting Dart...";

  final emitter = Emitter();
  final emittedCode = emitter.emit(ast);

  final compiledStdlib =
      noStdlib
          ? ""
          : "//BEGIN Bliss stdlib\n${compileStdlib()}//END Bliss stdlib\n\n";

  final codeWithStdlib = "$compiledStdlib$emittedCode";

  File(dartOutputFileName)
    ..createSync(recursive: true)
    ..writeAsStringSync(codeWithStdlib);

  spinner.success(
    "Generated Dart code: $colorCyan$dartOutputFileName$colorReset",
  );

  return (dartOutputFileName, exeOutputFileName);
}
