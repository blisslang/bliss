import 'dart:io';

import 'package:bliss/constants.dart';
import 'package:bliss/functions/generate.dart';
import 'package:cli_spin/cli_spin.dart';

Future<void> compile(String inputFileName) async {
  final (dartOutputFileName, exeOutputFileName) = generate(inputFileName);

  print("==> Compiling: $dartOutputFileName -> $exeOutputFileName");

  final spinner = CliSpin(spinner: CliSpinners.dots);
  spinner.start();

  spinner.text = "Building native executable...";

  final res = await Process.run("dart", [
    "compile",
    "exe",
    dartOutputFileName,
    "-o",
    exeOutputFileName,
  ]);

  if (res.exitCode == 1) throw "Compilation failed ${res.stderr}";

  spinner.success(
    "Compilation finished as: $colorCyan$exeOutputFileName$colorReset",
  );
}
