import 'dart:async';

import 'package:args/command_runner.dart';
import 'package:bliss/compiler/errors.dart';
import 'package:bliss/functions/compile.dart';

class CompileCmd extends Command {
  @override
  final name = "compile";
  @override
  final description = "Compile a .bliss file into a native executable.";

  CompileCmd() {
    argParser
      ..addFlag(
        "stdlib",
        help: "Include the Bliss Standard Library in the compiled code.",
        defaultsTo: true,
      )
      ..addFlag(
        "force",
        abbr: "f",
        help: "Skips all checks. Use with caution.",
        hideNegatedUsage: true,
      );
  }

  @override
  FutureOr? run() async {
    try {
      if (argResults!.rest.length != 1) {
        throw InvalidInputFileError(
          "A file has to be supplied. Usage: bliss compile <file>",
        );
      }

      final [inputFileName] = argResults!.rest;
      final force = argResults!.flag("force");
      final useStdlib = argResults!.flag("stdlib");

      if (!force && !inputFileName.endsWith(".bliss")) {
        throw InvalidInputFileError(
          "File has to contain Bliss source code (using file extension .bliss)",
        );
      }

      await compile(inputFileName, noStdlib: !useStdlib);
    } catch (e) {
      print("Error during compilation:");
      rethrow;
    }
  }
}
