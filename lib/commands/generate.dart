import 'dart:async';

import 'package:args/command_runner.dart';
import 'package:bliss/compiler/errors.dart';
import 'package:bliss/functions/generate.dart';

class GenerateCmd extends Command {
  @override
  final name = "generate";
  @override
  final description = "Generate Dart code from a .bliss file.";

  GenerateCmd() {
    argParser.addFlag(
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

      if (!force && !inputFileName.endsWith(".bliss")) {
        throw InvalidInputFileError(
          "File has to contain Bliss source code (using file extension .bliss)",
        );
      }

      generate(inputFileName);
    } catch (e) {
      print("Error during compilation:");
      rethrow;
    }
  }
}
