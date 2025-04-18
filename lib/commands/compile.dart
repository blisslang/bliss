import 'dart:async';
import 'dart:io';

import 'package:args/command_runner.dart';
import 'package:bliss/compiler/categorizer.dart';
import 'package:bliss/compiler/errors.dart';
import 'package:bliss/compiler/tokenizer.dart';

class CompileCmd extends Command {
  @override
  final name = "compile";
  @override
  final description = "Compile a .bliss file into a native executable.";

  CompileCmd() {
    argParser.addFlag(
      "force",
      abbr: "f",
      help: "Skips all checks. Use with caution.",
      hideNegatedUsage: true,
    );
  }

  @override
  FutureOr? run() {
    try {
      if (argResults!.rest.length != 1) {
        throw InvalidInputFileError(
          "A file has to be supplied. Usage: bliss compile <file>",
        );
      }

      final [file] = argResults!.rest;
      final force = argResults!.flag("force");

      print(" ==> Compiling: $file");

      if (!force && !file.endsWith(".bliss")) {
        throw InvalidInputFileError(
          "File has to contain Bliss source code (using file extension .bliss)",
        );
      }

      final contents = File(file).readAsStringSync();

      print("     * Tokenizing source...");
      final tokens = tokenize(contents);

      print("     * Parsing tokens...");
      final categorizer = Categorizer();
      final ast = categorizer.categorize(tokens);

      print("     * Emitting OCaml...");

      print(" ==> Compilation finished as \u{1B}[36mmain.exe\u{1B}[0m");

      print("\n\n===== AST: =====");
      print(ast.format());
    } catch (e) {
      print("Error during compilation: $e");
    }
  }
}
