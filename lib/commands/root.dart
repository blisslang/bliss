import "package:args/command_runner.dart";
import 'package:bliss/commands/compile.dart';

CommandRunner rootCmd() {
  final runner = CommandRunner(
    "bliss",
    "The compiler and toolchain for the Bliss Programming Language.",
  )..addCommand(CompileCmd());

  return runner;
}
