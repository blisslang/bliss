import 'dart:convert';
import 'dart:io';

import 'package:bliss/compiler/emitter.dart';

const _delimiters = "[]();\"";

bool isDelim(String c) {
  return _delimiters.contains(c);
}

String snakeCasify(String input) {
  return input.replaceAll("-", "_");
}

final _validIdentifierCharsRegex = RegExp("[a-zA-Z0-9_]");

String escapeInvalidChars(String input) {
  // If the identifier only consists of underscores we let it through so you can
  // for example ignore the name of a parameter by naming it '_'
  if (input.split("").every((c) => c == "_")) return input;

  return input.splitMapJoin(
    _validIdentifierCharsRegex,
    onNonMatch: (chars) {
      if (chars.isEmpty) return "";

      return chars
          .split("")
          .fold(
            "",
            (acc, c) =>
                moduleSeparator == c ? "$acc." : "${acc}__${c.runes.first}__",
          );
    },
  );
}

Future<(String, int)> runCommandWithStdinAndFallbackValueOnError(
  String command,
  List<String> args, {
  List<String>? stdin,
  required String valueOnError,
}) async {
  try {
    final process = await Process.start(command, args);

    stdin?.forEach(process.stdin.write);
    await process.stdin.close();

    final futureExitCode = process.exitCode;
    final futureStdOut = process.stdout.transform(utf8.decoder).join();
    final futureStdErr = process.stderr.transform(utf8.decoder).join();

    final results = await Future.wait([
      futureExitCode,
      futureStdOut,
      futureStdErr,
    ]);

    final exitCode = results[0] as int;
    final stdOutput = results[1] as String;
    final stdError = results[2] as String;

    if (stdError.isNotEmpty) {
      print("$command STDERR: $stdError");
    }

    if (exitCode != 0) {
      print(
        "$command failed with exit code $exitCode. Returning original input.",
      );
      if (stdOutput.isNotEmpty) {
        print("$command STDOUT (on failure): $stdOutput");
      }
      return (valueOnError, exitCode);
    }

    print("$command STDOUT (on success): $stdOutput");

    return (stdOutput, exitCode);
  } on ProcessException {
    return (valueOnError, exitCode);
  }
}
