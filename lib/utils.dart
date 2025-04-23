import 'package:bliss/compiler/emitter.dart';

const _delimiters = "[]();\"";

bool isDelim(String c) {
  return _delimiters.contains(c);
}

String camelize(String input) {
  return input.split("-").indexed.map((elem) {
    return elem.$1 > 0
        ? "${elem.$2[0].toUpperCase()}${elem.$2.substring(1)}"
        : elem.$2;
  }).join();
}

final _validIdentifierCharsRegex = RegExp("[a-zA-Z0-9\$]");

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
                moduleAndObjectSeparators.contains(c)
                    ? "$acc."
                    : "$acc\$\$${c.runes.first}\$\$",
          );
    },
  );
}
