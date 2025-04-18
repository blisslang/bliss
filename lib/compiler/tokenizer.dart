import 'package:bliss/utils.dart';

final _whitespaceRegex = RegExp("\\s");

String _padDelims(String string) {
  return string.split("").map((c) => isDelim(c) ? " $c " : c).join();
}

List<String> tokenize(String code) {
  return _padDelims(
    code,
  ).split(_whitespaceRegex).where((x) => x.isNotEmpty).toList();
}
