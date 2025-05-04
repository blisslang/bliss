import "dart:js_interop";
import 'dart:js_interop_unsafe';

import 'package:bliss/functions/generate.dart';
import 'package:web/web.dart';

String _generate(String input) {
  return generateDry(input);
}

void main() {
  window.setProperty("generate".toJS, _generate.toJS);
}
