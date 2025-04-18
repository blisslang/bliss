import "package:bliss/commands/root.dart";

void main(List<String> args) {
  final runner = rootCmd();
  runner.run(args);
}
