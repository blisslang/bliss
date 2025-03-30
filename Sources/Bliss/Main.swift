import ArgumentParser
import SwiftPrettyPrint

@main
struct Bliss: ParsableCommand {
    static let configuration = CommandConfiguration(
        abstract: "The compiler and toolchain for the Bliss programming language.",
        subcommands: [Compile.self]
    )
}

extension Bliss {
    struct Compile: ParsableCommand {
        static let configuration = CommandConfiguration(
            abstract: "Compile a .bliss file to a native executable.",
            discussion:
                "Takes in a positional argument of the file to compile. If the file does not exist or is not a .bliss source file it stops immediately."
        )

        @Argument(help: "The .bliss file to compile.")
        var file: String

        @Flag(help: "Skip all checks. Use with caution.")
        var force: Int

        mutating func run() throws {
            print(" ==> Compiling: \(file)")

            guard force != 0 || file.hasSuffix(".bliss") else {
                throw CompilerError.invalidInputFileError(
                    "\(file): File has to contain Bliss source code (using file extension .bliss)")
            }

            let contents = try String(contentsOfFile: file, encoding: .utf8)

            print("     * Tokenizing source...")
            let tokenizer = Tokenizer()
            let tokens = tokenizer.tokenize(code: contents)

            print("     * Parsing tokens...")
            var categorizer = Categorizer()
            let ast = try categorizer.categorize(tokens: tokens)

            print("     * Emitting OCaml...")

            print(" ==> Compilation finished as \u{1B}[36mmain.exe\u{1B}[0m")

            Pretty.prettyPrint(label: "AST", ast, option: Pretty.Option(colored: true))
        }
    }
}
