import argv
import glint
import lib/functions/generate

const name = "bliss"

const description = "The compiler and toolchain for the Bliss Programming Language."

fn generate_command() -> glint.Command(Nil) {
  use <- glint.command_help("Generates IR from a .bliss file.")

  use file_arg <- glint.named_arg("file")

  use debug_flag <- glint.flag(
    glint.bool_flag("debug")
    |> glint.flag_default(False)
    |> glint.flag_help("Print debug information during generation"),
  )

  use no_prelude_flag <- glint.flag(
    glint.bool_flag("no-prelude")
    |> glint.flag_default(False)
    |> glint.flag_help("Do not include the prelude in the generated code"),
  )

  use named, _args, flags <- glint.command()

  let file = file_arg(named)

  let assert Ok(debug) = debug_flag(flags)

  let assert Ok(no_prelude) = no_prelude_flag(flags)

  generate.generate(file, debug:, no_prelude:)
}

pub fn main() -> Nil {
  glint.new()
  |> glint.with_name(name)
  |> glint.global_help(description)
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: ["generate"], do: generate_command())
  |> glint.run(argv.load().arguments)
}
