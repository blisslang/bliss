import argv
import glint
import lib/functions/generate

const name = "bliss"

const description = "The compiler and toolchain for the Bliss Programming Language."

fn generate_command() -> glint.Command(Nil) {
  use <- glint.command_help("Generates IR from a .bliss file.")
  use file_arg <- glint.named_arg("file")
  use named, _args, _flags <- glint.command()

  let file = file_arg(named)

  generate.generate(file)
}

pub fn main() -> Nil {
  glint.new()
  |> glint.with_name(name)
  |> glint.global_help(description)
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: ["generate"], do: generate_command())
  |> glint.run(argv.load().arguments)
}
