import { execFile } from "child_process";
import { PassThrough, Readable } from "stream";

const combineStreams = (sources: Readable[]): Readable => {
  let passStream = new PassThrough({ objectMode: true });
  for (const stream of sources) {
    const end = stream == sources.at(-1);
    passStream = stream.pipe(passStream, { end });
  }
  return passStream;
};

export default defineEventHandler(async (event) => {
  const query = getQuery(event);
  const input = query.input as string | undefined;

  if (!input)
    return { status: "error", message: "Missing query parameter 'input'." };

  let stderrStream: Readable | null;
  let stdoutStream: Readable | null;

  try {
    const child = execFile("./node_modules/bs-platform/bsrefmt");

    child.stdin?.write(input);
    child.stdin?.end();

    stderrStream = child.stderr;
    stdoutStream = child.stdout;
  } catch (err) {
    console.error("bsrefmt formatting error:", err);
    return { status: "error", message: "Failed to format." };
  }

  if (!stderrStream || !stdoutStream)
    return { status: "error", message: "Failed to format." };

  return combineStreams([stderrStream, stdoutStream]);
});
