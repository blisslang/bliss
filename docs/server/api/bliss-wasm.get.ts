import { readFile } from "fs/promises";

export default defineEventHandler(async (event) => {
  const wasmModuleContents = await readFile("../build/wasm/bliss.wasm");
  event.node.res.setHeader("Content-Type", "application/wasm");
  return wasmModuleContents;
});
