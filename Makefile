run-wasm-test:
	wat2wasm examples/wasm-test.wat -o examples/wasm-test.wasm
	wasmedge examples/wasm-test.wasm