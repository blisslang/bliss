<script setup lang="ts">
declare global {
  interface Window {
    generate: (input: string) => string;
  }
}

const examples = [
  {
    label: "Recursive Fibonacci",
    value: "rec_fib",
    code: `
(def fib [n]
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
`,
  },
].map(({ code, ...obj }) => ({ code: code.trim(), ...obj }));

const inputCode = useLocalStorage("input-code", "");

const shouldFormatCode = useLocalStorage("format-output", false);
const throttleAmount = 1000;

const outputCode = ref("");
watch(
  [inputCode, shouldFormatCode],
  async ([inputCode, shouldFormatCode]) =>
    (outputCode.value =
      (await getOutputCode(inputCode, shouldFormatCode)) ?? "")
);

const currentExample = ref("");

watch(currentExample, (exampleId) => {
  const example = examples.find((x) => x.value === exampleId);
  if (!example) return;

  inputCode.value = example.code;
});

async function getOutputCode(inputCode: string, shouldFormatCode: boolean) {
  if (inputCode.length === 0) return;

  try {
    const ir = window.generate(inputCode);

    if (!shouldFormatCode) {
      return ir;
    } else {
      return await getFormattedIr(ir);
    }
  } catch (err: any) {
    return "Error while compiling.";
  }
}

const getFormattedIr = useThrottleFn(async (ir: string) => {
  const res = await $fetch("/api/format-reason", {
    method: "GET",
    params: { input: ir },
  });

  if (typeof res !== "string" && "status" in res && res.status === "error") {
    console.error("Reason formatting error:", res.message);
    return ir;
  }

  return res as unknown as string;
}, throttleAmount);

async function loadWasmModule() {
  let dart2wasm_runtime;
  let moduleInstance;
  try {
    const dartModulePromise = WebAssembly.compileStreaming(
      fetch("/api/bliss-wasm")
    );
    const imports = {};
    dart2wasm_runtime = await import("../bliss-wasm/bliss.mjs");
    moduleInstance = await dart2wasm_runtime.instantiate(
      dartModulePromise,
      imports
    );
  } catch (err) {
    console.error(`Failed to fetch and instantiate wasm module: ${err}`);
    console.error("See https://dart.dev/web/wasm for more information.");
  }

  dart2wasm_runtime!.invoke(moduleInstance);
}

onMounted(async () => {
  await loadWasmModule();

  outputCode.value =
    (await getOutputCode(inputCode.value, shouldFormatCode.value)) ?? "";
});
</script>

<template>
  <div
    class="w-full h-dvh overflow-y-hidden flex flex-col gap-8 pt-6 p-4 md:p-8"
  >
    <div class="flex flex-col gap-1 justify-center items-center">
      <h1
        class="text-[2.5rem] md:text-5xl font-bold font-title tracking-tighter bg-linear-[in_oklab_140deg,var(--ui-text)_50%,color-mix(in_oklab,var(--ui-text-muted)_90%,white)] leading-12"
        style="
          background-clip: text;
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
        "
      >
        Bliss Playground
      </h1>
      <h4 class="text-muted">The place where your dreams become reality.</h4>
    </div>

    <div class="flex flex-col gap-4 flex-1 overflow-y-auto">
      <div class="inline-flex gap-4">
        <UFormField
          label="Examples"
          class="flex flex-col justify-evenly"
          :ui="{ container: 'inline-flex gap-2 items-center' }"
        >
          <USelectMenu
            v-model="currentExample"
            valueKey="value"
            :items="examples"
            class="w-44"
            placeholder="View code examples"
            :ui="{
              trailingIcon:
                'group-data-[state=open]:rotate-180 transition-transform duration-200',
            }"
          />

          <UTooltip text="Clear example">
            <UButton
              class="size-8 grid place-content-center"
              icon="lucide:x"
              variant="outline"
              color="neutral"
              :ui="{ leadingIcon: '!size-4' }"
              @click="
                currentExample = '';
                inputCode = '';
              "
            />
          </UTooltip>
        </UFormField>

        <UTooltip text="Beware that formatting happens on the server">
          <UFormField
            label="Format output"
            class="flex flex-col justify-evenly"
          >
            <USwitch v-model="shouldFormatCode" />
          </UFormField>
        </UTooltip>
      </div>

      <div
        class="flex flex-col flex-1 md:inline-flex md:flex-row gap-2 justify-center items-center overflow-y-auto"
      >
        <CodeTextBox label="Bliss" v-model="inputCode" />

        <CodeTextBox
          label="IR"
          readonly
          highlight
          v-model="outputCode"
          output
        />
      </div>
    </div>
  </div>
</template>
