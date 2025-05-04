<script setup lang="ts">
import hljs from "highlight.js/lib/core";
import lisp from "highlight.js/lib/languages/lisp";
import reasonml from "highlight.js/lib/languages/reasonml";
import "highlight.js/styles/github.css";

const { highlight, output } = defineProps<{
  label: string;
  readonly?: boolean;
  highlight?: boolean;
  output?: boolean;
}>();

const code = defineModel<string | undefined>({ required: true });

hljs.registerLanguage("lisp", lisp);
hljs.registerLanguage("reasonml", reasonml);

const highlightedCode = computedAsync(async () =>
  hljs.highlight(code.value ?? "", {
    language: output ? "reasonml" : "lisp",
  })
);

const scrollEl = useTemplateRef<HTMLDivElement>("scroll");
const { arrivedState } = useScroll(scrollEl);

const toast = useToast();

async function handleCopyCode() {
  await navigator.clipboard.writeText(code.value ?? "");
  toast.add({
    title: "Code copied to clipboard!",
    icon: "lucide:clipboard",
    duration: 2000,
  });
}
</script>

<template>
  <div class="size-full flex-1 flex flex-col gap-1 overflow-y-auto">
    <p class="text-sm">
      {{ label }}
    </p>

    <div
      class="group relative flex-1 rounded-md ring-1 ring-inset ring-accented has-focus-visible:ring-2 has-focus-visible:ring-inset has-focus-visible:ring-primary transition-all duration-200 overflow-y-auto"
    >
      <UButton
        class="absolute z-5 right-1.5 top-1.5 md:opacity-0 group-hover:opacity-100 transition-opacity duration-200 size-7.5 place-content-center"
        variant="outline"
        color="neutral"
        @click="handleCopyCode"
      >
        <UIcon name="lucide:clipboard" class="shrink-0" />
      </UButton>

      <div
        ref="scroll"
        class="size-full overflow-auto"
        :class="{
          'mask-b-from-85% md:mask-b-from-92%': !arrivedState.bottom,
          'mask-t-from-85% md:mask-t-from-92%': !arrivedState.top,
        }"
      >
        <pre v-if="highlight" class="px-2.5 py-1.5">
<code
  v-html="highlightedCode?.value ?? code"
  class="font-code w-full text-sm gap-1.5 text-highlighted text-nowrap"
/></pre>

        <UTextarea
          v-else
          class="font-code w-full"
          :ui="{
            base: 'ring-0 focus-visible:ring-0 bg-transparent text-nowrap',
          }"
          :readonly
          autoresize
          v-model="code"
        />
      </div>
    </div>
  </div>
</template>
