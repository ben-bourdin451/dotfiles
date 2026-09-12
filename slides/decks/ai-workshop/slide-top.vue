<script setup lang="ts">
import { computed, toValue } from "vue";
import { useSlideContext } from "@slidev/client";

// This lives in slide-top.vue rather than global-top.vue on purpose.
// SlideTop renders *inside* SlideWrapper, which is what provides `$route` —
// this slide's own route. GlobalTop renders as a sibling of that wrapper, so it
// can only read the global nav state; during an export that reads as page 1 for
// every slide, the section resolves to null and the indicator disappears from
// every exported PDF/PNG. Slidev documents the same workaround.
const { $slidev, $route, $zoom } = useSlideContext();

// Section order → the number shown in the footer.
const ORDER = [
  "Prompting",
  "Agents",
  "Skills",
  "Loops",
  "Graph engineering",
  "Build",
];

// Each `layout: section` divider declares `section:` in its frontmatter;
// content slides inherit the most recent divider's section.
const section = computed(() => {
  const page = $route?.no ?? 0;
  let current: string | null = null;
  for (const r of toValue($slidev.nav.slides)) {
    if (r.no <= page && r.meta?.slide?.frontmatter?.section)
      current = r.meta.slide.frontmatter.section;
  }
  return current;
});

const num = computed(() =>
  section.value ? ORDER.indexOf(section.value) + 1 : 0,
);

// SlideWrapper scales its contents by the slide's `zoom`, so without this the
// indicator would shrink on the few slides that set one.
const unzoom = computed(() => ({
  scale: String(1 / toValue($zoom)),
  transformOrigin: "bottom left",
}));
</script>

<template>
  <!-- Section indicator, bottom-left (mirrors the logo's height & margin) -->
  <div
    v-if="section"
    :style="unzoom"
    class="section-indicator absolute bottom-0 left-0 m-4 h-5 flex items-center gap-2 pointer-events-none select-none"
  >
    <span class="section-num">{{ num }}</span>
    <span>{{ section }}</span>
  </div>
</template>

<style scoped>
.section-indicator {
  font-size: 0.7rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  opacity: 0.6;
}
.section-num {
  font-weight: 700;
  color: #ffbb1a;
}
</style>
