<script setup lang="ts">
import { computed, toValue } from "vue";
import { useNav } from "@slidev/client";

const nav = useNav();

// Section order → the number shown in the footer.
const ORDER = [
  "LLMs",
  "Tooling",
  "Myths",
  "Failure modes",
  "Engineering basics",
];

// Each `layout: section` divider declares `section:` in its frontmatter;
// content slides inherit the most recent divider's section.
const section = computed(() => {
  const page = toValue(nav.currentPage);
  let current: string | null = null;
  for (const r of toValue(nav.slides)) {
    if (r.no <= page && r.meta?.slide?.frontmatter?.section)
      current = r.meta.slide.frontmatter.section;
  }
  return current;
});

const num = computed(() =>
  section.value ? ORDER.indexOf(section.value) + 1 : 0,
);
</script>

<template>
  <!-- Section indicator, bottom-left (mirrors the logo's height & margin) -->
  <div
    v-if="section"
    class="section-indicator absolute bottom-0 left-0 m-4 h-5 flex items-center gap-2 pointer-events-none select-none"
  >
    <span class="section-num">{{ num }}</span>
    <span>{{ section }}</span>
  </div>

  <!-- Superthread logo, bottom-right on every slide -->
  <img
    src="/superthread-logo.svg"
    alt="Superthread"
    class="absolute bottom-0 right-0 m-4 h-5 w-auto opacity-60 pointer-events-none select-none"
  />
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
