<template>
  <!-- Superthread logo, bottom-right on every slide.
       Unconditional, so it renders correctly from the global layer. Anything
       that depends on *which* slide is showing belongs in slide-top.vue —
       see the note there. -->
  <img
    src="/superthread-logo.svg"
    alt="Superthread"
    class="absolute bottom-0 right-0 m-4 h-5 w-auto opacity-60 pointer-events-none select-none"
  />
</template>
