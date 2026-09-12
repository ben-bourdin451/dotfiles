<script setup lang="ts">
// A copy-able prompt "snippet" card. Body comes from the slot so Markdown
// inside still renders (paragraphs, lists, bold).
defineProps<{
  label?: string
  tone?: 'a' | 'b'
}>()
</script>

<template>
  <div class="snippet" :class="tone ? `snippet-${tone}` : ''">
    <div class="snippet-head">
      <span class="snippet-label">{{ label || 'Prompt' }}</span>
    </div>
    <div class="snippet-body"><slot /></div>
  </div>
</template>
