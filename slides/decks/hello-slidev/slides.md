---
theme: seriph
title: Hello Slidev
info: |
  ## Hello Slidev
  A dummy deck to verify the ~/slides setup works.
class: text-center
transition: slide-left
mdc: true
---

# Hello Slidev 👋

A dummy deck to confirm the `~/slides` setup works

<div class="pt-8 opacity-70 text-sm">
  Press <kbd>space</kbd> to advance ·
  <kbd>f</kbd> for fullscreen
</div>

<!--
Presenter note: this deck exists only to smoke-test the Slidev install.
-->

---
layout: default
---

# What this proves

<v-clicks>

- Slidev is installed and runs from `~/slides`
- Multiple decks can live side by side under `decks/`
- Markdown, animations, and code highlighting all render

</v-clicks>

<div v-click class="mt-8 text-emerald-400">

If you can read this line after a few clicks, click animations work. ✅

</div>

---

# Code highlighting

```ts {2-3|5|all}
function greet(name: string): string {
  const greeting = `Hello, ${name}!`
  return greeting
}

console.log(greet('Slidev'))
```

Line-by-line reveals driven by clicks.

---
layout: center
class: text-center
---

# That's it 🎉

Setup verified — start building real decks.

[sli.dev](https://sli.dev)
