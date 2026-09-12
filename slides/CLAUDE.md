# CLAUDE.md

Slidev decks for Superthread workshops. One pnpm project, one deck per folder under `decks/<name>/slides.md`. See `README.md` for layout.

## Running Slidev

- Do not use `pnpm dev`, `pnpm build` or `pnpm export`. In this repo they trigger a pnpm install step that fails with `ERR_PNPM_IGNORED_BUILDS` (playwright-chromium's build script is not approved) before Slidev runs.
- Call the binary directly instead:

```bash
./node_modules/.bin/slidev decks/<name>/slides.md --open
./node_modules/.bin/slidev build decks/<name>/slides.md --out dist
./node_modules/.bin/slidev export decks/<name>/slides.md --output decks/<name>/<name>.pdf --wait 800
```

- The first PDF export can produce a blank file of roughly 9KB. Check the size and re-run with `--wait 800` if so; a correct export is several hundred KB.
- To eyeball every slide for overflow, export PNGs with `--format png` and review them.

## Deck conventions

- New decks copy `public/`, `styles/index.css`, `global-top.vue` and `slide-top.vue` from the most recent deck, then update the `ORDER` list of sections in `slide-top.vue`.
- Anything that depends on *which* slide is showing (the section indicator) belongs in `slide-top.vue`, not `global-top.vue`. `GlobalTop` renders outside the per-slide wrapper, so during an export every slide reads as page 1 and the content silently disappears from the PDF/PNGs. `global-top.vue` is for unconditional chrome like the logo.
- **Slide subtitles get at least `mb-3`.** The one-line subtitle under an `# H1` is `<div class="opacity-80 -mt-1 mb-3">…</div>`; tighter values like `mb-1`/`mb-2` crowd the body. Go larger (`mb-4`) on sparse slides. Exceptions are fine where a slide genuinely needs the room — just don't let `mb-1`/`mb-2` become the default.
- Shared look: seriph theme, dark, Vela Sans (self-hosted in `public/fonts`), titles in `#ffbb1a`, Superthread logo bottom-right, `canvasWidth: 975`, hidden `about-me.md` import after the cover.
