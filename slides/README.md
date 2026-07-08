# slides

A home for my [Slidev](https://sli.dev) slide decks. One pnpm project, many decks —
each deck lives in its own folder under `decks/` with a `slides.md` entry file.

## Layout

```
slides/
├── package.json          # shared @slidev/cli + themes
├── decks/
│   └── <deck-name>/
│       ├── slides.md     # deck entry point
│       └── assets/       # deck-specific images, snippets, etc.
```

## Setup

```bash
cd ~/slides
pnpm install
```

## Working on a deck

Run the dev server for a specific deck (opens http://localhost:3030):

```bash
pnpm dev decks/hello-slidev/slides.md --open
```

Build a static SPA:

```bash
pnpm build decks/hello-slidev/slides.md --out decks/hello-slidev/dist
```

Export to PDF (needs `pnpm add -D playwright-chromium` first):

```bash
pnpm export decks/hello-slidev/slides.md
```

## New deck

```bash
mkdir -p decks/<name>/assets
# copy decks/hello-slidev/slides.md as a starting point
```

## Reference

- Docs: https://sli.dev
- Themes: https://sli.dev/resources/theme-gallery
