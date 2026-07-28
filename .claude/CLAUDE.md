# CLAUDE.md

This repo keeps its agent & contributor guidance in **[`AGENTS.md`](../AGENTS.md)** so the same
content is tool-agnostic and shared across every agent.

**👉 Read [`AGENTS.md`](../AGENTS.md)** for repo-local orientation (architecture, build & test,
gotchas) and the AusTraits-family cross-package pointer.

Don't duplicate that content here — edit `AGENTS.md` and this file stays correct by reference.

---

## Working here

Process only. Anything about *what this repo contains* goes in `AGENTS.md`.

- **`make check` before you finish.** It rebuilds, validates and runs the tests. Green means no
  regression; the "known gap" lines are pre-existing published defects, listed in
  [`COMMITMENTS.md`](../COMMITMENTS.md), and are expected.
- **Read [`plans/`](../plans/) before starting anything substantial.** The plan documents there are
  kept current, including where they turned out to be wrong — status headers and per-stage notes
  record what actually happened. Don't reconstruct state from `git log`.
- **The published output is a contract.** Verify before you change it, and keep a change to generated
  artefacts in its own commit, separate from the refactor that caused it — `tests/testthat/test-golden.R`
  compares a fresh build against the committed artefacts, so an unintended change fails loudly.
- **Never hand-edit `export/`, `docs/` or `release/`.** All generated. Edit `data/` and rebuild.
- **Published URLs never move.** They are what the paper, the w3id redirects and downstream packages
  resolve. `COMMITMENTS.md` C12 is the list; changing where a file sits in this repo is fine, changing
  where it is served from is not.
