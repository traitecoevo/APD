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
  `docs/` is gitignored, so if you find yourself staging it something has gone wrong. A shipped
  `release/<version>/` is an archive — `make release` refuses to overwrite one, and
  `APD_FORCE_RELEASE=1` is only for a release you are still preparing.
- **Published URLs never move.** They are what the paper, the w3id redirects and downstream packages
  resolve. `COMMITMENTS.md` C12 is the list; changing where a file sits in this repo is fine, changing
  where it is served from is not.
- **Cutting a release? Follow [`RELEASING.md`](../RELEASING.md) step by step, in order.** Four of its
  steps gate the ones after them, and 2.1.2 went wrong in three places by treating the list as
  unordered. The one that bites hardest: `git merge --ff-only develop` merges your *local* `develop`,
  so merging the release PR in the browser without pulling first publishes the commit *before* the
  release — and it succeeds, so nothing warns you.
- **Verify against the live service, not against the repo**, whenever the claim is about resolution or
  publication. `scripts/check_redirects.sh` is the tool. Several things here were believed for months
  on the strength of a plausible-looking config: 819 identifiers that did not resolve, a permalink that
  404'd, and two Zenodo releases nobody had deposited because the docs said an integration handled it.
