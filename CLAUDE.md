# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) and other agents when working with code in this repository.

This repository is a club/community-maintained list of quant internships. Contributors edit one YAML file per company in `data/`; an OCaml generator (`src/`) renders them into `README.md`.

## Critical rules

- **Never edit `README.md` by hand** — CI regenerates and commits it (`[bot] auto-update README.md`) after every merge to `main`. All content changes go in `data/*.yaml`.
- YAML field names must match the schema in `src/lib/types.ml` exactly (`name`, `website`, `locations`, `notes`, `roles`; per role: `role_type`, `links`; per link: `url`, optional `label`). A misspelled field passes yamllint but the generator **silently drops the entire company** from the README.
- `roles` is required. A company with no open roles must use `roles: []` — never omit the field or use `null`.
- README ordering follows the ASCII sort of the **filename** (kebab-case, `.yaml` extension), not the `name` field.
- Prefer job URLs on the firm's own careers domain; a Greenhouse/Lever/other ATS link is fine when the firm has no equivalent page on its own site.
- `role_type` must be one of: `SWE`, `QT`, `QD`, `QR`, `PhD`, `HW`, `FPGA`. All links for a category are grouped under a single `role_type` block per file — never repeat a category (see `data/optiver.yaml` for the pattern). Other role types may be okay as the industry progresses, but please note this in the commit message, or tell your human to include it.
- `label` on a link is only for disambiguating multiple links under one `role_type` (e.g. "Chicago", "Austin (PhD)"); omit it for a single link.

## Commands

- Lint a data file (same as CI): `yamllint -d relaxed data/<file>.yaml`. You may need to install this via a Python package manager. A reasonable default is `uv`, but please ask your human for their preferences, as there is no Python package in this repo.
- Regenerate README locally (requires OCaml/opam toolchain): `cd src && dune exec internships > ../README.md` — useful to verify a data file parses, but don't rely on committing the result; CI regenerates it on merge.
- Build the generator: `cd src && dune build`; format OCaml code: `dune fmt`. Most contributers **will not** have a local OCaml development environment. In this case use your best judgement, and put up the code for review.

Schema documentation with examples: `data/README.md` or just go read the files in `./data`.
