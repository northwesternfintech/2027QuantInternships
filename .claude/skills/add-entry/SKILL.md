---
name: add-entry
description: Add a new company or new roles/links to the 2027 quant internships list in data/*.yaml. Use when the user wants to add an internship posting, a company, a role, or an application link.
---

Add or update an entry in `data/`. The user's request: $ARGUMENTS

## Steps

1. **Identify the company file.** Files are kebab-case of the company name with a `.yaml` extension (e.g. `data/citadel-securities.yaml`, `data/hudson-river-trading.yaml`). If the file exists, add to it; otherwise create it. Filename determines README position (ASCII sort), so name it after the company. Sections that must appear first/last can be named aptly as `aa-something.yaml` or `zz-something.yaml`.

2. **Verify the URLs.** Prefer links on the firm's own careers domain; if given a Greenhouse/Lever/Ashby/Workday ATS link, check for an equivalent page on the firm's official site, but the ATS link is fine when none exists. Job-board APIs (e.g. `https://boards-api.greenhouse.io/v1/boards/<company>/jobs`) are useful for enumerating postings when careers pages are JavaScript-rendered. If a role is described without a URL, search the firm's careers site for the posting. For companies with a lot of postings, spawn an agent to independently verify your links are correct.

3. **Write the entry** following the schema exactly (field names come from `src/lib/types.ml`; a wrong field name silently drops the company from the README):

   ```yaml
   name: "Anonymous Trading Firm"
   website: "https://www.example.com/"
   locations: "Boston"
   notes: "One-liner of community knowledge about the firm."
   roles:
     - role_type: "QT"
       links:
         - url: "link"

     - role_type: "QR"
       links:
         - url: "link"
         - url: "link"
           label: "PhD"
   ```

   Conventions:
   - Double-quote all scalar values; two-space indentation; blank line between roles.
   - `role_type` must be one of the accepted categories: `SWE`, `QT`, `QD`, `QR`, `PhD`, `HW`, `FPGA`.
   - **One block per category**: all links for a category go under a single `role_type` entry (see `data/optiver.yaml`). Never repeat a `role_type` in the same file — when adding a link for a category that already exists, append it to that block's `links`.
   - `label` only when a `role_type` has multiple links and they need disambiguating (e.g. "Chicago", "Austin (PhD)"); omit otherwise.
   - No open roles yet: `roles: []` (the field is required — never omit it).
   - Do not touch `README.md`; CI regenerates it after merge.

4. **Validate:** run `yamllint -d relaxed data/<file>.yaml` (same check as CI). If the OCaml toolchain is available, `cd src && dune exec internships > /dev/null` also confirms the file parses — check stderr for a dropped-file warning mentioning the company.

5. **Report** which file changed and which roles/links were added. Commit messages follow the existing style, e.g. `Add IMC and Optiver postings for 2027`.
