# CLAUDE.md

Guidance for Claude Code working in this repository. This file is **specific to
medmod**; general jamovi conventions live in the skills below (and this file
wins where they disagree).

## Overview

**medmod** is a jamovi module and R package for simple mediation (`med`) and
moderation (`mod`) analysis, built on `lavaan`. Both surfaces wrap the same
lavaan backend, so changes generally need to land in both the R package
metadata (`DESCRIPTION`, `NAMESPACE`, `man/`) and the jamovi metadata
(`jamovi/*.yaml`).

## Skills

This repo follows the jamovi team's opt-in skills — use them here:
`jamovi-module` (module mechanics), `jamovi-dev-standards` (commit + test
conventions), `jamovi-r-standards` (air, renv, testthat). If they aren't loaded,
install the `jamovi-module` and `jamovi-dev-standards` plugins first.

> **Runtime caveat:** as a jamovi module, medmod runs inside jamovi's own
> bundled R and package library. The real integration check is
> `jmvtools::install()` + running in jamovi; dev-environment results can
> differ.

## Commands

```r
Rscript -e 'testthat::test_package("medmod")'   # run all tests
testthat::test_file("tests/testthat/testmed.R") # run one test file

jmvtools::prepare()   # regenerate R/*.h.R after editing jamovi/*.yaml
jmvtools::install()   # build .jmo and install into a local jamovi
```

`air format .` formats R code (config in `air.toml`; generated `*.h.R` files
are excluded).

`renv::restore()` bootstraps the dev library from `renv.lock`; the lockfile is
driven by `DESCRIPTION` (`renv::snapshot(type = "explicit")`).

## Architecture

For each analysis (`med`, `mod`) there are paired files:

- `jamovi/<name>.a.yaml` — options; `<name>.r.yaml` — results (tables, images);
  `<name>.u.yaml` — UI layout.
- `R/<name>.h.R` — **generated** R6 classes and the public `med()`/`mod()`
  function. Never hand-edit; regenerate with `jmvtools::prepare()`. The roxygen
  on `med`/`mod` comes from the YAML's `description.R` block — edit the YAML,
  then `devtools::document()` to refresh `man/`.
- `R/<name>.b.R` — the analysis body: `<name>Class` implements `.init()`,
  `.run()`, `.compute()`, `.populate*Table()`, `.prepare*Plot()`,
  `.cleanData()`, and `.lavaanify()`.

Flow per analysis: `.lavaanify()` builds a lavaan model-syntax string with
labeled paths (`a`, `b`, `c` for mediation; `b1`, `b2`, `b3` for moderation).
`.compute()` calls `lavaan::sem()`; the `.populate*()` methods pull labeled
rows out of `lavaan::parameterestimates()` into the results tables. Variable
names are base64-encoded via `jmvcore::toB64()` before going to lavaan, and the
table-population code looks rows up by the encoded names — keep that round-trip
consistent when adding variables to a model. The moderation backend
additionally mean-centers all inputs in `.cleanData()` (helper in `R/utils.R`)
and constructs the interaction term manually.

`jamovi/0000.yaml` is the module manifest (version, menu entries, minApp). A
version bump edits it **and** `DESCRIPTION`.

## Testing

Tests live in `tests/testthat/` (`testmed.R`, `testmod.R`) and call the
exported `med()`/`mod()` functions the way an R user would, asserting table
values (`r$med$asDF`) against known estimates with numeric tolerances. Seed any
randomness (`set.seed()`) so estimates stay reproducible. Add a test for any
new option or table.
