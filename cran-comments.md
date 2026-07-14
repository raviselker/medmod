# medmod 1.2.0

## Resubmission after archival

medmod was archived on CRAN on 2022-04-06 as check issues were not
corrected in time. The issues from the archived check results are
resolved in this release:

* `'LazyData' is specified without a 'data' directory` — the `LazyData`
  field has been removed (the package contains no data sets).
* Test failures on some flavors (`there is no package called 'fastmap'`)
  were caused by an incomplete dependency library on the check machine,
  not by medmod itself; the test suite passes on all platforms checked
  below.
* `Namespaces in Imports field not imported from: 'R6' 'ggplot2' 'lavaan'`
  — all three are used (via `::` and `import()`); this NOTE no longer
  appears.

## Test environments

* macOS (release), Windows (release), Ubuntu (release, devel, oldrel-1)
  via GitHub Actions
* local macOS, R release

## R CMD check results

0 errors | 0 warnings

The only NOTE is the expected new-submission NOTE (package was archived
on CRAN).
