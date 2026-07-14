# medmod 1.2.0

## New features

* Both analyses can now display a path diagram of the fitted model, with
  optional path labels, coefficient estimates and significance stars. The
  moderation diagram optionally draws the moderator's main-effect path.
* The moderation estimates table can now display coefficient labels
  matching the simple slope parametrisation.
* Model estimates are cached, so changing display options no longer refits
  the model (particularly noticeable with bootstrap standard errors).
* Added two example datasets to jamovi's data library, one for mediation
  and one for moderation.

## Bug fixes

* Results now correctly refresh when the confidence level or the number of
  bootstrap samples changes.
* The percent mediation column now uses the same estimates as the other
  columns (no change in results for these models, where the two coincide).
* Replaced deprecated ggplot2 idioms (`aes_string()`, `size` for line
  widths).
* Analyses now give an informative error when an expected model term is
  missing from the estimates.

# medmod 1.1.0

* Releases prior to 1.2.0 were not documented here.
