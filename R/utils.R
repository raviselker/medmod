#' Center a vector
#'
#' \code{center} returns a numeric vector with centered values.
#'
#' @param x Numeric vector.
center <- function(x) {
    centered <- as.vector(scale(x = x, center = TRUE, scale = FALSE))

    return(centered)
}

#' Significance stars for p values
#'
#' \code{sigStars} converts p values to the conventional significance stars:
#' \code{*} p < .05, \code{**} p < .01, \code{***} p < .001.
#'
#' @param p Numeric vector of p values.
sigStars <- function(p) {
    stars <- character(length(p))
    stars[p < 0.05] <- '*'
    stars[p < 0.01] <- '**'
    stars[p < 0.001] <- '***'

    return(stars)
}

#' Extract parameter rows from lavaan estimates
#'
#' \code{lavaanRow} returns the rows of a \code{lavaan::parameterestimates()}
#' data frame whose columns equal the given values, e.g.
#' \code{lavaanRow(est, label = 'ab')} or \code{lavaanRow(est, lhs = dep,
#' rhs = pred)}. It errors when the expected parameter is absent instead of
#' silently returning zero rows.
#'
#' @param est Data frame of parameter estimates.
#' @param ... Named values; each name is a column of \code{est} that must
#'   equal the value.
lavaanRow <- function(est, ...) {
    conditions <- list(...)

    rows <- rep(TRUE, nrow(est))
    for (field in names(conditions)) {
        rows <- rows & est[[field]] == conditions[[field]]
    }

    if (!any(rows)) {
        stop(jmvcore::format(
            "Parameter with {} was not found in the model estimates",
            paste0(names(conditions), " '", unlist(conditions), "'", collapse = ", ")
        ))
    }

    return(est[rows, ])
}
