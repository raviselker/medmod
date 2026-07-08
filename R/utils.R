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

#' Shorten labels for display
#'
#' \code{shortenLabel} truncates long labels with an ellipsis so they fit
#' inside path diagram boxes.
#'
#' @param x Character vector of labels.
#' @param n Maximum number of characters.
shortenLabel <- function(x, n = 16) {
    long <- nchar(x) > n
    x[long] <- paste0(substr(x[long], 1, n - 1), '\u2026')

    return(x)
}

#' Format a path coefficient annotation
#'
#' \code{pathLabel} builds the arrow annotation for a path diagram, e.g.
#' \code{"a = 0.42***"}.
#'
#' @param name Path name (e.g. \code{'a'}).
#' @param est Estimated coefficient.
#' @param p The p value, converted to significance stars.
pathLabel <- function(name, est, p) {
    return(paste0(name, ' = ', sprintf('%.2f', est), sigStars(p)))
}

#' Draw a path diagram
#'
#' \code{drawPathDiagram} renders boxes and labelled arrows as a ggplot.
#' Arrows run between box borders; an edge whose \code{to} is not a node name
#' ends at its explicit \code{toX}/\code{toY} point (used to point at a path).
#'
#' @param nodes Data frame with \code{name}, \code{label}, \code{x}, \code{y}.
#' @param edges Data frame with \code{from}, \code{to}, \code{toX},
#'   \code{toY}, \code{fromX}, \code{fromY}, \code{label}, \code{nudgeX},
#'   \code{nudgeY} (label offset from the arrow midpoint). Arrows run between
#'   box borders by default; non-\code{NA} \code{fromX}/\code{fromY} and
#'   \code{toX}/\code{toY} override the anchor points.
#' @param ggtheme The jamovi ggplot2 theme.
#' @param theme The jamovi theme colors.
drawPathDiagram <- function(nodes, edges, ggtheme, theme) {
    halfW <- 1.55
    halfH <- 0.55
    gap <- 0.12

    borderPoint <- function(x, y, towardX, towardY) {
        dx <- towardX - x
        dy <- towardY - y
        t <- pmin(
            ifelse(dx == 0, Inf, (halfW + gap) / abs(dx)),
            ifelse(dy == 0, Inf, (halfH + gap) / abs(dy))
        )
        list(x = x + dx * t, y = y + dy * t)
    }

    fromNode <- match(edges$from, nodes$name)
    toNode <- match(edges$to, nodes$name)

    fromX <- nodes$x[fromNode]
    fromY <- nodes$y[fromNode]
    toX <- ifelse(is.na(toNode), edges$toX, nodes$x[toNode])
    toY <- ifelse(is.na(toNode), edges$toY, nodes$y[toNode])

    start <- borderPoint(fromX, fromY, toX, toY)
    start$x <- ifelse(is.na(edges$fromX), start$x, edges$fromX)
    start$y <- ifelse(is.na(edges$fromY), start$y, edges$fromY)

    end <- list(x = toX, y = toY)
    trimmed <- borderPoint(toX, toY, fromX, fromY)
    end$x[!is.na(toNode)] <- trimmed$x[!is.na(toNode)]
    end$y[!is.na(toNode)] <- trimmed$y[!is.na(toNode)]

    segments <- data.frame(
        x = start$x,
        y = start$y,
        xend = end$x,
        yend = end$y,
        label = edges$label,
        labelX = (start$x + end$x) / 2 + edges$nudgeX,
        labelY = (start$y + end$y) / 2 + edges$nudgeY
    )

    p <- ggplot2::ggplot() +
        ggplot2::geom_segment(
            data = segments,
            ggplot2::aes(
                x = .data$x,
                y = .data$y,
                xend = .data$xend,
                yend = .data$yend
            ),
            colour = theme$color[1],
            linewidth = 0.8,
            linejoin = 'mitre',
            arrow = ggplot2::arrow(
                angle = 20,
                length = ggplot2::unit(0.25, 'cm'),
                type = 'closed'
            )
        ) +
        ggplot2::geom_rect(
            data = nodes,
            ggplot2::aes(
                xmin = .data$x - halfW,
                xmax = .data$x + halfW,
                ymin = .data$y - halfH,
                ymax = .data$y + halfH
            ),
            fill = theme$fill[1],
            colour = theme$color[1],
            linewidth = 0.6
        ) +
        ggplot2::geom_text(
            data = nodes,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
            colour = theme$color[1],
            size = 5
        ) +
        ggplot2::geom_text(
            data = segments,
            ggplot2::aes(x = .data$labelX, y = .data$labelY, label = .data$label),
            colour = theme$color[1],
            size = 4.5
        ) +
        ggplot2::scale_x_continuous(
            name = NULL,
            breaks = NULL,
            limits = c(min(nodes$x) - halfW - 0.5, max(nodes$x) + halfW + 0.5)
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            breaks = NULL,
            limits = c(min(nodes$y) - halfH - 0.6, max(nodes$y) + halfH + 0.6)
        ) +
        ggplot2::labs(caption = '* p < .05, ** p < .01, *** p < .001') +
        ggtheme +
        ggplot2::theme(
            axis.line = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            plot.caption = ggplot2::element_text(
                size = 10,
                colour = theme$color[1],
                hjust = 1
            )
        )

    return(p)
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
