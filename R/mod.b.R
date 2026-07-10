modClass <- R6::R6Class(
    "modClass",
    inherit = modBase,
    private = list(
        #### Init + run functions ----
        .init = function() {
            private$.initModTable()
            private$.initSimpleSlopeTable()

            syntax <- private$.lavaanify(FALSE)
            self$results$.setModelSyntax(syntax)
        },
        .run = function() {
            if (self$results$isFilled()) {
                return()
            }

            ready <- !is.null(self$options$dep) &&
                !is.null(self$options$pred) &&
                !is.null(self$options$mod)

            if (ready) {
                data <- private$.cleanData()
                results <- private$.compute(data)

                private$.populateModTable(results)
                private$.populateSimpleSlopeTable(results)
                private$.preparePathDiagram(results)
                private$.prepareSimpleSlopePlot(data, results)
            }
        },

        #### Compute results ----
        .compute = function(data) {
            # the estimates only depend on the variables, the estimation
            # options and the CI level (the model cache's clearWith), so
            # reuse them across other option changes -- refitting is
            # expensive with bootstrap SEs. Only lavaan's estimates table is
            # kept, so the state size does not grow with the data
            est <- self$results$model$state

            if (is.null(est)) {
                model <- private$.lavaanify()
                se <- self$options$estMethod
                bootstrap <- self$options$bootstrap

                suppressWarnings({
                    fit <- lavaan::sem(model = model, data = data, se = se, bootstrap = bootstrap)
                }) # suppressWarnings

                est <- lavaan::parameterestimates(
                    fit,
                    level = self$options$ciWidth / 100
                )

                self$results$model$setState(est)
            }

            return(list('est' = est))
        },

        #### Init tables/plots functions ----
        .initModTable = function() {
            table <- self$results$mod
            pred <- self$options$pred
            mod <- self$options$mod

            table$addRow(1, values = list(term = pred, label = 'b\u2081'))
            table$addRow(2, values = list(term = mod, label = 'b\u2082'))
            table$addRow(
                3,
                values = list(
                    term = jmvcore::stringifyTerm(c(pred, mod)),
                    label = 'b\u2083'
                )
            )

            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format(
                '{}% Confidence Interval',
                ciWidth
            ))
            table$getColumn('upper')$setSuperTitle(jmvcore::format(
                '{}% Confidence Interval',
                ciWidth
            ))
        },
        .initSimpleSlopeTable = function() {
            table <- self$results$simpleSlope$estimates

            table$addRow(1, values = list(term = "Average"))
            table$addRow(2, values = list(term = "Low (-1SD)"))
            table$addRow(3, values = list(term = "High (+1SD)"))

            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format(
                '{}% Confidence Interval',
                ciWidth
            ))
            table$getColumn('upper')$setSuperTitle(jmvcore::format(
                '{}% Confidence Interval',
                ciWidth
            ))

            table$setNote(
                "note",
                jmvcore::format(
                    "shows the effect of the predictor ({}) on the dependent variable ({}) at different levels of the moderator ({})",
                    self$options$pred,
                    self$options$dep,
                    self$options$mod
                )
            )
        },

        #### Populate tables ----
        .populateModTable = function(results) {
            table <- self$results$mod
            est <- results$est

            dep <- jmvcore::toB64(self$options$dep)
            pred <- jmvcore::toB64(self$options$pred)
            mod <- jmvcore::toB64(self$options$mod)
            int <- paste0(jmvcore::toB64(self$options$pred), jmvcore::toB64(self$options$mod))

            labels <- c(pred, mod, int)
            for (i in seq_along(labels)) {
                r <- lavaanRow(est, lhs = dep, rhs = labels[i])

                row <- list()
                row[['est']] <- r$est
                row[['se']] <- r$se
                row[['z']] <- r$z
                row[['p']] <- r$pvalue
                row[['lower']] <- r$ci.lower
                row[['upper']] <- r$ci.upper

                table$setRow(rowNo = i, values = row)
            }
        },
        .populateSimpleSlopeTable = function(results) {
            table <- self$results$simpleSlope$estimates
            est <- results$est

            labels <- c('mean', 'sdBelow', 'sdAbove')
            for (i in seq_along(labels)) {
                r <- lavaanRow(est, lhs = labels[i])

                row <- list()
                row[['est']] <- r$est
                row[['se']] <- r$se
                row[['z']] <- r$z
                row[['p']] <- r$pvalue
                row[['lower']] <- r$ci.lower
                row[['upper']] <- r$ci.upper

                table$setRow(rowNo = i, values = row)
            }
        },

        #### Plot functions ----
        .preparePathDiagram = function(results) {
            image <- self$results$pathDiagram
            est <- results$est

            b1 <- lavaanRow(est, label = 'b1')
            b2 <- lavaanRow(est, label = 'b2')
            b3 <- lavaanRow(est, label = 'b3')

            image$setState(data.frame(
                path = c('b1', 'b2', 'b3'),
                est = c(b1$est, b2$est, b3$est),
                p = c(b1$pvalue, b2$pvalue, b3$pvalue)
            ))
        },
        .pathDiagram = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) {
                return(FALSE)
            }

            paths <- image$state
            showName <- self$options$pathDiagramLabel
            showEst <- self$options$pathDiagramEst
            showSig <- self$options$pathDiagramSig

            nodes <- data.frame(
                name = c('pred', 'dep', 'mod'),
                label = shortenLabel(c(
                    self$options$pred,
                    self$options$dep,
                    self$options$mod
                )),
                x = c(1.5, 8.5, 5),
                y = c(1, 1, 5)
            )

            # conceptual moderation diagram: the moderator arrow points at the
            # pred -> dep path and carries the interaction estimate (b3); the
            # moderator's main effect (b2) is optionally drawn as its own arrow
            edges <- data.frame(
                from = c('pred', 'mod'),
                to = c('dep', NA),
                fromX = NA_real_,
                fromY = NA_real_,
                toX = c(NA, 5),
                toY = c(NA, 1.15),
                label = c(
                    pathLabel('b[1]', paths$est[1], paths$p[1], showName, showEst, showSig),
                    pathLabel('b[3]', paths$est[3], paths$p[3], showName, showEst, showSig)
                ),
                # b1 stays centered below its arrow; b3 grows leftwards from
                # just left of the vertical arrow
                nudgeX = c(0, -0.25),
                nudgeY = c(-0.4, 0),
                hjust = c(0.5, 1)
            )

            if (self$options$pathDiagramMainEffect) {
                edges <- rbind(
                    edges,
                    data.frame(
                        from = 'mod',
                        to = NA,
                        fromX = 5.85,
                        fromY = 4.38,
                        toX = 7.7,
                        toY = 1.62,
                        label = pathLabel(
                            'b[2]',
                            paths$est[2],
                            paths$p[2],
                            showName,
                            showEst,
                            showSig
                        ),
                        nudgeX = 0.25,
                        nudgeY = 0.15,
                        hjust = 0
                    )
                )
            }

            p <- drawPathDiagram(nodes, edges, ggtheme, theme, sigCaption = showSig)

            return(p)
        },
        .prepareSimpleSlopePlot = function(data, results) {
            image <- self$results$simpleSlope$plot
            est <- results$est

            mod <- self$options$mod

            labels <- c('mean', 'sdBelow', 'sdAbove')
            names <- c('Average', 'Low (-1SD)', 'High (+1SD)')

            intercept <- lavaanRow(est, label = 'b0')

            betas <- do.call(
                rbind,
                lapply(labels, function(label) lavaanRow(est, lhs = label))
            )

            coef <- data.frame(
                term = factor(labels, levels = labels),
                name = factor(names, levels = names),
                slope = as.numeric(betas$est),
                intercept = as.numeric(rep(intercept$est, nrow(betas))),
                conf.low = as.numeric(betas$ci.lower),
                conf.high = as.numeric(betas$ci.upper),
                group = rep('CI', length(betas$est))
            )

            image$setState(list(coef = coef, data = data))
        },
        .simpleSlopePlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) {
                return(FALSE)
            }

            themeSpec <- ggplot2::theme(
                legend.position = 'right',
                legend.background = ggplot2::element_rect("transparent"),
                legend.key = ggplot2::element_blank()
            )

            dep <- jmvcore::toB64(self$options$dep)
            pred <- jmvcore::toB64(self$options$pred)
            mod <- self$options$mod

            p <- ggplot2::ggplot(
                data = image$state$data,
                ggplot2::aes(x = .data[[pred]], y = .data[[dep]])
            ) +
                ggplot2::geom_point(color = theme$color[1], alpha = 0.3) +
                ggplot2::geom_abline(
                    data = image$state$coef,
                    ggplot2::aes(slope = slope, intercept = intercept, color = name),
                    linewidth = 1.2
                ) +
                ggplot2::labs(x = self$options$pred, y = self$options$dep, color = mod) +
                ggplot2::scale_color_discrete(name = mod) +
                ggtheme +
                themeSpec

            print(p)

            TRUE
        },

        #### Helper functions ----
        .cleanData = function() {
            dep <- self$options$dep
            pred <- self$options$pred
            mod <- self$options$mod

            data <- list()
            data[[jmvcore::toB64(dep)]] <- center(jmvcore::toNumeric(self$data[[dep]]))
            data[[jmvcore::toB64(pred)]] <- center(jmvcore::toNumeric(self$data[[pred]]))
            data[[jmvcore::toB64(mod)]] <- center(jmvcore::toNumeric(self$data[[mod]]))
            data[[paste0(jmvcore::toB64(pred), jmvcore::toB64(mod))]] <- center(
                data[[jmvcore::toB64(pred)]] * data[[jmvcore::toB64(mod)]]
            )

            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'

            return(data)
        },
        .lavaanify = function(B64 = TRUE) {
            if (B64) {
                dep <- jmvcore::toB64(self$options$dep)
                pred <- jmvcore::toB64(self$options$pred)
                mod <- jmvcore::toB64(self$options$mod)
            } else {
                dep <- self$options$dep
                pred <- self$options$pred
                mod <- self$options$mod
            }

            head1 <- '# Regressions\n'
            head2 <- '# Mean of centered moderator (for use in simple slopes)\n'
            head3 <- '# Variance of centered moderator (for use in simple slopes)\n'
            head4 <- '# Simple slopes\n'
            simp1 <- 'sdBelow := b1 + b3*(modMean - sqrt(modVar))\n'
            simp2 <- 'mean := b1 + b3*(modMean)\n'
            simp3 <- 'sdAbove := b1 + b3*(modMean + sqrt(modVar))\n'

            reg <- paste0(dep, " ~ b0*1 + b1*", pred, " + b2*", mod, " + b3*", pred, mod, "\n\n")

            mean <- paste0(mod, " ~ modMean*1\n\n")
            var <- paste0(mod, " ~~ modVar*", mod, "\n\n")

            model <- paste0(head1, reg, head2, mean, head3, var, head4, simp1, simp2, simp3)

            return(model)
        }
    )
)
