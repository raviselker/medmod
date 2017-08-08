
modClass <- R6::R6Class(
    "modClass",
    inherit = modBase,
    private = list(
        #### Init + run functions ----
        .init = function () {
            
            private$.initModTable()
            private$.initSimpleSlopeTable()

            syntax <- private$.lavaanify(FALSE)
            self$results$.setModelSyntax(syntax)
            
        },
        .run = function() {
            
            if (self$results$isFilled())
                return()
            
            ready <- ! is.null(self$options$dep) &&  ! is.null(self$options$pred) &&  ! is.null(self$options$mod)
            
            if (ready) {
                
                data <- private$.cleanData()
                results <- private$.compute(data)
                
                private$.populateModTable(results)
                private$.populateSimpleSlopeTable(results)
                private$.prepareSimpleSlopePlot(data, results)
            }
        },
        
        #### Compute results ----
        .compute = function(data) {
            
            model <- private$.lavaanify()
            se <- self$options$estMethod
            bootstrap <- self$options$bootstrap

            suppressWarnings({

                fit <- lavaan::sem(model = model, data=data, se=se, bootstrap=bootstrap)

            }) # suppressWarnings

            return(list('fit'=fit))
        },
        
        #### Init tables/plots functions ----
        .initModTable = function() {
            
            table <- self$results$mod
            pred <- self$options$pred
            mod <- self$options$mod
            
            table$addRow(1, values = list(term=pred))
            table$addRow(2, values = list(term=mod))
            table$addRow(3, values = list(term=jmvcore::stringifyTerm(c(pred, mod))))
            
            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
        },
        .initSimpleSlopeTable = function() {
            
            table <- self$results$simpleSlope$estimates
            
            table$addRow(1, values = list(term="Average"))
            table$addRow(2, values = list(term="Low (-1SD)"))
            table$addRow(3, values = list(term="High (+1SD)"))
            
            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            
            table$setNote("note", jmvcore::format("shows the effect of the predictor ({}) on the dependent variable ({}) at different levels of the moderator ({})", 
                                                  self$options$pred, self$options$dep, self$options$mod))
        },
        
        #### Populate tables ----
        .populateModTable = function(results) {
            
            table <- self$results$mod
            est <- lavaan::parameterestimates(results$fit, standardized = TRUE, level = self$options$ciWidth / 100)
            
            dep <- jmvcore::toB64(self$options$dep)
            pred <- jmvcore::toB64(self$options$pred)
            mod <- jmvcore::toB64(self$options$mod)
            int <- paste0(jmvcore::toB64(self$options$pred), jmvcore::toB64(self$options$mod))
            
            labels <- c(pred, mod, int)
            for (i in seq_along(labels)) {
                
                index <- which(est$lhs == dep & est$rhs == labels[i])
                r <- est[index,]
                
                row <- list()
                row[['est']] <- r$est
                row[['se']] <- r$se
                row[['z']] <- r$z
                row[['p']] <- r$pvalue
                row[['lower']] <- r$ci.lower
                row[['upper']] <- r$ci.upper

                table$setRow(rowNo=i, values=row)
            }
        },
        .populateSimpleSlopeTable = function(results) {
            
            table <- self$results$simpleSlope$estimates
            est <- lavaan::parameterestimates(results$fit, standardized = TRUE, level = self$options$ciWidth / 100)
            
            labels <- c('mean', 'sdBelow', 'sdAbove')
            for (i in seq_along(labels)) {
                
                index <- which(est$lhs == labels[i])
                r <- est[index,]
                
                row <- list()
                row[['est']] <- r$est
                row[['se']] <- r$se
                row[['z']] <- r$z
                row[['p']] <- r$pvalue
                row[['lower']] <- r$ci.lower
                row[['upper']] <- r$ci.upper
                
                table$setRow(rowNo=i, values=row)
            }
        },
        
        #### Plot functions ----
        .prepareSimpleSlopePlot = function(data, results) {
            
            image <- self$results$simpleSlope$plot
            est <- lavaan::parameterestimates(results$fit, standardized = TRUE, level = self$options$ciWidth / 100)
            
            mod <- self$options$mod
            
            labels <- c('mean', 'sdBelow', 'sdAbove')
            names <- c('Average', 'Low (-1SD)', 'High (+1SD)')
            
            indices <- numeric(3)
            for (i in seq_along(labels))
                indices[i] <- which(est$lhs == labels[i])

            intercept <-est[which(est$label == 'b0'), ]
            
            betas <- est[indices, ]
            
            coef <- data.frame(
                term = factor(labels, levels = labels),
                name = factor(names, levels = names),
                slope = as.numeric(betas$est),
                intercept = as.numeric(rep(intercept$est, nrow(betas))),
                conf.low = as.numeric(betas$ci.lower),
                conf.high = as.numeric(betas$ci.upper),
                group = rep('CI', length(betas$est))
            )

            image$setState(list(coef=coef, data=data))
        },
        .simpleSlopePlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            themeSpec <- ggplot2::theme(
                legend.position = 'right',
                legend.background = ggplot2::element_rect("transparent"),
                legend.key = ggplot2::element_blank())
            
            errorType <- paste0(self$options$ciWidth, '% CI')
            dep <- jmvcore::toB64(self$options$dep)
            pred <- jmvcore::toB64(self$options$pred)
            mod <- self$options$mod
            
            p <- ggplot2::ggplot(data=image$state$data, ggplot2::aes_string(x=pred, y=dep)) +
                ggplot2::geom_point(color=theme$color[1], alpha = 0.3) +
                ggplot2::geom_abline(data=image$state$coef, ggplot2::aes(slope=slope, intercept=intercept, color=name), size=1.2) +
                ggplot2::labs(x=self$options$pred, y=self$options$dep) +
                ggplot2::scale_color_discrete(name = mod) +
                ggtheme + themeSpec
            
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
            data[[paste0(jmvcore::toB64(pred), jmvcore::toB64(mod))]] <- center(data[[jmvcore::toB64(pred)]] * data[[jmvcore::toB64(mod)]])
            
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
                
            reg1 <- paste0(dep, " ~ b1*", pred, "\n")
            reg2 <- paste0(dep, " ~ b2*", mod, "\n")
            reg3 <- paste0(dep, " ~ b3*", pred, mod, "\n\n")
            
            reg <- paste0(dep, " ~ b0*1 + b1*", pred, " + b2*", mod, " + b3*", pred, mod, "\n\n")
            
            mean <- paste0(mod, " ~ modMean*1\n\n")
            var <- paste0(mod, " ~~ modVar*", mod, "\n\n")
            
            model <- paste0(head1, reg, head2, mean, head3, var, simp1, simp2, simp3)
            
            return(model)
        })
)
