
medClass <- R6::R6Class(
    "medClass",
    inherit = medBase,
    private = list(
        #### Init + run functions ----
        .init = function () {
            
            private$.initMedTable()
            private$.initPathsTable()
            
            syntax <- private$.lavaanify(FALSE)
            self$results$.setModelSyntax(syntax)
            
        },
        .run = function() {
            
            if (self$results$isFilled())
                return()
            
            ready <- ! is.null(self$options$dep) &&  ! is.null(self$options$pred) &&  ! is.null(self$options$med)
            
            if (ready) {
                
                data <- private$.cleanData()
                results <- private$.compute(data)
                
                private$.populateMedTable(results)
                private$.populatePathsTable(results)
                private$.prepareEstPlot(results)

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
        .initMedTable = function() {
            
            table <- self$results$med
            
            table$addRow(1, values = list(effect='Indirect', label='a \u00D7 b'))
            table$addRow(2, values = list(effect='Direct', label='c'))
            table$addRow(3, values = list(effect='Total', label='c + a \u00D7 b'))
            
            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
        },
        .initPathsTable = function() {
            
            table <- self$results$paths
            
            table$setRow(rowNo=1, values=list(var1=self$options$pred, arrow='\U2192', var2=self$options$med, label='a'))
            table$setRow(rowNo=2, values=list(var1=self$options$med, arrow='\U2192', var2=self$options$dep, label='b'))
            table$setRow(rowNo=3, values=list(var1=self$options$pred, arrow='\U2192', var2=self$options$dep, label='c'))
            
            ciWidth <- self$options$ciWidth
            table$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
            table$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
        },
        
        #### Populate tables ----
        .populateMedTable = function(results) {
            
            table <- self$results$med
            est <- lavaan::parameterestimates(results$fit, standardized = TRUE, level = self$options$ciWidth / 100)
            
            total <- abs(est[est$label == 'ab', 'std.lv']) + abs(est[est$label == 'c', 'std.lv'])
            
            labels <- c('ab', 'c', 'total')
            for (i in seq_along(labels)) {
                
                index <- which(est$label == labels[i])
                r <- est[index,]
                
                row <- list()
                row[['est']] <- r$est
                row[['se']] <- r$se
                row[['z']] <- r$z
                row[['p']] <- r$pvalue
                row[['lower']] <- r$ci.lower
                row[['upper']] <- r$ci.upper
                row[['pm']] <-  ifelse(labels[i] == 'total', 100,  abs(r$est) / total * 100)
                
                table$setRow(rowNo=i, values=row)
            }
        },
        .populatePathsTable = function(results) {
            
            table <- self$results$paths
            est <- lavaan::parameterestimates(results$fit, standardized = TRUE, level = self$options$ciWidth / 100)
            
            labels <- c('a', 'b', 'c')
            for (i in seq_along(labels)) {
                
                index <- which(est$label == labels[i])
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
        .prepareEstPlot = function(results) {
            
            image <- self$results$estPlot
            est <- lavaan::parameterestimates(results$fit, standardized = TRUE, level = self$options$ciWidth / 100)
            
            labels <- c('ab', 'c', 'total')
            names <- c('Indirect', 'Direct', 'Total')
            
            indices <- numeric(3)
            for (i in seq_along(labels))
                indices[i] <- which(est$label == labels[i])

            betas <- est[indices, ]
            
            df <- data.frame(
                term = names,
                estimate = as.numeric(betas$est),
                conf.low = as.numeric(betas$ci.lower),
                conf.high = as.numeric(betas$ci.upper),
                group = rep('CI', length(betas$est))
            )
            df$term <- factor(df$term, rev(df$term))
            
            image$setState(df)
        },
        .estPlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            themeSpec <- ggplot2::theme(
                legend.position = 'right',
                legend.background = ggplot2::element_rect("transparent"),
                legend.title = ggplot2::element_blank(),
                legend.key = ggplot2::element_blank(),
                legend.text = ggplot2::element_text(size=16, colour='#333333'))
            
            errorType <- paste0(self$options$ciWidth, '% CI')
            
            p <- ggplot2::ggplot(data=image$state) +
                ggplot2::geom_hline(yintercept=0, linetype="dotted", colour=theme$color[1], size=1.2) +
                ggplot2::geom_errorbar(ggplot2::aes(x=term, ymin=conf.low, ymax=conf.high, width=.2, colour='colour'), size=.8) +
                ggplot2::geom_point(ggplot2::aes(x=term, y=estimate, colour='colour'), shape=21, fill=theme$fill[1], size=3) +
                ggplot2::scale_colour_manual(name='', values=c(colour=theme$color[1]), labels=paste("", errorType)) +
                ggplot2::labs(x="Effect", y="Estimate") +
                ggplot2::coord_flip() +
                ggtheme + themeSpec
            
            print(p)
            
            TRUE
        },
        
        #### Helper functions ----
        .cleanData = function() {
            
            dep <- self$options$dep
            pred <- self$options$pred
            med <- self$options$med
            
            data <- list()
            data[[jmvcore::toB64(dep)]] <- jmvcore::toNumeric(self$data[[dep]])
            data[[jmvcore::toB64(pred)]] <- jmvcore::toNumeric(self$data[[pred]])
            data[[jmvcore::toB64(med)]] <- jmvcore::toNumeric(self$data[[med]])
            
            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            
            return(data)
            
        },
        .lavaanify = function(B64 = TRUE) {
            
            dep <- self$options$dep
            pred <- self$options$pred
            med <- self$options$med
            
            head1 <- '# Direct effect\n'
            head2 <- '# Mediator\n'
            head3 <- '# Indirect effect (a*b)\n'
            head4 <- '# Total effect\n'
            indir <- "ab := a*b\n\n"
            tot <- "total := c + (a*b)"
            
            if (B64) {
                
                dir <- paste0(jmvcore::toB64(dep), " ~ c*", jmvcore::toB64(pred), "\n\n")
                med1 <- paste0(jmvcore::toB64(med), " ~ a*", jmvcore::toB64(pred), "\n")
                med2 <- paste0(jmvcore::toB64(dep), " ~ b*", jmvcore::toB64(med), "\n\n")
                
            } else {
                
                dir <- paste0(dep, " ~ c*", pred, "\n\n")
                med1 <- paste0(med, " ~ a*", pred, "\n")
                med2 <- paste0(dep, " ~ b*", med, "\n\n")
            }
            
            model <- paste0(head1, dir, head2, med1, med2, head3, indir, head4, tot)
            
            return(model)
        })
)
