context('med')

test_that('mediation works', {
    set.seed(1234)
    X <- rnorm(100)
    M <- 0.5 * X + rnorm(100)
    Y <- 0.7 * M + rnorm(100)
    data <- data.frame(X = X, M = M, Y = Y)

    r <- medmod::med(data, dep = "Y", pred = "X", med = "M", paths = TRUE)

    med <- r$med$asDF
    path <- r$paths$asDF

    # Test mediation effects
    expect_equal(0.03635467, med$est[2], tolerance = 1e-5)
    expect_equal(4.934837e-05, med$p[1], tolerance = 1e-5)
    expect_equal('Total', as.character(med$effect[3]))

    # Test path estimates
    expect_equal(0.7883193, path$est[2], tolerance = 1e-5)
    expect_equal(3.967471e-06, path$p[1], tolerance = 1e-5)
    expect_equal(0.1044457, path$se[3], tolerance = 1e-5)
})

test_that('percent mediation is the effect share of the total effect', {
    # GIVEN a mediation analysis with percent mediation enabled
    set.seed(1234)
    X <- rnorm(100)
    M <- 0.5 * X + rnorm(100)
    Y <- 0.7 * M + rnorm(100)
    data <- data.frame(X = X, M = M, Y = Y)

    # WHEN the analysis is run
    r <- medmod::med(data, dep = 'Y', pred = 'X', med = 'M', pm = TRUE)
    med <- r$med$asDF

    # THEN each effect's percent mediation is its share of the unstandardized
    # indirect + direct magnitude, and the total row is 100
    total <- abs(med$est[1]) + abs(med$est[2])
    expect_equal(med$pm[1], abs(med$est[1]) / total * 100, tolerance = 1e-8)
    expect_equal(med$pm[2], abs(med$est[2]) / total * 100, tolerance = 1e-8)
    expect_equal(med$pm[3], 100)
})

test_that('path diagram renders', {
    # GIVEN a mediation analysis with the path diagram enabled
    set.seed(1)
    data <- data.frame(Y = rnorm(50), X = rnorm(50), M = rnorm(50))
    r <- medmod::med(data, dep = 'Y', pred = 'X', med = 'M', pathDiagram = TRUE)

    # WHEN the diagram is rendered
    pdf(NULL)
    on.exit(dev.off())

    # THEN no error is raised
    expect_error(print(r$pathDiagram), NA)
})

test_that('estimate plot renders', {
    # GIVEN a mediation analysis with the estimate plot enabled
    set.seed(1)
    data <- data.frame(Y = rnorm(50), X = rnorm(50), M = rnorm(50))
    r <- medmod::med(data, dep = 'Y', pred = 'X', med = 'M', estPlot = TRUE)

    # WHEN the plot is rendered
    pdf(NULL)
    on.exit(dev.off())

    # THEN no error is raised
    expect_error(print(r$estPlot), NA)
})
