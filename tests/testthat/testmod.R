context('mod')

test_that('moderation works', {
    set.seed(1234)
    X <- rnorm(100)
    M <- rnorm(100)
    X_M <- X * M
    Y <- 0.7 * X + 0.1 * M + 4.2 * X_M + rnorm(100)
    data <- data.frame(X = X, M = M, Y = Y)

    r <- medmod::mod(data, dep = "Y", pred = "X", mod = "M", simpleSlopeEst = TRUE)

    mod <- r$mod$asDF
    ss <- r$simpleSlope$estimates$asDF

    # Test mediation effects
    expect_equal(-0.4706588, mod$est[2], tolerance = 1e-5)
    expect_equal(3.388984e-07, mod$p[2], tolerance = 1e-5)
    expect_equal('X:M', as.character(mod$term[3]))

    # Test simple slope effects
    expect_equal(2.159019, ss$z[1], tolerance = 1e-5)
    expect_equal(0.5417532, ss$se[2], tolerance = 1e-5)
    expect_equal(5.249576, ss$est[3], tolerance = 1e-5)
})

test_that('mod table labels the coefficients used in the model', {
    # GIVEN a moderation analysis with labels enabled
    set.seed(1)
    data <- data.frame(Y = rnorm(50), X = rnorm(50), M = rnorm(50))

    # WHEN the analysis is run
    r <- medmod::mod(data, dep = 'Y', pred = 'X', mod = 'M', label = TRUE)

    # THEN each term shows its coefficient label, matching the path diagram
    expect_equal(as.character(r$mod$asDF$label), c('b1', 'b2', 'b3'))
})

test_that('path diagram renders', {
    # GIVEN a moderation analysis with the path diagram enabled
    set.seed(1)
    data <- data.frame(Y = rnorm(50), X = rnorm(50), M = rnorm(50))
    r <- medmod::mod(data, dep = 'Y', pred = 'X', mod = 'M', pathDiagram = TRUE)

    # WHEN the diagram is rendered
    pdf(NULL)
    on.exit(dev.off())

    # THEN no error is raised
    expect_error(print(r$pathDiagram), NA)
})

test_that('simple slope plot renders', {
    # GIVEN a moderation analysis with the simple slope plot enabled
    set.seed(1)
    data <- data.frame(Y = rnorm(50), X = rnorm(50), M = rnorm(50))
    r <- medmod::mod(data, dep = 'Y', pred = 'X', mod = 'M', simpleSlopePlot = TRUE)

    # WHEN the plot is rendered
    pdf(NULL)
    on.exit(dev.off())

    # THEN no error is raised
    expect_error(print(r$simpleSlope$plot), NA)
})

test_that('mod model syntax labels the simple slopes section', {
    # GIVEN any configured moderation analysis
    set.seed(1)
    data <- data.frame(Y = rnorm(50), X = rnorm(50), M = rnorm(50))

    # WHEN the analysis is run
    r <- medmod::mod(data, dep = 'Y', pred = 'X', mod = 'M')

    # THEN the displayed model syntax has a header for the simple slope definitions
    expect_match(r$modelSyntax, '# Simple slopes')
})
