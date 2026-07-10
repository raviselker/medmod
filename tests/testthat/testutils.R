context('utils')

test_that('pathLabel composes the arrow annotation from its parts', {
    # GIVEN a significant path estimate
    # WHEN the annotation is built with each combination of parts
    # THEN a plotmath expression includes the name, estimate and stars only
    # when requested (subscripted names like b[1] render as subscripts)
    expect_equal(pathLabel('b[1]', 0.423, 0.004), 'b[1] == "0.42**"')
    expect_equal(pathLabel('b[1]', 0.423, 0.004, showSig = FALSE), 'b[1] == "0.42"')
    expect_equal(pathLabel('b[1]', 0.423, 0.004, showEst = FALSE), 'b[1]*"**"')
    expect_equal(
        pathLabel('b[1]', 0.423, 0.004, showEst = FALSE, showSig = FALSE),
        'b[1]'
    )
    expect_equal(pathLabel('b[1]', 0.423, 0.004, showName = FALSE), '"0.42**"')
    expect_equal(
        pathLabel('b[1]', 0.423, 0.004, showName = FALSE, showSig = FALSE),
        '"0.42"'
    )
    expect_equal(
        pathLabel('b[1]', 0.423, 0.004, showName = FALSE, showEst = FALSE),
        '"**"'
    )
    expect_equal(
        pathLabel(
            'b[1]',
            0.423,
            0.004,
            showName = FALSE,
            showEst = FALSE,
            showSig = FALSE
        ),
        '""'
    )
})

test_that('lavaanRow returns the rows matching one condition', {
    # GIVEN a parameter estimates data frame
    est <- data.frame(label = c('a', 'b', 'c'), est = 1:3)

    # WHEN a present parameter is extracted by label
    r <- lavaanRow(est, label = 'b')

    # THEN the matching row is returned
    expect_equal(r$est, 2)
})

test_that('lavaanRow combines multiple conditions', {
    # GIVEN estimates where only one row matches both fields
    est <- data.frame(lhs = c('Y', 'Y', 'M'), rhs = c('X', 'M', 'X'), est = 1:3)

    # WHEN a parameter is extracted by lhs and rhs together
    r <- lavaanRow(est, lhs = 'Y', rhs = 'M')

    # THEN only the row matching both is returned
    expect_equal(r$est, 2)
})

test_that('sigStars maps p values to significance stars', {
    # GIVEN p values on each side of the conventional cutoffs
    p <- c(0.2, 0.05, 0.049, 0.01, 0.009, 0.001, 0.0009)

    # WHEN they are converted to stars
    stars <- sigStars(p)

    # THEN each cutoff adds a star
    expect_equal(stars, c('', '', '*', '*', '**', '**', '***'))
})

test_that('lavaanRow errors informatively when the parameter is missing', {
    # GIVEN a parameter estimates data frame without the requested label
    est <- data.frame(label = c('a', 'b'), est = 1:2)

    # WHEN an absent parameter is extracted
    run <- function() lavaanRow(est, label = 'ab')

    # THEN it errors, naming the missing parameter
    expect_error(run(), "'ab'")
})
