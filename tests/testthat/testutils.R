context('utils')

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

test_that('lavaanRow errors informatively when the parameter is missing', {
    # GIVEN a parameter estimates data frame without the requested label
    est <- data.frame(label = c('a', 'b'), est = 1:2)

    # WHEN an absent parameter is extracted
    run <- function() lavaanRow(est, label = 'ab')

    # THEN it errors, naming the missing parameter
    expect_error(run(), "'ab'")
})
