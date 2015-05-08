test_that('bathy.colors returns the correct number of colors', {
    expect_equal(length(bathy.colors(0)), 0)
    expect_equal(length(bathy.colors(1)), 1)
    expect_equal(length(bathy.colors(128)), 128)
    }
)

test_that('bathy.colors does not accept a negative number of colors', {
    expect_error(bathy.colors(-1))
})

test_that('bathy.colors does not accept a negative alpha value', {
    expect_error(bathy.colors(1, alpha=-0.5))
})

test_that('bathy.colors does not accept an alpha value greater than 1', {
    expect_error(bathy.colors(1, alpha=1.5))
})

test_that('bathy.colors assigns the correct alpha value', {
    cols = bathy.colors(10, alpha=0.0)
    for(i in seq(length(cols)))
        expect_equal(substr(cols[i], 8, 9), "00")
    cols = bathy.colors(10, alpha=0.5)
    for(i in seq(length(cols)))
        expect_equal(substr(cols[i], 8, 9), "80")
    cols = bathy.colors(10, alpha=1)
    for(i in seq(length(cols)))
        expect_equal(substr(cols[i], 8, 9), "FF")
})

test_that('bathy.colors begins with grey and ends with blue', {
    cols = col2rgb(bathy.colors(10))
    rownames(cols) = NULL
    colnames(cols) = NULL
    expect_equal(cols[1,1], cols[2,1])
    expect_equal(cols[2,1], cols[3,1])
    expect_true((cols[3,10] > cols[1,10]) & (cols[3,10] > cols[2,10]))
})

test_that('jet.colors returns the correct number of colors', {
    expect_equal(length(jet.colors(0)), 0)
    expect_equal(length(jet.colors(1)), 1)
    expect_equal(length(jet.colors(128)), 128)
})

test_that('jet.colors does not accept a negative number of colors.', {
    expect_error(jet.colors(-1))
})

test_that('jet.colors does not accept a negative alpha value', {
    expect_error(jet.colors(1, alpha=-0.5))
})

test_that('jet.colors does not accept an alpha value greater than 1', {
    expect_error(jet.colors(1, alpha=1.5))
})

test_that('jet.colors assigns the correct alpha value', {
    cols = jet.colors(10, alpha=0.0)
    for(i in seq(length(cols)))
        expect_equal(substr(cols[i], 8, 9), "00")
    cols = jet.colors(10, alpha=0.5)
    for(i in seq(length(cols)))
        expect_equal(substr(cols[i], 8, 9), "80")
    cols = jet.colors(10, alpha=1)
    for(i in seq(length(cols)))
        expect_equal(substr(cols[i], 8, 9), "FF")
})

test_that('jet.colors begins with blue and ends with red', {
    cols = col2rgb(jet.colors(10))
    expect_true((cols[3,1] > cols[1,1]) & (cols[3,1] > cols[2,1]))
    expect_true((cols[1,10] > cols[2,10]) & (cols[1,10] > cols[3,10]))
})

## TODO INSERT UNIT TESTS FOR SHALLOW.BATHY.COLORS ##
