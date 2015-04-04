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

test_that('jet.colors returns the correct number of colors', {
    expect_equal(length(jet.colors(0)), 0)
    expect_equal(length(jet.colors(1)), 1)
    expect_equal(length(jet.colors(128)), 128)
    }
)

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

