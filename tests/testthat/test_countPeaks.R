context("countPeaks")

test_that("countPeaks throws an error for non-MassPeaks lists", {
    expect_error(countPeaks(1:10), "must be a list of MassPeaks")
    expect_error(countPeaks(list(1:3, 1:3), "must be a list of MassPeaks"))
    expect_error(countPeaks(list(createMassPeaks(1:2, 1:2),
                                 createMassSpectrum(1:2, 1:2))),
                 "must be a list of MassPeaks")
})

test_that("countPeaks returns correct results", {
    expect_equal(countPeaks(list(createMassPeaks(1:2, 1:2))), 2)
    expect_equal(countPeaks(list(createMassPeaks(1:2, 1:2),
                                 createMassPeaks(1:3, 1:3),
                                 createMassPeaks(1:4, 1:4),
                                 createMassPeaks(1:5, 1:5))), 2:5)
})
