load("testspectra.rda")
load("testpeaks.rda")
load("testtype.rda")

context("screenSpectra")

test_that("screenSpectra returns expected results",{
  expect_equal(length(screenSpectra(spectra,type)$fspectra)==length(screenSpectra(spectra,type)$fmeta), TRUE)
  expect_equal(round(sum(screenSpectra(spectra)$prop), 5), 0.0467)
  expect_equal(round(sum(screenSpectra(spectra)$upper), 5), 0.38162)
  expect_equal(round(sum(screenSpectra(spectra, method = "ESD")$upper), 5), 0.37325)
  expect_equal(round(sum(screenSpectra(spectra, method = "Hampel")$upper), 5), 0.36792)
  expect_equal(round(sum(screenSpectra(spectra, method = "boxplot")$upper), 5), 0.38992)
  expect_equal(round(sum(screenSpectra(spectra)$est.table$`A score`), 5), 50.72749)
  expect_equal(round(sum(screenSpectra(spectra, estimator = "MAD")$est.table$`A score`), 5), 44.96811)
  
})

context("detectOutliers")

set.seed(123)
test_that("detectOutliers returns expected results",{
  expect_equal(sum(detectOutliers(peaks)), 63, tolerance = 2)
  expect_equal(sum(detectOutliers(peaks, by = type)$Atypical), 37, tolerance = 2)
})

context("summaryStats")

test_that(".summaryStats returns expected results",{
  expect_equal(all(round(colMeans(.summaryStats(peaks)), 5) == c(15.90000,3152.17975,10398.23973,10.91572,27.66695,
                                                         14.36878,24.35125,15.45800,55.82330)),TRUE)
  expect_equal(all(round(colMeans(.summaryStats(spectra)), 5) == c(1857.00000,2500.05000,12995.77000,
                                                                0.00000,7.69026,6.63410,6.86903,5.01145,55.82330)),TRUE)

})

context("countPeaks")

test_that("countPeaks throws an error for non-MassPeaks lists", {
  expect_error(countPeaks(1:10), "must be a MassPeaks object")
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

context("redResolution")

test_that("redResolution returns expected results",{
  expect_equal(length(mass(redResolution(spectra[[1]],by=2))), 929)
})

context("transfIntensity")

test_that("transfIntensity returns expected results",{
  expect_equal(round(mean(intensity(transfIntensity(spectra[[1]],function(x) log(x+1)))), 5), 1.87533)
})

context("wavSmoothing")

test_that("wavSmoothing returns expected results",{
  expect_equal(round(mean(intensity(wavSmoothing(spectra[1:5])[[5]])), 5), 10.10748)
})



