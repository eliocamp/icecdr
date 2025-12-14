test_that("character and date range works", {
  range <- c("2015-01-15", "2015-03-13")
  expect_equal(parse_range(range), as.Date(range))

  range <- as.Date(c("2015-01-15", "2015-03-13"))
  expect_equal(parse_range(range), range)
})


test_that("year-month range works", {
  range <- c("2015-01", "2015-03-13")
  expect_equal(parse_range(range), as.Date(c("2015-01-01", range[2])))

  range <- c("2015-01-03", "2015-03")
  expect_equal(parse_range(range), as.Date(c("2015-01-03", "2015-03-31")))
})


test_that("year works", {
  range <- c("2015", "2015-03-13")
  expect_equal(parse_range(range), as.Date(c("2015-01-01", range[2])))

  range <- c("2015", "2015")
  expect_equal(parse_range(range), as.Date(c("2015-01-01", "2015-12-31")))

})

test_that("leap years work", {
  range <- c("2000", "2004-02")
  expect_equal(parse_range(range), as.Date(c("2000-01-01", "2004-02-29")))

  # Skip years (not really relevant to this package, though, but still nice)
  range <- c("2000", "1900-02")
  expect_equal(parse_range(range), as.Date(c("2000-01-01", "1900-02-28")))
})
