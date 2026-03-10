test_that("cdr_returns files", {
  range <- c("2023-01-01", "2023-05-31")
  file <- with_no_download(cdr_antarctic_monthly(range))

  expect_length(file, 1)
  expect_type(file, "character")
  expect_true(file.exists(file))
})


test_that("long requets are splitted", {
  range <- c("1990-01-01", "2023-05-31")
  suppressMessages(expect_message(
    file <- with_no_download(cdr_arctic_daily(range, file = "arctic.nc"))
  ))

  expect_length(file, 7)
  expect_type(file, "character")

  expected <- vapply(
    1:7,
    function(x) paste0("arctic_0", x, ".nc"),
    character(1)
  )
  expect_equal(basename(file), expected)
})


test_that("cdr errors with bad range", {
  range <- c("1900-01", "2023-01")
  vcr::local_cassette("bad-range")
  expect_error(cdr_antarctic_monthly(range), "Not Found")
})


test_that("cache works", {
  range <- c("2023-01-01", "2023-01")
  suppressMessages(
    file <- with_no_download(cdr_antarctic_monthly(range, use_cache = TRUE))
  )
  info <- file.info(file)
  file2 <- with_no_download(cdr_antarctic_monthly(range, use_cache = TRUE))

  expect_equal(info, file.info(file2))
})


test_that("Error messages", {
  range <- c("2023-01", "2023-01")
  expect_error(
    cdr_antarctic_monthly(
      variables = "random variable",
      range,
      use_cache = TRUE
    ),
    "Variable not available"
  )

  expect_error(cdr(date_range = range, hemisphere = "antarctica"), "hemisphere")

  expect_error(cdr(date_range = range, hemisphere = "antarctica"), "hemisphere")

  expect_error(cdr(date_range = range, resolution = "yearly"), "resolution")
})
