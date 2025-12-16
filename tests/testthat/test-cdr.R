options(CDR_DONT_DOWNLOAD = TRUE)

test_that("cdr_returns files", {
  range <- as.Date(c("2023-01-01", "2023-05-31"))

  file <- cdr_antarctic_monthly(range)

  expect_length(file, 1)
  expect_type(file, "character")
})


test_that("long requets are splitted", {
  range <- as.Date(c("1990-01-01", "2023-05-31"))

  suppressMessages(expect_message(
    file <- cdr_arctic_daily(range, file = "arctic.nc")
  ))

  expect_length(file, 4)
  expect_type(file, "character")

  expected <- vapply(
    1:4,
    function(x) paste0("arctic_0", x, ".nc"),
    character(1)
  )
  expect_equal(basename(file), expected)
})


test_that("cdr downloads", {
  options(CDR_DONT_DOWNLOAD = FALSE)
  range <- as.Date(c("2023-01-01", "2023-02-28"))

  suppressMessages(file <- cdr_antarctic_monthly(range))

  expect_length(file, 1)
  expect_type(file, "character")
  expect_true(file.exists(file))
})


test_that("cache works", {
  range <- as.Date(c("2023-01-01", "2023-02-28"))

  suppressMessages(file <- cdr_antarctic_monthly(range, use_cache = TRUE))
  info <- file.info(file)
  file2 <- cdr_antarctic_monthly(range, use_cache = TRUE)

  expect_equal(info, file.info(file2))
})


test_that("Error messages", {
  options(CDR_DONT_DOWNLOAD = TRUE)
  range <- as.Date(c("2023-01-01", "2023-02-28"))

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
