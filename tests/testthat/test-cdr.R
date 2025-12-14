

options(CDR_DONT_DOWNLOAD = TRUE)

test_that("cdr_returns files", {
  range <- as.Date(c("2023-01-01", "2023-05-31"))

  expect_message(file <- cdr_antarctic_monthly(range))

  expect_length(file, 1)
  expect_type(file, "character")

})


test_that("long requets are splitted", {
  range <- as.Date(c("1990-01-01", "2023-05-31"))

  # Expect 4 messages: inform of chunking and then 3 downloads.
  expect_message(file <- cdr_antarctic_daily(range)) |>
    expect_message() |>
    expect_message() |>
    expect_message()

  expect_length(file, 3)
  expect_type(file, "character")

})


options(CDR_DONT_DOWNLOAD = FALSE)

test_that("cdr downloads", {
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
