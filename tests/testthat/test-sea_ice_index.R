test_that("monthly sea_ice_index gets data", {
  vcr::local_cassette("sea_index_monthly")
  file <- sea_ice_index(
    hemisphere = "south",
    resolution = "monthly",
    use_cache = FALSE
  )

  expect_true(file.exists(file))

  data <- read.csv(file)
  expect_true(nrow(data) > 0)
  expect_identical(colnames(data), c("time", "hemisphere", "extent", "area"))
  expect_true(all(data[["hemisphere"]] == "south"))
})

test_that("monthly sea_ice_index gets data", {
  vcr::local_cassette("sea_index_daily")
  file <- sea_ice_index(
    hemisphere = "north",
    resolution = "daily",
    use_cache = FALSE
  )

  expect_true(file.exists(file))

  data <- read.csv(file)
  expect_true(nrow(data) > 0)
  expect_identical(colnames(data), c("time", "hemisphere", "extent"))
  expect_true(all(data[["hemisphere"]] == "north"))
})


test_that("sea_ice_index throws good errors", {
  expect_error(sea_ice_index(hemisphere = "both"), "hemisphere")
  expect_error(sea_ice_index(resolution = "both"), "resolution")
})


test_that("sea_ice_index cache works", {
  vcr::local_cassette("sea_index_monthly")
  dir <- tempfile()
  expect_message(
    file <- sea_ice_index(dir = dir, use_cache = TRUE),
    "Downloading"
  )

  info <- file.info(file)
  expect_message(
    file2 <- sea_ice_index(dir = dir, use_cache = TRUE),
    "existing"
  )

  expect_equal(info, file.info(file2))
})
