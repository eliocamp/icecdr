cdo_installed <- system("cdo -V", ignore.stdout = TRUE) == 0
skip_if(!cdo_installed)

test_that("fix all works", {
  file_og <- cdr_antarctic_monthly(
    c("2022-01", "2022-01"),
    dir = here::here("tests/testthat/test_data"),
    use_cache = TRUE
  )
  file <- tempfile()
  file.copy(file_og, file)

  file_is_valid_grid <- function(file) {
    grid_is_valid(grid_parse(rcdo::cdo_execute(rcdo::cdo_griddes(file))))
  }

  file <- suppressWarnings(cdr_fix(file))

  expect_true(file_is_valid_grid(file))

  get_vars <- function(file) {
    trimws(rcdo::cdo_execute(rcdo::cdo_showname(file)))
  }

  expect_equal(
    get_vars(file),
    "aice"
  )
})


test_that("fix grid works", {
  file_og <- cdr_antarctic_monthly(
    c("2022-01", "2022-01"),
    dir = here::here("tests/testthat/test_data"),
    use_cache = TRUE
  )
  file <- tempfile()
  file.copy(file_og, file)

  file_is_valid_grid <- function(file) {
    grid_is_valid(grid_parse(rcdo::cdo_execute(rcdo::cdo_griddes(file))))
  }

  file <- suppressWarnings(cdr_fix_grid(file))

  expect_true(file_is_valid_grid(file))
})


test_that("fix names works", {
  file_og <- cdr_antarctic_monthly(
    c("2022-01", "2022-01"),
    dir = here::here("tests/testthat/test_data"),
    variables = c("aice", "aice_bt"),
    version = 4,
    use_cache = TRUE
  )

  file <- tempfile()
  file.copy(file_og, file)

  file <- cdr_fix_names(file)

  get_vars <- function(file) {
    trimws(rcdo::cdo_execute(rcdo::cdo_showname(file)))
  }

  expect_equal(
    get_vars(file),
    "aice aice_bt"
  )

  files <- replicate(2, {
    file <- tempfile()
    file.copy(file_og, file)
    file
  })

  files <- cdr_fix_names(files)

  expect_equal(
    get_vars(files[1]),
    "aice aice_bt"
  )

  expect_equal(
    get_vars(files[2]),
    "aice aice_bt"
  )
})
