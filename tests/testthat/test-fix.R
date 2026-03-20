cdo_installed <- system("cdo -V", ignore.stdout = TRUE) == 0
skip_if(!cdo_installed)

test_that("fix all works", {
  file_og <- "test_data/6d5fafd0d3fa213a0a0943dc0d87ec8e.nc"
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
  file_og <- "test_data/6d5fafd0d3fa213a0a0943dc0d87ec8e.nc"
  file <- tempfile()
  file.copy(file_og, file)

  file_is_valid_grid <- function(file) {
    grid_is_valid(grid_parse(rcdo::cdo_execute(rcdo::cdo_griddes(file))))
  }

  file <- suppressWarnings(cdr_fix_grid(file))

  expect_true(file_is_valid_grid(file))
})


test_that("fix names works", {
  file_og <- "test_data/2f607d15cbb6ed73b98555c196e9eee1.nc"

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
