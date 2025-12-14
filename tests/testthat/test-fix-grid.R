skip_on_cran()
test_that("fix grid works", {
  file_og <- cdr_antarctic_monthly(c("2022-01", "2022-01"), dir = "test_data", use_cache = TRUE)
  file <- tempfile()
  file.copy(file_og, file)

  file_is_valid_grid <- function(file) {
    grid_is_valid(grid_parse(rcdo::cdo_execute(rcdo::cdo_griddes(file))))
  }

  file <- suppressWarnings(cdr_fix_grid(file))

  expect_true(file_is_valid_grid(file))



})
