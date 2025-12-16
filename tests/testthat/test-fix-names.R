cdo_installed <- system("cdo -V", ignore.stdout = TRUE) == 0
skip_if(!cdo_installed)
options(CDR_DONT_DOWNLOAD = FALSE)
test_that("fix names works", {
  file_og <- cdr_antarctic_monthly(
    c("2022-01", "2022-01"),
    dir = "test_data",
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
