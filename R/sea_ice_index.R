#' Download sea ice index data.
#'
#' The NSIDC sea ice index product provides computed sea ice extent for each hemisphere
#' on daily and monthly resolution.
#'
#' @inheritParams cdr
#'
#' @details
#' The returned files are not the raw files, but contain minor format modifications
#' for ease of use. Instead of `year` and `month` columns, there is a single `time`
#' column.
#' Instead of a `region` column with `"S"` or `"N"` values, there is a `hemisphere`
#' column with values `"south"` and `"north"` (for consistency with the function
#' arguments).
#' Column names are all in lower case and are the same for the daily and monthly
#' products, except for an `area` column that is only available in the monthly product.
#' Area and extent values are expressed in km^2.
#'
#' @returns The route to the file name.
#'
#' @examples
#' \dontrun{
#' sea_ice_index("south", "monthly")
#' }
#'
#' @export
sea_ice_index <- function(
  hemisphere = c("south", "north"),
  resolution = c("monthly", "daily"),
  file = NULL,
  dir = tempdir(),
  use_cache = TRUE
) {
  resolution <- resolution[1]
  resolutions <- c("monthly", "daily")
  if (!checkmate::test_choice(resolution, resolutions)) {
    cli::cli_abort(
      "{.arg resolutions} needs to be one of {.val {resolutions}}, not {.val {resolution}}."
    )
  }

  hemisphere <- hemisphere[1]
  hemispheres <- c("south", "north")
  if (!checkmate::test_choice(hemisphere, hemispheres)) {
    cli::cli_abort(
      "{.arg hemisphere} needs to be one of {.val {hemispheres}}, not {.val {hemisphere}}."
    )
  }

  source <- glue::glue("{hemisphere}_{resolution}")
  if (is.null(file)) {
    file <- glue::glue("sea-ice-index_{source}.csv")
  }

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  destination <- file.path(dir, file)
  source_file <- paste0(destination, ".source")

  if (use_cache) {
    if (all(file.exists(c(source_file, destination)))) {
      existing_source <- readLines(source_file)
      if (source == existing_source) {
        cli::cli_inform("Returning existing file.")
        return(destination)
      }
    }
  }

  h <- if (hemisphere == "south") "S" else "N"

  cli::cli_inform("Downloading data.")

  if (resolution == "daily") {
    url <- glue::glue(
      "https://noaadata.apps.nsidc.org/NOAA/G02135/{hemisphere}/daily/data/{h}_seaice_extent_daily_v4.0.csv"
    )

    destination <- download_files(url, destination)
    destination <- fix_daily(destination, hemisphere)
  } else {
    m <- zero_pad(1:12)
    files <- glue::glue("{h}_{m}_extent_v4.0.csv")
    urls <- glue::glue(
      "https://noaadata.apps.nsidc.org/NOAA/G02135/{hemisphere}/monthly/data/{files}"
    )

    tempfiles <- download_files(urls)
    destination <- merge_monthly_files(tempfiles, destination)
  }

  writeLines(source, source_file)

  return(destination)
}

fix_daily <- function(file, hemisphere) {
  data <- utils::read.csv(file, header = FALSE, skip = 2)

  data$time <- as.POSIXct(
    as.Date(
      glue::glue("{data$V1}-{data$V2}-{data$V3}")
    ),
    tz = "utc"
  )
  data$extent <- data$V4
  data$hemisphere <- hemisphere
  data$extent <- data$extent * 1e12

  data <- data[, c("time", "hemisphere", "extent")]
  utils::write.csv(data, file, row.names = FALSE)
  return(file)
}

merge_monthly_files <- function(in_files, out_file) {
  data <- lapply(in_files, utils::read.csv)
  data <- Reduce(rbind, data)

  data$time <- as.POSIXct(
    as.Date(
      glue::glue("{data$year}-{data$mo}-01")
    ),
    tz = "utc"
  )
  data$hemisphere <- ifelse(trimws(data$region) == "S", "south", "north")
  data$extent <- data$extent * 1e12
  data$area <- data$area * 1e12

  data <- data[, c("time", "hemisphere", "extent", "area")]
  utils::write.csv(data, out_file, row.names = FALSE)
  return(out_file)
}
