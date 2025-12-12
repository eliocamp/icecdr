make_nsidc <- function(definition) {
  force(definition)
  function(
    date_range,
    variables = "cdr_seaice_conc",
    file = NULL,
    dir = tempdir(),
    use_cache = FALSE
  ) {
    nsidc(
      date_range = date_range,
      variables = variables,
      hemisphere = definition$hemisphere,
      resolution = definition$resolution,
      file = file,
      dir = dir,
      use_cache = use_cache
    )
  }
}


#' Convenience functions
#'
#' These are convenience functions to download, daily or monthly
#' sea ice concentration data from the whole Antarctic or Arctic.
#'
#' @inheritParams nsidc
#'
#' @export
#' @rdname convenience
antarctic_monthly <- make_nsidc(list(
  hemisphere = "south",
  resolution = "monthly"
))

#' @export
#' @rdname convenience
antarctic_daily <- make_nsidc(list(hemisphere = "south", resolution = "daily"))

#' @export
#' @rdname convenience
arctic_monthly <- make_nsidc(list(hemisphere = "north", resolution = "monthly"))

#' @export
#' @rdname convenience
arctic_daily <- make_nsidc(list(hemisphere = "north", resolution = "daily"))

file_name <- function(url) {
  digest::digest(url)
}

as_mdlist <- function(vector) {
  paste0(paste0("* ", vector), collapse = "\n")
}

#' Download sea ice concentration from NSIDC Climate Data Record V4
#'
#' This is a low-level function to download data from PolarWatch's ERDDAP
#' server.
#'
#' @param date_range Vector of size two with the start and end dates.
#' It can be a Date vector or any vector with a valid [as.Date()] method.
#' @param variables Character vector with the variables to fetch. Valid values are
#' `r as_mdlist(nsidc_variables)`
#' @param hemisphere Character with the hemisphere to download.
#' Can be either "south" or "north".
#' @param resolution Character with the temporal resolution.
#' Can be either "monthly" or "daily".
#' @param date_stride Numeric with the temporal stride.
#' A number greater than 1 means to take only the nth date in the series.
#' @param xgrid_range Numeric vector of size 2 with the range of the x dimension.
#' @param xgrid_stride Numeric with the stride of the x dimension.
#' @param ygrid_range Numeric vector of size 2 with the range of the y dimension.
#' @param ygrid_stride Numeric with the stride of the y dimension.
#' @param format Character with the format.
#' @param file Character with the file name to use for download. If `NULL`,
#' the file name will be constructed by hashing the request URL.
#' Requests consisting in more than one file will append a number.
#' @param dir Directory where to download the file.
#' @param use_cache Logical indicating whether to not perform the download if
#' the file already exist.
#'
#'
#' @return Path or vect
#'
#' @export
nsidc <- function(
  date_range = c(NA, NA),

  variables = "cdr_seaice_conc",
  hemisphere = c("south", "north"),
  resolution = c("monthly", "daily"),

  date_stride = 1,

  xgrid_range = c(NA, NA),
  xgrid_stride = 1,

  ygrid_range = c(NA, NA),
  ygrid_stride = 1,

  format = "nc",
  file = NULL,
  dir = tempdir(),
  use_cache = FALSE
) {
  bad_variables <- setdiff(variables, nsidc_variables)
  if (length(bad_variables) != 0) {
    cli::cli_abort(c(
      "Variable{?s} no available: {.val {bad_variables}}.",
      i = "See possible variables with {.code nsidc_variables}."
    ))
  }

  hemisphere <- hemisphere[1]
  hemispheres <- c("south", "north")
  if (!checkmate::test_choice(hemisphere, hemispheres)) {
    cli::cli_abort(
      "{.arg hemisphere} needs to be one of {.val {hemispheres}}, not {.val {hemisphere}}."
    )
  }

  resolution <- resolution[1]
  resolutions <- c("monthly", "daily")
  if (!checkmate::test_choice(resolution, resolutions)) {
    cli::cli_abort(
      "{.arg resolutions} needs to be one of {.val {resolutions}}, not {.val {resolution}}."
    )
  }

  date_range <- as.Date(date_range)

  if (resolution == "monthly") {
    variables <- glue::glue("{variables}_monthly")
    date_range <- floor_month(date_range)
  }

  hemisphere <- hemisphere[1]
  if (hemisphere == "north") {
    ygrid_limits <- c(5837500.0, -5337500.0)
    xgrid_limits <- c(-3837500.0, 3737500.0)
  }

  if (hemisphere == "south") {
    ygrid_limits <- c(4337500.0, -3937500.0)
    xgrid_limits <- c(-3937500.0, 3937500.0)
  }

  xgrid_range <- replace_limits(xgrid_range, xgrid_limits)
  ygrid_range <- replace_limits(ygrid_range, ygrid_limits)

  size <- 4 *
    request_size(
      variables,
      resolution,
      date_range,
      date_stride,
      xgrid_range,
      xgrid_stride,
      ygrid_range,
      ygrid_stride
    )

  # 2gb file limit
  size_limit <- 2 * 1024 * 1024 * 1024
  nchunks <- ceiling(size / size_limit)
  if (nchunks > 1) {
    size <- utils:::format.object_size(size, "auto")
    cli::cli_inform(
      "Request size ({size}) larger than the limit (2 Gb). Downloading in {nchunks} chunks."
    )

    if (resolution == "daily") {
      unit <- paste0(date_stride, " day")
    }

    if (resolution == "monthly") {
      unit <- paste0(date_stride, " month")
    }

    dates <- seq(date_range[1], date_range[2], by = unit)
    chunks <- as.numeric(cut(dates, 2))

    files <- vapply(
      unique(chunks),
      \(chunk) {
        date_range <- range(dates[chunks == chunk])

        if (!is.null(file)) {
          file <- paste0(
            tools::file_path_sans_ext(file),
            "_",
            formatC(n_chunks, width = 2, flag = "0"),
            ".",
            tools::file_ext(file)
          )
        }
        cli::cli_inform(
          "Downloading file {chunk} of {n_chunks} ({date_range[1]} to {date_range[2]})."
        )

        nsidc(
          date_range = date_range,
          variables = variables,
          hemisphere = hemisphere,
          resolution = resolution,
          date_stride = date_stride,
          xgrid_range = xgrid_range,
          xgrid_stride = xgrid_stride,
          ygrid_range = ygrid_range,
          ygrid_stride = ygrid_stride,
          format = format,
          use_cache = use_cache,
          dir = dir,
          file = file
        )
      },
      character(1)
    )

    return(files)
  }

  url <- nsidc_url(
    variables = variables,
    hemisphere = hemisphere,
    resolution = resolution,
    date_range = date_range,
    date_stride = date_stride,
    xgrid_range = xgrid_range,
    xgrid_stride = xgrid_stride,
    ygrid_range = ygrid_range,
    ygrid_stride = ygrid_stride,
    format = format
  )

  if (is.null(file)) {
    file <- paste0(file_name(url), ".", format)
  }

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  destination <- file.path(dir, file)

  if (use_cache) {
    if (file.exists(destination)) {
      return(destination)
    }
  }

  old <- options(timeout = 60 * 360)
  on.exit(options(old))
  # download.file(url, destination)

  download.file(url, destination)
  return(destination)
}


request_size <- function(
  variables,
  resolution,
  date_range,
  date_stride,
  xgrid_range,
  xgrid_stride,
  ygrid_range,
  ygrid_stride
) {
  if (resolution == "daily") {
    unit <- paste0(date_stride, " day")
  }

  if (resolution == "monthly") {
    unit <- paste0(date_stride, " month")
  }

  ndates <- length(seq(
    date_range[1],
    date_range[2],
    by = unit
  ))

  res <- 25000
  nx <- (abs(xgrid_range[2] - xgrid_range[1]) / xgrid_stride) / res + 1
  ny <- (abs(ygrid_range[2] - ygrid_range[1]) / ygrid_stride) / res + 1
  nvar <- length(variables)

  nx * ny * ndates * nvar
}

nsidc_variables <- c(
  "cdr_seaice_conc",
  "melt_onset_day_cdr_seaice_conc",
  "nsidc_bt_seaice_conc",
  "nsidc_nt_seaice_conc",
  "qa_of_cdr_seaice_conc",
  "spatial_interpolation_flag",
  "stdev_of_cdr_seaice_conc",
  "temporal_interpolation_flag"
)


nsidc_url <- function(
  variables = "cdr_seaice_conc",
  hemisphere = c("south", "north"),
  resolution = c("monthly", "daily"),

  date_range = c(NA, NA),
  date_stride = 1,

  xgrid_range = c(NA, NA),
  xgrid_stride = 1,

  ygrid_range = c(NA, NA),
  ygrid_stride = 1,

  format = "nc"
) {
  resolution <- c(daily = "1day", monthly = "mday")[resolution]
  hemisphere <- c(south = "sh", north = "nh")[hemisphere]

  base <- glue::glue(
    "https://polarwatch.noaa.gov/erddap/griddap/nsidcG02202v4{hemisphere}{resolution}.{format}"
  )
  date_range_str <- format(date_range, "%Y-%m-%dT00:00:00Z")
  time <- variable_definition(date_range_str, date_stride)
  xgrid <- variable_definition(xgrid_range, xgrid_stride)
  ygrid <- variable_definition(ygrid_range, ygrid_stride)

  query <- paste0(
    glue::glue("{variables}[{time}][{ygrid}][{xgrid}]"),
    collapse = ","
  )

  URLencode(glue::glue("{base}?{query}"))
}


replace_limits <- function(range, limits) {
  ifelse(is.na(range), limits, range)
}

variable_definition <- function(range, stride) {
  glue::glue("({range[1]}):{stride}:({range[2]})")
}


floor_month <- function(date) {
  y <- format(date, "%Y")
  m <- format(date, "%m")
  as.Date(glue::glue("{y}-{m}-01"))
}
