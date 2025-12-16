# nocov start
make_cdr <- function(definition) {
  force(definition)
  function(
    date_range,
    variables = "aice",
    version = 5,
    file = NULL,
    dir = tempdir(),
    use_cache = FALSE
  ) {
    cdr(
      date_range = date_range,
      variables = variables,
      hemisphere = definition$hemisphere,
      resolution = definition$resolution,
      version = version,
      file = file,
      dir = dir,
      use_cache = use_cache
    )
  }
}
# nocov end

#' Convenience functions
#'
#' These are convenience functions to download, daily or monthly
#' sea ice concentration data from the whole Antarctic or Arctic.
#' For a more complete low level function, see [cdr()].
#'
#' @inheritParams cdr
#'
#' @export
#' @rdname convenience
cdr_antarctic_monthly <- make_cdr(list(
  hemisphere = "south",
  resolution = "monthly"
))

#' @export
#' @rdname convenience
cdr_antarctic_daily <- make_cdr(list(
  hemisphere = "south",
  resolution = "daily"
))

#' @export
#' @rdname convenience
cdr_arctic_monthly <- make_cdr(list(
  hemisphere = "north",
  resolution = "monthly"
))

#' @export
#' @rdname convenience
cdr_arctic_daily <- make_cdr(list(hemisphere = "north", resolution = "daily"))

file_name <- function(url) {
  digest::digest(url)
}


cdr_variables_daily <- list(
  v4 = list(
    aice = "cdr_seaice_conc",
    qa = "qa_of_cdr_seaice_conc",
    stdev = "stdev_of_cdr_seaice_conc",
    interpolation_spatial = "spatial_interpolation_flag",
    interpolation_temporal = "temporal_interpolation_flag",
    aice_bt = "nsidc_bt_seaice_conc",
    aice_nt = "nsidc_nt_seaice_conc"
  ),
  v5 = list(
    aice = "cdr_seaice_conc",
    qa = "cdr_seaice_conc_qa_flag",
    stdev = "cdr_seaice_conc_stdev",
    interpolation_spatial = "cdr_seaice_conc_interp_spatial_flag",
    interpolation_temporal = "cdr_seaice_conc_interp_temporal_flag"
  )
)

cdr_variables_monthly <- list(
  v4 = list(
    aice = "cdr_seaice_conc_monthly",
    qa = "qa_of_cdr_seaice_conc_monthly",
    stdev = "stdev_of_cdr_seaice_conc_monthly",
    aice_bt = "nsidc_bt_seaice_conc_monthly",
    aice_nt = "nsidc_nt_seaice_conc_monthly"
  ),

  v5 = list(
    aice = "cdr_seaice_conc_monthly",
    stdev = "cdr_seaice_conc_monthly_stdev",
    qa = "cdr_seaice_conc_monthly_qa_flag"
  )
)

cdr_variables <- list(
  monthly = cdr_variables_monthly,
  daily = cdr_variables_daily
)

cdr_variables_description <- list(
  aice = "CDR Sea ice concentration.",
  qa = "Quality control flag.",
  stdev = "Sea ice standard deviation.",
  interpolation_spatial = "Flag for spatial interpolation.",
  interpolation_temporal = "Flag for temporal interpolation.",
  aice_bt = "Sea ice concentration of the Bootstrap method.",
  aice_nt = "Sea ice concentration of the Nasa Team method."
)

as_describe <- function(cdr_variables_description) {
  items <- paste0(
    "\\item{`",
    names(cdr_variables_description),
    "`}{",
    unlist(cdr_variables_description),
    "}"
  )

  paste0("\\describe{", paste0(items, collapse = "\n"), "}")
}


variable_name <- function(variables, version, resolution) {
  version <- paste0("v", version)

  cdr_variables[[resolution]][[version]][variables]
}


#' Download sea ice concentration from NSIDC Climate Data Record
#'
#' This is a low-level function to download data from PolarWatch's ERDDAP
#' server.
#'
#' @param date_range Vector of size two with the start and end dates. Supported
#' formats are:
#'   * A Date or POSIXct/POSIXlt object.
#'   * A character vector with ISO format dates: `c("2020-01-01", "2020-12-31")`
#'   * A character vector with year-month: `c("2020-01", "2020-06")` (expands to first/last day of month).
#'   * A character vector with year only: `c("2020", "2021")` (expands to full year).
#' @param variables Character vector with the variables to fetch. Valid values are
#' `r as_describe(cdr_variables_description)`
#' Although not all variables are available in all versions and all resolutions.
#' The `*_nt` and `*_bt` variables are only available in version 4 and the
#' interpolation flags are only available for daily values.
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
#' @param version Version of the dataset. Can be 4 or 5.
#' @param file Character with the file name to use for download. If `NULL`,
#' the file name will be constructed by hashing the request URL.
#' Requests consisting in more than one file will append a number.
#' @param dir Directory where to download the file.
#' @param use_cache Logical indicating whether to not perform the download if
#' the file already exist.
#'
#'
#' @return Path or vector of paths to the downloaded files.
#'
#' @export
cdr <- function(
  date_range = c(NA, NA),

  variables = "aice",
  hemisphere = c("south", "north"),
  resolution = c("monthly", "daily"),

  date_stride = 1,

  xgrid_range = c(NA, NA),
  xgrid_stride = 1,

  ygrid_range = c(NA, NA),
  ygrid_stride = 1,
  version = 4,
  format = "nc",
  file = NULL,
  dir = tempdir(),
  use_cache = FALSE
) {
  if (!(version %in% c(4, 5))) {
    cli::cli_abort("Invalid version. Available versions are 4 and 5.")
  }

  resolution <- resolution[1]
  resolutions <- c("monthly", "daily")
  if (!checkmate::test_choice(resolution, resolutions)) {
    cli::cli_abort(
      "{.arg resolutions} needs to be one of {.val {resolutions}}, not {.val {resolution}}."
    )
  }

  variables_translated <- variable_name(variables, version, resolution)

  missing_variable <- vapply(variables_translated, is.null, logical(1))
  if (any(missing_variable)) {
    bad_variables <- variables[missing_variable]
    cli::cli_abort(c(
      "Variable{?s} not available for this dataset: {.val {bad_variables}}.",
      i = "See possible variables in {.help [{.fun cdr}](icecdr::cdr)}."
    ))
  }

  hemisphere <- hemisphere[1]
  hemispheres <- c("south", "north")
  if (!checkmate::test_choice(hemisphere, hemispheres)) {
    cli::cli_abort(
      "{.arg hemisphere} needs to be one of {.val {hemispheres}}, not {.val {hemisphere}}."
    )
  }

  date_range <- parse_range(date_range)

  if (resolution == "monthly") {
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
  n_chunks <- ceiling(size / size_limit)

  if (n_chunks > 1) {
    class(size) <- "object_size"
    size <- format(size, "auto")
    cli::cli_inform(
      "Request size ({size}) larger than the limit (2 Gb). Downloading in {n_chunks} chunks."
    )

    if (resolution == "daily") {
      unit <- paste0(date_stride, " day")
    }

    if (resolution == "monthly") {
      unit <- paste0(date_stride, " month")
    }

    dates <- seq(date_range[1], date_range[2], by = unit)
    chunks <- as.numeric(cut(dates, n_chunks))

    files <- vapply(
      unique(chunks),
      function(chunk) {
        date_range <- range(dates[chunks == chunk])

        if (!is.null(file)) {
          file <- paste0(
            tools::file_path_sans_ext(file),
            "_",
            formatC(chunk, width = 2, flag = "0"),
            ".",
            tools::file_ext(file)
          )
        }

        cli::cli_inform(
          "Downloading file {chunk} of {n_chunks} ({date_range[1]} to {date_range[2]})."
        )

        cdr(
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
          version = version,
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
    variables = variables_translated,
    hemisphere = hemisphere,
    resolution = resolution,
    date_range = date_range,
    date_stride = date_stride,
    xgrid_range = xgrid_range,
    xgrid_stride = xgrid_stride,
    ygrid_range = ygrid_range,
    ygrid_stride = ygrid_stride,
    version = version,
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

  if (getOption("CDR_DONT_DOWNLOAD", default = FALSE)) {
    return(destination)
  }

  utils::download.file(url, destination)
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
  version = 4,
  format = "nc"
) {
  resolution <- c(daily = "1day", monthly = "mday")[resolution]
  hemisphere <- c(south = "sh", north = "nh")[hemisphere]

  base <- glue::glue(
    "https://polarwatch.noaa.gov/erddap/griddap/nsidcG02202v{version}{hemisphere}{resolution}.{format}"
  )
  date_range_str <- format(date_range, "%Y-%m-%dT00:00:00Z")
  time <- variable_definition(date_range_str, date_stride)
  xgrid <- variable_definition(xgrid_range, xgrid_stride)
  ygrid <- variable_definition(ygrid_range, ygrid_stride)

  query <- paste0(
    glue::glue("{variables}[{time}][{ygrid}][{xgrid}]"),
    collapse = ","
  )

  utils::URLencode(glue::glue("{base}?{query}"))
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
