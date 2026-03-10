#' Minor fixes to CDR files.
#'
#' `cdr_fix()` "fixes" some inconsistencies in the raw data.
#' It standardised variable names between versions and temporal resolutions and
#' adds projection information to the grid definition so CDO can compute the
#' area of each gridpoint to compute weighted means or  area integrals.
#' Both require the `rcdo` package and CDO installed.
#'
#' @param files Path to the files.
#' @param fix Character vector with the fixes to implement.
#'
#' @details
#' `cdr_fix_names()` and `cdr_fix_grid()` are aliases for `cdr_fix(fix = "names")`
#' and `cdr_fix(fix = "grid")`, respectively.
#' Using `cdr_fix()` to fix both at the same time is more efficient as it
#' recreates each file only once.
#'
#' @return The path to the modified files.
#'
#' @examples
#' \dontrun{
#' cdr_antarctic_monthly(c("2022-01", "2022-01")) |>
#'   cdr_fix()
#' }
#'
#' @export
#' @rdname fixes
cdr_fix <- function(files, fix = c("names", "grid")) {
  vapply(files, cdr_fix_one, fix = fix, FUN.VALUE = character(1))
}

#' @export
#' @rdname fixes
cdr_fix_names <- function(files) {
  cdr_fix(files, fix = "names")
}

#' @export
#' @rdname fixes
cdr_fix_grid <- function(files) {
  cdr_fix(files, fix = "grid")
}


cdr_fix_one <- function(file, fix = c("names", "grid")) {
  if (!file.exists(file)) {
    cli::cli_abort("File {.file {file}} does not exist.")
  }

  rlang::check_installed(c("rcdo", "ncdf4"))

  op <- file

  if ("grid" %in% fix) {
    op <- cdo_fix_grid(file, op)
  }

  if ("names" %in% fix) {
    op <- cdo_fix_name(file, op)
  }

  if (is.character(op)) {
    return(op)
  }

  out <- rcdo::cdo_execute(op)
  file.rename(out, file)

  return(file)
}


cdo_fix_grid <- function(file, prev_op = file) {
  grid <- rcdo::cdo_execute(rcdo::cdo_griddes(file))

  grid <- grid_parse(grid)

  if (grid_is_valid(grid)) {
    return(prev_op)
  }

  grid$gridtype <- "projection"

  fnc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(fnc))

  # Remove possible old params
  grid <- grid[!grepl("grid_mapping", grid)]
  grid <- grid[!grepl("grid_mapping_name", grid)]
  grid <- grid[!grepl("proj_params", grid)]
  grid <- grid[!grepl("scanningMode", grid)]

  proj4 <- ncdf4::ncatt_get(fnc, 0, "grid_mapping_proj4text")

  if (!proj4$hasatt) {
    cli::cli_abort("no crs")
  }

  grid_mapping_name <- ncdf4::ncatt_get(fnc, 0, "grid_mapping_name")$value

  grid$grid_mapping <- "crs"
  grid$grid_mapping_name <- grid_mapping_name
  grid$proj_params <- glue::glue("\"{proj4$value}\"")

  grid_file <- grid_write(grid, tempfile())

  op <- rcdo::cdo_setgrid(prev_op, grid_file)

  return(op)
}

grid_parse <- function(grid) {
  grid <- grid[!grepl("^#", grid)]

  splitted <- strsplit(grid, split = "=")

  names <- vapply(splitted, function(x) trimws(x[[1]]), character(1))
  values <- lapply(splitted, function(x) trimws(paste0(x[-1], collapse = "=")))

  stats::setNames(values, names)
}

grid_write <- function(grid, file) {
  text <- paste0(names(grid), " = ", unlist(grid, recursive = FALSE))
  writeLines(text, file)
  return(invisible(file))
}

grid_is_valid <- function(grid) {
  grid$gridtype == "projection" &
    !is.null(grid$grid_mapping) &
    !is.null(grid$grid_mapping_name) &
    !is.null(grid$proj_params)
}


cdo_fix_name <- function(file, prev_op = file) {
  old_variables <- strsplit(
    trimws(rcdo::cdo_execute(rcdo::cdo_showname(file))),
    split = " "
  )[[1]]

  vars <- unlist(cdr_variables)
  vars <- vars[!duplicated(vars)] # unique(x) removes names :(

  vars <- vars[vars %in% old_variables]

  if (length(vars) == 0) {
    cli::cli_inform(
      "No standard variable name found, returning unchanged file"
    )
    return(prev_op)
  }

  new_variables <- vapply(
    strsplit(names(vars), split = ".", fixed = TRUE),
    function(x) x[3],
    character(1)
  )

  names <- paste0(
    paste(old_variables, new_variables, sep = ","),
    collapse = ","
  )
  op <- rcdo::cdo_chname(prev_op, names)
  return(op)
}
