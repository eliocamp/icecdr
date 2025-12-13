#' Fix the file grid definition to use with CDO
#'
#' The files served by the ERDDAP server lack projection information so CDO
#' can't compute the area of each gridpoint to compute weighted means or
#' area integrals.
#'
#' @param file Path to the file.
#'
#' @return The path to the modified file.
#'
#' @export
cdo_fix_grid <- function(file) {
  rlang::check_installed(c("rcdo", "ncdf4"))

  grid <- rcdo::cdo_execute(rcdo::cdo_griddes(file))

  grid <- grid_parse(grid)

  if (grid_is_valid(grid)) {
    return(file)
  }

  grid$gridtype <- "projection"
  # grid$scanningMode <- NULL

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

  out <- rcdo::cdo_execute(rcdo::cdo_setgrid(file, grid_file))

  file.rename(out, file)
  return(file)
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
