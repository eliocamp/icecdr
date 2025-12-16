#' @export
#' @rdname fixes
cdr_fix_names <- function(files) {
  unname(vapply(files, cdr_fix_names_one, character(1)))
}

cdr_fix_names_one <- function(file) {
  if (!file.exists(file)) {
    cli::cli_abort("File {.file {file}} does not exist.")
  }

  rlang::check_installed(c("rcdo", "ncdf4"))

  old_variables <- strsplit(
    trimws(rcdo::cdo_execute(rcdo::cdo_showname(file))),
    split = " "
  )[[1]]

  vars <- unlist(cdr_variables)
  vars <- vars[!duplicated(vars)] # unique(x) removes names :(

  vars <- vars[vars %in% old_variables]

  new_variables <- vapply(
    strsplit(names(vars), split = ".", fixed = TRUE),
    function(x) x[3],
    character(1)
  )

  names <- paste0(
    paste(old_variables, new_variables, sep = ","),
    collapse = ","
  )

  out <- rcdo::cdo_execute(rcdo::cdo_chname(file, names))
  file.rename(out, file)
  file
}
