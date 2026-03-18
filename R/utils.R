zero_pad <- function(x, width = 2) {
  formatC(x, width = width, flag = "0")
}

download_files <- function(urls, destinations = NULL) {
  if (is.null(destinations)) {
    destinations <- replicate(length(urls), tempfile())
  }

  return(invisible(unname(mapply(download_file, urls, destinations))))
}

download_file <- function(url, destination = NULL) {
  if (is.null(destination)) {
    destination <- tempfile()
  }

  response <- httr2::request(url) |>
    httr2::req_user_agent(icecdr_user_agent) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_progress(type = "down") |>
    httr2::req_perform(path = destination)

  # When mocking requests for tests, the response is not saved to file, so
  # we need to handle that ourselves.
  if (is.raw(response$body)) {
    writeBin(response$body, destination)
  }

  if (httr2::resp_is_error(response)) {
    info <- error_info(response)
    if (file.exists(destination)) {
      file.remove(destination)
    }
    error_msg <- "Failed to download. Got error {info$status_code}"

    if (length(info$message) != 0) {
      error_msg <- c(paste0(error_msg, " with message:"), info$message)
    } else {
      error_msg <- paste0(error_msg, ".")
    }
    cli::cli_abort(error_msg)
  }
  return(invisible(destination))
}
