
parse_range <- function(x) {
  if (is_dateish(x)) {
    return(x)
  }

  x <- x |>
    as.character() |>
    strsplit("-") |>
    lapply(as.numeric)

  # Support for inputting year
  if (length(x[[1]]) == 1) {
    x[[1]][[2]] <- 1
    x[[1]][[3]] <- 1
  }

  if (length(x[[2]]) == 1) {
    x[[2]][[2]] <- 12
    x[[2]][[3]] <- 31
  }

  # Support for year-month
  if (length(x[[1]]) == 2) {
    x[[1]][[3]] <- 1
  }

  if (length(x[[2]]) == 2) {
    # Need to convert the month into date to correctly
    # account for leap years
    days <- paste0(c(x[[2]], 1), collapse = "-") |>
      as.Date() |>
      days_in_month()

    x[[2]][[3]] <- days
  }

  x |>
    vapply(\(x) paste0(x, collapse = "-"), character(1)) |>
    as.Date()
}



is_dateish <- function(x) {
  inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt")
}


# from lubridate
leap_year <- function(date) {
  if (is.numeric(date)) {
    year <- date
  } else {
    year <- data.table::year(date)
  }
  (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

N_DAYS_IN_MONTHS <- c(
  Jan = 31L,
  Feb = 28L,
  Mar = 31L,
  Apr = 30L,
  May = 31L,
  Jun = 30L,
  Jul = 31L,
  Aug = 31L,
  Sep = 30L,
  Oct = 31L,
  Nov = 30L,
  Dec = 31L
)

days_in_month <- function(x) {
  month_x <- data.table::month(x)
  n_days <- N_DAYS_IN_MONTHS[month_x]
  n_days[month_x == 2 & leap_year(x)] <- 29L
  n_days
}
