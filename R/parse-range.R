
parse_range <- function(x) {
  if (is_dateish(x)) {
    return(x)
  }

  x <- strsplit(as.character(x), "-")
  x <- lapply(x, as.numeric)

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
    days <- days_in_month(as.Date(paste0(c(x[[2]], 1), collapse = "-")))

    x[[2]][[3]] <- days
  }


  as.Date(vapply(x, function(x) paste0(x, collapse = "-"), character(1)))
}



is_dateish <- function(x) {
  inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt")
}


# from lubridate
leap_year <- function(date) {
  if (is.numeric(date)) {
    year <- date
  } else {
    year <- year(date)
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

year <- function(date) {
  as.numeric(format(date, "%Y"))
}

month <- function(date) {
  as.numeric(format(date, "%m"))
}

days_in_month <- function(x) {
  month_x <- month(x)
  n_days <- N_DAYS_IN_MONTHS[month_x]
  n_days[month_x == 2 & leap_year(x)] <- 29L
  n_days
}
