# Convenience functions

These are convenience functions to download, daily or monthly sea ice
concentration data from the whole Antarctic or Arctic. For a more
complete low level function, see
[`cdr()`](https://eliocamp.github.io/icecdr/reference/cdr.md).

## Usage

``` r
cdr_antarctic_monthly(
  date_range,
  variables = "aice",
  version = 6,
  file = NULL,
  dir = tempdir(),
  use_cache = TRUE
)

cdr_antarctic_daily(
  date_range,
  variables = "aice",
  version = 6,
  file = NULL,
  dir = tempdir(),
  use_cache = TRUE
)

cdr_arctic_monthly(
  date_range,
  variables = "aice",
  version = 6,
  file = NULL,
  dir = tempdir(),
  use_cache = TRUE
)

cdr_arctic_daily(
  date_range,
  variables = "aice",
  version = 6,
  file = NULL,
  dir = tempdir(),
  use_cache = TRUE
)
```

## Arguments

- date_range:

  Vector of size two with the start and end dates. Supported formats
  are:

  - A Date or POSIXct/POSIXlt object.

  - A character vector with ISO format dates:
    `c("2020-01-01", "2020-12-31")`

  - A character vector with year-month: `c("2020-01", "2020-06")`
    (expands to first/last day of month).

  - A character vector with year only: `c("2020", "2021")` (expands to
    full year).

- variables:

  Character vector with the variables to fetch. Valid values are

  `qa`

  :   Quality control flag.

  `stdev`

  :   Sea ice standard deviation.

  `interpolation_spatial`

  :   Flag for spatial interpolation.

  `interpolation_temporal`

  :   Flag for temporal interpolation.

  `aice_bt`

  :   Sea ice concentration of the Bootstrap method.

  `aice_nt`

  :   Sea ice concentration of the Nasa Team method.

  Although not all variables are available in all versions and all
  resolutions. The `*_nt` and `*_bt` variables are only available in
  version 4 and the interpolation flags are only available for daily
  values.

- version:

  Version of the dataset. Can be 4 or 5.

- file:

  Character with the file name to use for download. If `NULL`, the file
  name will be constructed by hashing the request URL. Requests
  consisting in more than one file will append a number.

- dir:

  Directory where to download the file.

- use_cache:

  Logical indicating whether to not perform the download if the file
  already exist.

## Value

A string vector with the location of the downloaded files

## Examples

``` r
if (FALSE) { # \dontrun{
cdr_antarctic_monthly(c("2022-01", "2022-01"))
} # }

```
