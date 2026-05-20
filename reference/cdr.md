# Download sea ice concentration from NSIDC Climate Data Record

This is a low-level function to download data from PolarWatch's ERDDAP
server.

## Usage

``` r
cdr(
  date_range = c(NA, NA),
  variables = "aice",
  hemisphere = c("south", "north"),
  resolution = c("monthly", "daily"),
  date_stride = 1,
  xgrid_range = c(NA, NA),
  xgrid_stride = 1,
  ygrid_range = c(NA, NA),
  ygrid_stride = 1,
  version = 6,
  format = "nc",
  file = NULL,
  dir = tempdir(),
  use_cache = TRUE
)
```

## Arguments

- date_range:

  Vector of size two with the start and end dates. If a vector of size
  one is passed, the value will be recycled to size 2. Supported formats
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

- hemisphere:

  Character with the hemisphere to download. Can be either "south" or
  "north".

- resolution:

  Character with the temporal resolution. Can be either "monthly" or
  "daily".

- date_stride:

  Numeric with the temporal stride. A number greater than 1 means to
  take only the nth date in the series.

- xgrid_range:

  Numeric vector of size 2 with the range of the x dimension.

- xgrid_stride:

  Numeric with the stride of the x dimension.

- ygrid_range:

  Numeric vector of size 2 with the range of the y dimension.

- ygrid_stride:

  Numeric with the stride of the y dimension.

- version:

  Version of the dataset. Can be 4 or 5.

- format:

  Character with the format.

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

Path or vector of paths to the downloaded files.

## Examples

``` r
if (FALSE) { # \dontrun{
cdr(date_range = 2022,
    # Data every 7 days
    date_stride = 7,
    resolution = "daily",
    # Thin the grid by taking every other gridpoint
    xgrid_stride = 2,
    ygrid_stride = 2,
    hemisphere = "north"
    )
} # }
```
