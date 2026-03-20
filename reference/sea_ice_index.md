# Download sea ice index data.

The NSIDC sea ice index product provides computed sea ice extent for
each hemisphere on daily and monthly resolution.

## Usage

``` r
sea_ice_index(
  hemisphere = c("south", "north"),
  resolution = c("monthly", "daily"),
  file = NULL,
  dir = tempdir(),
  use_cache = TRUE
)
```

## Arguments

- hemisphere:

  Character with the hemisphere to download. Can be either "south" or
  "north".

- resolution:

  Character with the temporal resolution. Can be either "monthly" or
  "daily".

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

The route to the file name.

## Details

The returned files are not the raw files, but contain minor format
modifications for ease of use. Instead of `year` and `month` columns,
there is a single `time` column. Instead of a `region` column with `"S"`
or `"N"` values, there is a `hemisphere` column with values `"south"`
and `"north"` (for consistency with the function arguments). Column
names are all in lower case and are the same for the daily and monthly
products, except for an `area` column that is only available in the
monthly product. Area and extent values are expressed in km^2.

## Examples

``` r
if (FALSE) { # \dontrun{
sea_ice_index("south", "monthly")
} # }
```
