# icecdr (development version)

## New features

-   The `date_range` argument in `CDR()` now accepts a length one vector and throws an error if a vector longer than 2 elements is passed.

## Bug Fixes

-   `sea_ice_index` handles missing values correctly.

# icecdr 1.2.0

-   Switches to the CoastWatch ERDDAP due to PolarWatch closure.

-   Adds version 6.

## Breaking changes

-   Default version bumped to 6.

# icecdr 1.1.0

## New features

-   The new `sea_ice_index()` function downloads monthly and daily sea ice extent.
-   `use_cache` is now `TRUE` by default.

# icecdr 1.0.0

-   Initial CRAN submission.
