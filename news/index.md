# Changelog

## icecdr (development version)

### New features

- The `date_range` argument in `CDR()` now accepts a length one vector
  and throws an error if a vector longer than 2 elements is passed.

### Bug Fixes

- `sea_ice_index` handles missing values correctly.

## icecdr 1.2.0

CRAN release: 2026-05-08

- Switches to the CoastWatch ERDDAP due to PolarWatch closure.

- Adds version 6.

### Breaking changes

- Default version bumped to 6.

## icecdr 1.1.0

CRAN release: 2026-03-24

### New features

- The new
  [`sea_ice_index()`](https://eliocamp.github.io/icecdr/reference/sea_ice_index.md)
  function downloads monthly and daily sea ice extent.
- `use_cache` is now `TRUE` by default.

## icecdr 1.0.0

CRAN release: 2026-03-16

- Initial CRAN submission.
