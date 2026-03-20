# Minor fixes to CDR files.

`cdr_fix()` "fixes" some inconsistencies in the raw data. It
standardised variable names between versions and temporal resolutions
and adds projection information to the grid definition so CDO can
compute the area of each gridpoint to compute weighted means or area
integrals. Both require the `rcdo` package and CDO installed.

## Usage

``` r
cdr_fix(files, fix = c("names", "grid"))

cdr_fix_names(files)

cdr_fix_grid(files)
```

## Arguments

- files:

  Path to the files.

- fix:

  Character vector with the fixes to implement.

## Value

The path to the modified files.

## Details

`cdr_fix_names()` and `cdr_fix_grid()` are aliases for
`cdr_fix(fix = "names")` and `cdr_fix(fix = "grid")`, respectively.
Using `cdr_fix()` to fix both at the same time is more efficient as it
recreates each file only once.

## Examples

``` r
if (FALSE) { # \dontrun{
cdr_antarctic_monthly(c("2022-01", "2022-01")) |>
  cdr_fix()
} # }
```
