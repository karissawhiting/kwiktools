# kwiktools

A collection of small utility functions for common data analysis tasks: resolving data file paths, saving/loading dated data snapshots, formatting `gt`/`gtsummary` tables, and cleaning up data frame labels.

## Installation

You can install the development version of kwiktools from GitHub with:

``` r
# install.packages("pak")
pak::pkg_install("karissawhiting/kwiktools")
```

## Functions

### `get_data_path()`

Resolves a data file path to a server location (if mounted) or falls back to a local path via `here::here()`. Optionally inserts a date into the filename.

``` r
library(kwiktools)

# With SERVER_BASE set in .Renviron
get_data_path("patients.RData")

# Insert a date before the file extension
get_data_path("patients.RData", data_date = "2026-08-19")
```

### `save_date()` / `save_with_date()`

Wrappers around `save()` and `readr::write_csv()` that save an `.RData` or `.csv` file with today's date appended to the filename.

``` r
save_date(df, here::here("data", "patients.RData"))
save_with_date(df, here::here("data", "patients.RData"))
```

### `load_most_recent()`

Loads the most recently dated (or most recently modified) `.RData` file from a directory.

``` r
load_most_recent(here::here("data"), filename_keyword = "patients")
```

### `names_to_labels()`

Converts data frame column names (e.g. `snake_case`) into friendly, title-case labels.

``` r
names_to_labels(df)
```
