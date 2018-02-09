[![Build Status](https://travis-ci.org/tpall/boulder.svg?branch=master)](https://travis-ci.org/tpall/boulder)
[![Coverage Status](https://img.shields.io/codecov/c/github/tpall/boulder/master.svg)](https://codecov.io/github/tpall/boulder?branch=master)

## boulder

boulder is designed for quering and downloading data from Estonian Health Statistics And Health Research Database (TAI). 
Name 'Boulder' is from Brand Estonia toolbox [boulders](https://brand.estonia.ee/design/boulders/).

> bouldeR: In our nature, the giant erratic boulders appear unexpectedly in the forest or on the beach. In our visual communication, they play a similar disruptive role. The use of boulders is not compulsory. [Brand Estonia](https://brand.estonia.ee/design/boulders/)

Package has two main functions `get_all_tables()` and `pull_table()`.

- `get_all_tables()` downloads list of available database tables and 
- `pull_table()` downloads your table of interest based on table name. 

Table descriptions are available in 'Title' column of data frame produced by `get_all_tables()`. 
By default `get_all_tables()` uses local table supplied with the package.
To download fresh list of database tables from TAI use `local = FALSE` argument.


Package interacts with pxweb API at TAI. 
There is also official pxweb API package [rOpenGov/pxweb](https://github.com/rOpenGov/pxweb) allowing interactive browsing trough databases.


## Example

Install package from GitHub:
``` r
devtools::install_github("tpall/boulder")
```

Parse data from __json__ file manually downloaded from [Estonian Health Statistics Database](http://pxweb.tai.ee/PXWeb2015/index_en.html) into a data frame.

``` r
library(boulder)

path_to_PK10.json <- system.file("extdata", "PK10.json", package = "boulder", mustWork = TRUE)
pk10 <- json_to_df(path_to_PK10.json)
#> Data source is Estonian Cancer Registry.
```

Download table "RK01" from database:
``` r
library(boulder)
tabs <- get_all_tables(lang = "en")
tabs
#> # A tibble: 1,685 x 5
#>       Database      Node  Name
#>          <chr>     <chr> <chr>
#>  1 01Rahvastik 03Abordid  RK01
#>  2 01Rahvastik 03Abordid  RK11
#>  3 01Rahvastik 03Abordid  RK20
#>  4 01Rahvastik 03Abordid  RK30
#>  5 01Rahvastik 03Abordid  RK40
#>  6 01Rahvastik 03Abordid  RK50
#>  7 01Rahvastik 03Abordid  RK61
#>  8 01Rahvastik 03Abordid  RK62
#>  9 01Rahvastik 03Abordid  RK63
#> 10 01Rahvastik  05Eluiga  OE12
#> # ... with 1,675 more rows, and 2 more variables: Title <chr>,
#> #   Updated <chr>

rk01 <- pull_table("RK01", lang = "en")
#> Warning: JSON string contains (illegal) UTF8 byte-order-mark!
rk01
#> # A tibble: 3,876 x 4
#>     Year  County    `Age group` Abortions
#>    <chr>   <chr>          <chr>     <dbl>
#>  1  2000 Estonia All age groups     15331
#>  2  2000 Estonia          10-14        20
#>  3  2000 Estonia          15-17       689
#>  4  2000 Estonia          18-19      1168
#>  5  2000 Estonia          20-24      3701
#>  6  2000 Estonia          25-29      3545
#>  7  2000 Estonia          30-34      2940
#>  8  2000 Estonia          35-39      2158
#>  9  2000 Estonia          40-44       996
#> 10  2000 Estonia          45-49       110
#> # ... with 3,866 more rows
```
