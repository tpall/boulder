[![Build Status](https://travis-ci.org/tpall/boulder.svg?branch=master)](https://travis-ci.org/tpall/boulder)

# boulder


> bouldeR: In our nature, the giant erratic boulders appear unexpectedly in the forest or on the beach. In our visual communication, they play a similar disruptive role. The use of boulders is not compulsory. [Brand Estonia](https://brand.estonia.ee/design/boulders/)

Install package from GitHub:
``` r
devtools::install_github("tpall/boulder")
```

Parse data from __json__ file downloaded from [Estonian Health Statistics Database](http://pxweb.tai.ee/PXWeb2015/index_en.html) into a data.frame.

``` r
library(boulder)

path_to_PK10.json <- system.file("extdata", "PK10.json", package = "boulder", mustWork = TRUE)
pk10 <- json_to_df(path_to_PK10.json)
#> Data source is Estonian Cancer Registry.
```

Download table "RK01" from database:
``` r
library(boulder)
vars <- list_variables()
vars
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
rk01 <- dl_tai_table("RK01", lang = "et")
#> Warning: JSON string contains (illegal) UTF8 byte-order-mark!
rk01
#> # A tibble: 3,876 x 4
#>    Aasta Maakond         Vanuserühm Abordid
#>    <chr>   <chr>              <chr>   <chr>
#>  1  2000   Eesti Vanuserühmad kokku   15331
#>  2  2000   Eesti              10-14      20
#>  3  2000   Eesti              15-17     689
#>  4  2000   Eesti              18-19    1168
#>  5  2000   Eesti              20-24    3701
#>  6  2000   Eesti              25-29    3545
#>  7  2000   Eesti              30-34    2940
#>  8  2000   Eesti              35-39    2158
#>  9  2000   Eesti              40-44     996
#> 10  2000   Eesti              45-49     110
#> # ... with 3,866 more rows
```
