
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/tpall/boulder.svg?branch=master)](https://travis-ci.org/tpall/boulder) [![Coverage Status](https://img.shields.io/codecov/c/github/tpall/boulder/master.svg)](https://codecov.io/github/tpall/boulder?branch=master)

boulder - access Estonian health statistics
-------------------------------------------

boulder is designed for querying and downloading data from [Estonian Health Statistics And Health Research Database](http://pxweb.tai.ee/PXWeb2015/index_en.html) (TAI). Name 'Boulder' is from Brand Estonia toolbox [boulders](https://brand.estonia.ee/design/boulders/).

> bouldeR: In our nature, the giant erratic boulders appear unexpectedly in the forest or on the beach. In our visual communication, they play a similar disruptive role. The use of boulders is not compulsory. [Brand Estonia](https://brand.estonia.ee/design/boulders/)

Verbs
-----

Package has two main functions `get_all_tables()` and `pull_table()`.

-   `get_all_tables()` downloads list of available database tables and
-   `pull_table()` downloads your table of interest based on table name.

Table descriptions are available in 'Title' column of data frame produced by `get_all_tables()`. By default `get_all_tables()` uses local table supplied with the package. To download fresh list of database tables from TAI use `local = FALSE` argument.

`pull_table()` converts "." and "..", apparently denoting missing values, to NA-s, and filters out some summary rows, generally coded by "0"-s, to reduce table size in attempt to avoid hitting size limit of POST request. Otherwise, variables and their names are not modified, as prepended dots ".", ".." to the variable names indicate their hierarchy. It's strongly advisable to compare downloaded table to the table available on the TAI website.

Package interacts with pxweb API at TAI. There is also official pxweb API package [rOpenGov/pxweb](https://github.com/rOpenGov/pxweb) allowing interactive browsing through databases.

Installation
------------

Install package from GitHub:

``` r
devtools::install_github("tpall/boulder")
```

Usage
-----

Parse data from **json** file manually downloaded from [Estonian Health Statistics Database](http://pxweb.tai.ee/PXWeb2015/index_en.html) into a data frame.

``` r
library(boulder)
path_to_PK10.json <- system.file("extdata", "PK10.json", package = "boulder", mustWork = TRUE)
pk10 <- json_to_df(path_to_PK10.json)
#> Data source is Estonian Cancer Registry.
```

Download table "RK01" from database:

``` r
# load library
library(boulder)

# check available tables
tabs <- get_all_tables(lang = "en")
tabs
#> # A tibble: 1,688 x 6
#>    Database    Node                         Name  Title    Updated  url   
#>    <chr>       <chr>                        <chr> <chr>    <chr>    <chr> 
#>  1 01Rahvastik Abortions                    RK01  Abortio… 2017-06… http:…
#>  2 01Rahvastik Abortions                    RK11  Abortio… 2017-11… http:…
#>  3 01Rahvastik Abortions                    RK20  Abortio… 2017-06… http:…
#>  4 01Rahvastik Abortions                    RK30  Abortio… 2017-06… http:…
#>  5 01Rahvastik Abortions                    RK40  Abortio… 2017-06… http:…
#>  6 01Rahvastik Abortions                    RK50  Use of … 2017-06… http:…
#>  7 01Rahvastik Abortions                    RK61  Legally… 2017-06… http:…
#>  8 01Rahvastik Abortions                    RK62  Abortio… 2017-06… http:…
#>  9 01Rahvastik Abortions                    RK63  Abortio… 2017-06… http:…
#> 10 01Rahvastik Births and breastfed infants SR01  Live bi… 2017-10… http:…
#> # ... with 1,678 more rows

# dowload table of interest
rk01 <- pull_table("RK01", lang = "en")
rk01
#> # A tibble: 3,876 x 4
#>    Year  County  `Age group`      value
#>    <chr> <chr>   <chr>            <dbl>
#>  1 2000  Estonia All age groups 15331  
#>  2 2000  Estonia 10-14             20.0
#>  3 2000  Estonia 15-17            689  
#>  4 2000  Estonia 18-19           1168  
#>  5 2000  Estonia 20-24           3701  
#>  6 2000  Estonia 25-29           3545  
#>  7 2000  Estonia 30-34           2940  
#>  8 2000  Estonia 35-39           2158  
#>  9 2000  Estonia 40-44            996  
#> 10 2000  Estonia 45-49            110  
#> # ... with 3,866 more rows

# look at the table title and date of last update
comment(rk01)
#>                               Title                             Updated 
#> "Abortions by age group and county"               "2017-06-06T10:16:55"
```
