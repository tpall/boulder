# boulder


> ## bouldeR
In our nature, the giant erratic boulders appear unexpectedly in the forest or on the beach. In our visual communication, they play a similar disruptive role. The use of boulders is not compulsory. [Brand Estonia](https://brand.estonia.ee/design/boulders/)


Parse data from __json__ file downloaded from [Estonian Health Statistics Database](http://pxweb.tai.ee/PXWeb2015/index_en.html) into a data.frame.

```{r }
devtools::install_github("tpall/boulder")
library(boulder)

path_to_PK10.json <- system.file("extdata", "PK10.json", package = "boulder", mustWork = TRUE)
pk10 <- json_to_df(path_to_PK10.json)
```


