
``` r
tab = jsonlite::fromJSON("/home/robin/CatchDat/TravelAIR/travelAIData/convertedjson/0a913b60-fefa-4cce-8166-4b8db10cd7d6.json")
head(tab$to_loc)
```

    ## [[1]]
    ## [1] -1.216376 54.550790
    ## 
    ## [[2]]
    ## [1] -1.216376 54.550790
    ## 
    ## [[3]]
    ## [1] -1.216376 54.550790
    ## 
    ## [[4]]
    ## [1] -1.216319 54.550710
    ## 
    ## [[5]]
    ## [1] -1.216319 54.550710
    ## 
    ## [[6]]
    ## [1] -1.216319 54.550710

``` r
tab$to_loc[[1]][[1]]
```

    ## [1] -1.216376

``` r
tab$to_loc[[1]][[2]]
```

    ## [1] 54.55079

``` r
tab$to_loc_lon = sapply(tab$to_loc, FUN = function(x) x[[1]])
tab$to_loc_lat = sapply(tab$to_loc, FUN = function(x) x[[2]])
tab$from_loc_lon = sapply(tab$from_loc, FUN = function(x) x[[1]])
tab$from_loc_lat = sapply(tab$from_loc, FUN = function(x) x[[2]])
tab = dplyr::select(tab, -from_loc, -to_loc)
sapply(tab, length)
```

    ##     duration     distance    device_id  method_desc           ts 
    ##           89           89           89           89           89 
    ##   to_loc_lon   to_loc_lat from_loc_lon from_loc_lat 
    ##           89           89           89           89

``` r
write.csv(apply(tab, 2, strtrim, 3)[1:3,])
```

    ## "","duration","distance","device_id","method_desc","ts","to_loc_lon","to_loc_lat","from_loc_lon","from_loc_lat"
    ## "1"," 42"," 22","0a9","Sta","1.4","-1.","54.","-1.","54."
    ## "2"," 42"," 22","0a9","Sta","1.4","-1.","54.","-1.","54."
    ## "3"," 42"," 22","0a9","Sta","1.4","-1.","54.","-1.","54."
