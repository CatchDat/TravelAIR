
``` r
tab = jsonlite::fromJSON("/home/robin/CatchDat/TravelAIR/travelAIData/convertedjson/0a913b60-fefa-4cce-8166-4b8db10cd7d6.json")
tab = tidyr::unnest(tab)
write.csv(apply(tab, 2, strtrim, 3)[1:3,])
```

    ## "","duration","distance","device_id","method_desc","ts","to_loc","from_loc"
    ## "1"," 42"," 22","0a9","Sta","1.4","-1.","-1."
    ## "2"," 42"," 22","0a9","Sta","1.4","54.","54."
    ## "3"," 42"," 22","0a9","Sta","1.4","-1.","-1."
