sophisthse
==========

R package to download data from [sophist.hse.ru](http://sophist.hse.ru/). The package `sophisthse` may be installed via:
```r
library("devtools")
install_github("bdemeshev/sophisthse")
```


Basic usage example:
```r
library("sophisthse")
df <- sophisthse("WAG_Y")
```

Two more advanced usage examples:
```r
wagez <- sophisthse("WAG_Y", output = "zoo")
wgpd <- sophisthse(c("WAG_Y","GDPVA_Y"))
wagez <- sophisthse(c("WAG_Y","GDPVA_Y"), output = "data.frame")
```