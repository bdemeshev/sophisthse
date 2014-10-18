sophisthse
==========

R package to download data from sophist.hse.ru

Basic usage example:
```r
df <- sophisthse("WAG_Y")
```

Two more advanced usage examples:
```r
wagez <- sophisthse("WAG_Y", output = "zoo")
wgpd <- sophisthse(c("WAG_Y","GDPVA_Y"))
```