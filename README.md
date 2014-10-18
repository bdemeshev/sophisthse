sophisthse
==========


Пакет `sophisthse` предназначен для скачивания временных рядов с [sophist.hse.ru](http://sophist.hse.ru/). Пакет можно установить командами:
```r
library("devtools")
install_github("bdemeshev/sophisthse")
```


Пример простого использования:
```r
library("sophisthse")
df <- sophisthse("WAG_Y")
```

Еще несколько примеров использования:
```r
wagez <- sophisthse("WAG_Y", output = "zoo")
wgpd <- sophisthse(c("WAG_Y","GDPVA_Y"))
wagez <- sophisthse(c("WAG_Y","GDPVA_Y"), output = "data.frame")
```

Метаданные о рядах сохраняются в аттрибуте `metadata`. Их можно увидеть с помощью команд:
```r
df <- sophisthse("WAG_Y")
info <- attr(df,"metadata")
info
```

### English translation:


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

Some more examples:
```r
wagez <- sophisthse("WAG_Y", output = "zoo")
wgpd <- sophisthse(c("WAG_Y","GDPVA_Y"))
wagez <- sophisthse(c("WAG_Y","GDPVA_Y"), output = "data.frame")
```

Metadata is saved in the attribute `metadata` and may be accessed via:
```r
df <- sophisthse("WAG_Y")
info <- attr(df,"metadata")
info
```