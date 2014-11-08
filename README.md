sophisthse
==========


Пакет `sophisthse` предназначен для скачивания временных рядов с [sophist.hse.ru](http://sophist.hse.ru/). Пакет можно установить командами:
```r
install.packages("devtools")
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

Метаданные о рядах сохраняются в атрибуте `metadata`. Их можно увидеть с помощью команд:
```r
df <- sophisthse("WAG_Y")
info <- attr(df,"metadata")
info
```

Не очень срочные планы по развитию:
* команда для получения списка всех рядов
* убрать warning при скачивании рядов с пропущенными данными. Всё работает корректно, поэтому просто разобраться, где он возникает и убрать.
* может наброситься на разномастные таблицы? Данные опросов можно скачивать целиком без всяких пакетов после регистрации.

### English translation:


R package to download data from [sophist.hse.ru](http://sophist.hse.ru/). The package `sophisthse` may be installed via:
```r
install.packages("devtools")
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