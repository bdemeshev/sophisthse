[![Travis-CI Build Status](https://travis-ci.org/bdemeshev/sophisthse.svg?branch=master)](https://travis-ci.org/bdemeshev/sophisthse) ![](http://cranlogs.r-pkg.org/badges/sophisthse)

sophisthse
==========


Пакет `sophisthse` предназначен для скачивания временных рядов с [sophist.hse.ru](http://sophist.hse.ru/). Пакет выложен на официальный репозиторий CRAN и ставится стандартно:
```r
install.packages("sophisthse")
```
Для новичков в R: Пакеты устанавливаются один раз, и каждый раз для скачанивания рядов выполнять эту команду совершенно ненужно :)


Разработка пакета ведётся на гитхабе. Если есть более свежая версия пакета, чем на CRAN, то её можно поставить командами:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/sophisthse")
```



Пример простого использования:
```r
library("sophisthse")
df <- sophisthse("WAG_Y")
```

Еще несколько примеров использования:
```r
wagez <- sophisthse("WAG_Y", output = "zoo")
wgpd <- sophisthse(c("WAG_Y", "GDPVA_Y"))
wagez <- sophisthse(c("WAG_Y", "GDPVA_Y"), output = "data.frame")
```

Метаданные о рядах сохраняются в атрибуте `metadata`. Их можно увидеть с помощью команд:
```r
df <- sophisthse("WAG_Y")
info <- attr(df, "metadata")
info
```

Полный список рядов доступных для скачивания удобно глянуть в Rstudio:
```r
View(series_info)
```
Без Rstudio просто `series_info`.


Список актуальных доступных таблиц можно получить командой:
```r
sophisthse_tables()
```

Есть очень короткое введение в пакет [в виде виньетки](http://htmlpreview.github.io/?https://github.com/bdemeshev/sophisthse/blob/master/inst/doc/sophisthse_intro.html).


Не очень срочные планы по развитию:

- [ ] ряды пересматриваются. Добавить дату скачивания? Вести архив обновлений?



### English translation:


R package to download data from [sophist.hse.ru](http://sophist.hse.ru/). The package `sophisthse` is available on CRAN and may be installed via:
```r
install.packages("sophisthse")
```

If there is more recent version of the package on github, you may install it with commands:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/sophisthse")
```


Basic usage example:
```r
library("sophisthse")
df <- sophisthse("WAG_Y")
```

Some more examples:
```r
wagez <- sophisthse("WAG_Y", output = "zoo")
wgpd <- sophisthse(c("WAG_Y", "GDPVA_Y"))
wagez <- sophisthse(c("WAG_Y", "GDPVA_Y"), output = "data.frame")
```

Metadata is saved in the attribute `metadata` and may be accessed via:
```r
df <- sophisthse("WAG_Y")
info <- attr(df, "metadata")
info
```

Built-in information about time series available for download maybe nicely viewed in Rstudio:
```r
View(series_info)
```
Or just `series_info` without Rstudio.

The list of available tables:
```r
sophisthse_tables()
```
