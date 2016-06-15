[![Travis-CI Build Status](https://travis-ci.org/bdemeshev/sophisthse.svg?branch=master)](https://travis-ci.org/bdemeshev/sophisthse)

sophisthse
==========


Пакет `sophisthse` предназначен для скачивания временных рядов с [sophist.hse.ru](http://sophist.hse.ru/). Пакет можно установить командами:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/sophisthse")
```
Для новичков в R: Пакеты устанавливаются один раз, и каждый раз для скачанивания рядов выполнять эти команды совершенно ненужно :)


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




Не очень срочные планы по развитию:


- [ ] ряды пересматриваются. Добавить дату скачивания? Вести архив обновлений?
- [x] команда для получения списка всех рядов --- скорректировать (туда сразу качать описание переменной)
- [x] добавить labels для смысла переменных в data.frame, чтобы красиво отображалось в Rstudio (а-ля haven)
- [x] тип `ts` в output для корретного отображения дат при использовании пакета `forecast`.
- [x] убрать warning при скачивании рядов с пропущенными данными. Всё работает корректно, но для благозвучия лучше убрать. Возникает при вызове rus2num, например, `sophisthse("HHI_Q_I")`
- [x] у некоторых рядов в названии есть русская М вместо английской M. Переделать такой трэшак в английские буквы :)
- [x] в коде разделить: название таблицы с рядами и название ряда (нужно!) при этом базовая команда sophisthse должна кушать и названия таблиц и названия рядов и возвращать то, что хочет пользователь. Нужна просто функция `series2tables` :)

Разработчику:

- При проверке пакет командой `check()` появляется заметка (note) о строках помеченных UTF-8. Это ок. Единицы измерения, методология, источник и прочее содержат русские буквы.


### English translation:


R package to download data from [sophist.hse.ru](http://sophist.hse.ru/). The package `sophisthse` may be installed via:
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
