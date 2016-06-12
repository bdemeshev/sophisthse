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

Список рядов (не совсем точный пока что) можно получить командой:
```r
sophisthse_tables()
```

Не очень срочные планы по развитию:

- [ ] команда для получения списка всех рядов --- скорректировать (туда сразу качать описание переменной)
- [ ] ряды пересматриваются. Добавить дату скачивания? Вести архив обновлений?
- [ ] в коде разделить: название таблицы с рядами и название ряда (нужно!) при этом базовая команда sophisthse должна кушать и названия таблиц и названия рядов и возвращать то, что хочет пользователь
- [ ] у некоторых рядов в названии есть русская М вместо английской M. Переделать такой трэшак в английские буквы :)
- [ ] может наброситься на разномастные таблицы? Данные опросов можно скачивать целиком без всяких пакетов после регистрации.
- [x] добавить labels для смысла переменных в data.frame, чтобы красиво отображалось в Rstudio (а-ля haven)
- [x] тип `ts` в output для корретного отображения дат при использовании пакета `forecast`.
- [x] убрать warning при скачивании рядов с пропущенными данными. Всё работает корректно, но для благозвучия лучше убрать. Возникает при вызове rus2num, например, `sophisthse("HHI_Q_I")`


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

Approximate list of time series can be obtained by:
```r
sophisthse_tables()
```
