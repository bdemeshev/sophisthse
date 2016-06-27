## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("bdemeshev/sophisthse")

## ---- warning=FALSE, message=FALSE---------------------------------------
library("sophisthse") # для скачивания данных
library("forecast") # графики временных рядов, ARIMA/ETS модели
df <- sophisthse("WAG_Y")
head(df)

