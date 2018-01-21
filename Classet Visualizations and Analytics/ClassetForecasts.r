
library("AzureML")
ws <- workspace()
dataset <- download.datasets(ws, "ClassetForecasts.csv")

head(dataset)

month <- c(1,3,6,12,24)
EEM_future <- dataset$EEM
AGG_future <- dataset$AGG
HYG_future <- dataset$HYG
GSCI_future <- dataset$GSCI
SP_Future <- dataset$SP
EEM_future
month

barplot(EEM_future, main = "EEM Forecasts", xlab = "Month in Future", ylab = "Return on Investment (%) ", names.arg = c("1", "2", "6", "12", "24"), col = "green")

barplot(AGG_future, main = "AGG Forecasts", xlab = "Month in Future", ylab = "Return on Investment (%) ", names.arg = c("1", "2", "6", "12", "24"), col = "red")

barplot(SP_Future, main = "S&P 500 Forecasts", xlab = "Month in Future", ylab = "Return on Investment (%) ", names.arg = c("1", "2", "6", "12", "24"), col = "green")

barplot(HYG_future, main = "HYG Forecasts", xlab = "Month in Future", ylab = "Return on Investment (%) ", names.arg = c("1", "2", "6", "12", "24"), col = "green")

barplot(GSCI_future, main = "GSCI Forecasts", xlab = "Month in Future", ylab = "Return on Investment (%) ", names.arg = c("1", "2", "6", "12", "24"), col = "green")
