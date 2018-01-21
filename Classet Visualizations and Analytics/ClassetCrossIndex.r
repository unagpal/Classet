
library("AzureML")
ws <- workspace()
dataset <- download.datasets(ws, "ClassetCrossIndex.csv")

head(dataset)

index <- c("S&P", "HYG", "EEM", "GSC", "AGG")
EEM_cross <- dataset$EEM
AGG_cross <- dataset$AGG
HYG_cross <- dataset$HYG
GSCI_cross <- dataset$GSCI
SP_cross <- dataset$S.P

barplot(EEM_cross, main = "EEM: Cross Index Analysis", xlab = "Alternative Asset Class", ylab = "Correlation ", names.arg = index, col = "blue")
barplot(AGG_cross, main = "AGG: Cross Index Analysis", xlab = "Alternative Asset Class", ylab = "Correlation ", names.arg = index, col = "blue")
barplot(HYG_cross, main = "HYG: Cross Index Analysis", xlab = "Alternative Asset Class", ylab = "Correlation ", names.arg = index, col = "blue")
barplot(GSCI_cross, main = "GSCI: Cross Index Analysis", xlab = "Alternative Asset Class", ylab = "Correlation ", names.arg = index, col = "blue")
barplot(SP_cross, main = "S&P 500: Cross Index Analysis", xlab = "Alternative Asset Class", ylab = "Correlation ", names.arg = index, col = "blue")
