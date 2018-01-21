
library("AzureML")
ws <- workspace()
dataset <- download.datasets(ws, "ClassetDataTable.csv")

head(dataset)
EEM_long_term_indexvalue <- dataset$EEM
AGG_long_term_indexvalue <- dataset$AGG
HYG_long_term_indexvalue <- dataset$HYG
GSCI_long_term_indexvalue <- dataset$GSCI
SP_long_term_indexvalue <- dataset$S.P.500
long_term_indexdate <- dataset$Date

EEM_fit_long = lm(EEM_long_term_indexvalue~long_term_indexdate)
coef(EEM_fit_long)
summary(EEM_fit_long)

AGG_fit_long = lm(AGG_long_term_indexvalue~long_term_indexdate)
coef(AGG_fit_long)
summary(AGG_fit_long)

HYG_fit_long = lm(HYG_long_term_indexvalue~long_term_indexdate)
coef(HYG_fit_long)
summary(HYG_fit_long)

SP_fit_long = lm(SP_long_term_indexvalue~long_term_indexdate)
coef(SP_fit_long)
summary(SP_fit_long)

GSCI_fit_long = lm(GSCI_long_term_indexvalue~long_term_indexdate)
coef(GSCI_fit_long)
summary(GSCI_fit_long)

EEM_short_term_indexvalue <- EEM_long_term_indexvalue[c(1:30)]
AGG_short_term_indexvalue <- AGG_long_term_indexvalue[c(1:30)]
HYG_short_term_indexvalue <- HYG_long_term_indexvalue[c(1:30)]
GSCI_short_term_indexvalue <- GSCI_long_term_indexvalue[c(1:30)]
SP_short_term_indexvalue <- SP_long_term_indexvalue[c(1:30)]
short_term_indexdate <- dataset$Date[c(1:30)]

EEM_fit_short = lm(EEM_short_term_indexvalue~short_term_indexdate)
coef(EEM_fit_short)
summary(EEM_fit_short)

AGG_fit_short = lm(AGG_short_term_indexvalue~short_term_indexdate)
coef(AGG_fit_short)
summary(AGG_fit_short)

HYG_fit_short = lm(HYG_short_term_indexvalue~short_term_indexdate)
coef(HYG_fit_short)
summary(HYG_fit_short)

GSCI_fit_short = lm(GSCI_short_term_indexvalue~short_term_indexdate)
coef(GSCI_fit_short)
summary(GSCI_fit_short)

SP_fit_short = lm(SP_short_term_indexvalue~short_term_indexdate)
coef(SP_fit_short)
summary(SP_fit_short)

plot(type = "l", x = short_term_indexdate, y = SP_short_term_indexvalue, main = "S&P 500: Short Term Analysis", ylab = "S&P 500 Value", xlab = "S&P Date", col = "red")

plot(type = "l", x = long_term_indexdate, y = SP_long_term_indexvalue, main = "S&P 500: Long Term Analysis", ylab = "S&P 500 Value", xlab = "S&P Date", col = "red")

plot(type = "l", x = short_term_indexdate, y = GSCI_short_term_indexvalue, main = "GSCI: Short Term Analysis", ylab = "GSCI Value", xlab = "GSCI Date", col = "red")
plot(type = "l", x = long_term_indexdate, y = GSCI_long_term_indexvalue, main = "GSCI: Long Term Analysis", ylab = "GSCI Value", xlab = "GSCI Date", col = "red")

plot(type = "l", x = short_term_indexdate, y = HYG_short_term_indexvalue, main = "HYG: Short Term Analysis", ylab = "HYG Value", xlab = "HYG Date", col = "red")
plot(type = "l", x = long_term_indexdate, y = HYG_long_term_indexvalue, main = "HYG: Long Term Analysis", ylab = "HYG Value", xlab = "HYG Date", col = "red")

plot(type = "l", x = short_term_indexdate, y = AGG_short_term_indexvalue, main = "AGG: Short Term Analysis", ylab = "AGG Value", xlab = "AGG Date", col = "red")
plot(type = "l", x = long_term_indexdate, y = AGG_long_term_indexvalue, main = "AGG: Long Term Analysis", ylab = "AGG Value", xlab = "AGG Date", col = "red")

plot(type = "l", x = short_term_indexdate, y = EEM_short_term_indexvalue, main = "EEM: Short Term Analysis", ylab = "EEM Value", xlab = "EEM Date", col = "red")
plot(type = "l", x = long_term_indexdate, y = EEM_long_term_indexvalue, main = "EEM: Long Term Analysis", ylab = "EEM Value", xlab = "EEM Date", col = "red")

print("HYG - EEM")
HYG_EEM_coeff_long = lm(HYG_long_term_indexvalue~EEM_long_term_indexvalue)
coef(HYG_EEM_coeff_long)
summary(HYG_EEM_coeff_long)

print("EEM - HYG")
EEM_HYG_coeff_long = lm(EEM_long_term_indexvalue~HYG_long_term_indexvalue)
coef(EEM_HYG_coeff_long)
summary(EEM_HYG_coeff_long)

print("HYG - AGG")
HYG_AGG_coeff_long = lm(HYG_long_term_indexvalue~AGG_long_term_indexvalue)
coef(HYG_AGG_coeff_long)
summary(HYG_AGG_coeff_long)

print("AGG - HYG")
AGG_HYG_coeff_long = lm(AGG_long_term_indexvalue~HYG_long_term_indexvalue)
coef(AGG_HYG_coeff_long)
summary(AGG_HYG_coeff_long)

print("HYG - SP")
HYG_SP_coeff_long = lm(HYG_long_term_indexvalue~SP_long_term_indexvalue)
coef(HYG_SP_coeff_long)
summary(HYG_SP_coeff_long)

print("SP - HYG")
SP_HYG_coeff_long = lm(SP_long_term_indexvalue~HYG_long_term_indexvalue)
coef(SP_HYG_coeff_long)
summary(SP_HYG_coeff_long)

print("HYG - GSCI")
HYG_GSCI_coeff_long = lm(HYG_long_term_indexvalue~GSCI_long_term_indexvalue)
coef(HYG_GSCI_coeff_long)
summary(HYG_GSCI_coeff_long)

print("GSCI - HYG")
GSCI_HYG_coeff_long = lm(GSCI_long_term_indexvalue~HYG_long_term_indexvalue)
coef(GSCI_HYG_coeff_long)
summary(GSCI_HYG_coeff_long)

print("SP - GSCI")
SP_GSCI_coeff_long = lm(SP_long_term_indexvalue~GSCI_long_term_indexvalue)
coef(SP_GSCI_coeff_long)
summary(SP_GSCI_coeff_long)

print("GSCI - SP")
GSCI_SP_coeff_long = lm(GSCI_long_term_indexvalue~SP_long_term_indexvalue)
coef(GSCI_SP_coeff_long)
summary(GSCI_SP_coeff_long)

print("SP - AGG")
SP_AGG_coeff_long = lm(SP_long_term_indexvalue~AGG_long_term_indexvalue)
coef(SP_AGG_coeff_long)
summary(SP_AGG_coeff_long)

print("AGG - SP")
AGG_SP_coeff_long = lm(AGG_long_term_indexvalue~SP_long_term_indexvalue)
coef(AGG_SP_coeff_long)
summary(AGG_SP_coeff_long)

print("SP - EEM")
SP_EEM_coeff_long = lm(SP_long_term_indexvalue~EEM_long_term_indexvalue)
coef(SP_EEM_coeff_long)
summary(SP_EEM_coeff_long)

print("EEM - SP")
EEM_SP_coeff_long = lm(EEM_long_term_indexvalue~SP_long_term_indexvalue)
coef(EEM_SP_coeff_long)
summary(EEM_SP_coeff_long)

print("EEM - GSCI")
EEM_GSCI_coeff_long = lm(EEM_long_term_indexvalue~GSCI_long_term_indexvalue)
coef(EEM_GSCI_coeff_long)
summary(EEM_GSCI_coeff_long)

print("GSCI - EEM")
GSCI_EEM_coeff_long = lm(GSCI_long_term_indexvalue~EEM_long_term_indexvalue)
coef(GSCI_EEM_coeff_long)
summary(GSCI_EEM_coeff_long)

print("EEM - AGG")
EEM_AGG_coeff_long = lm(EEM_long_term_indexvalue~AGG_long_term_indexvalue)
coef(EEM_AGG_coeff_long)
summary(EEM_AGG_coeff_long)

print("AGG - EEM")
AGG_EEM_coeff_long = lm(AGG_long_term_indexvalue~EEM_long_term_indexvalue)
coef(AGG_EEM_coeff_long)
summary(AGG_EEM_coeff_long)

print("GSCI - AGG")
GSCI_AGG_coeff_long = lm(GSCI_long_term_indexvalue~AGG_long_term_indexvalue)
coef(GSCI_AGG_coeff_long)
summary(GSCI_AGG_coeff_long)

print("AGG - GSCI")
AGG_GSCI_coeff_long = lm(AGG_long_term_indexvalue~GSCI_long_term_indexvalue)
coef(AGG_GSCI_coeff_long)
summary(AGG_GSCI_coeff_long)

