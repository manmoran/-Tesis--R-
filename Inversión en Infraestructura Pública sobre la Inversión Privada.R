# Universidad de Guadalajara
# Impacto de la Inversión en Infraestructura Pública sobre la Inversión Privada
# Miguel Ángel Navarro Morán
# Date: 12/12/2020
  
library(xts)
library(tidyverse)
library(lubridate)
library(tseries)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)
library(nlme)
library(readxl)
library(openxlsx)
library(foreign)
library(urca)
library(ggplot2)
library(cointReg)
library(coin)
library(PerformanceAnalytics)
library(aTSA)
library(MTS)

# Seleccionando archivo XLSX
file.choose()
Data_series = read_excel("C:\\Users\\manav\\Desktop\\MAESTRÍA\\TESIS\\SERIES\\SERIES\\SERIES Impacto de la Inversión en Infraestructura Pública sobre la Inversión Privada.xlsx")
attach(Data_series)
View(Data_series)

# Especificando período de tiempo
Data_series.ts = ts(Data_series, start = 2006, frequency = 4)

# Especificando y analizando variable: Inversión Privada
Ipriv.ts = ts(Data_series.ts[,3], start = 2006, frequency = 4)
Ipriv.ts
plot(Ipriv.ts, 
     ylab = "Índice 2013=100", 
     xlab = "Período",
     lwd  = 3,
     main = "Formación bruta de capital fijo en México")
summary(Ipriv.ts)

# Especificando y analizando variable: Inversión en Infraestructura Pública
Iinf.ts = ts(Data_series.ts[,4], start = 2006, frequency = 4)
Iinf.ts
plot(Iinf.ts,
     ylab = "Índice 2013=100",
     xlab = "Período",
     lwd  = 3,
     main = "Inversión en Infraestructura Pública en México")
summary(Iinf.ts)

# Especificando y analizando variable: PIB
PIB.ts = ts(Data_series.ts[,5], start = 2006, frequency = 4)
PIB.ts
plot(PIB.ts,
     ylab = "Índice 2013=100",
     xlab = "Período",
     lwd  = 3,
     main = "PIB en México")
summary(PIB.ts)

# Especificando y analizando variable: Tasa de Interés Real
R.ts = ts(Data_series.ts[,6], start = 2006, frequency = 4)
R.ts
plot(R.ts, 
     ylab = "Tasa",
     xlab = "Período",
     lwd  = 3,
     main = "Tasa de interés real en México")
summary(R.ts) 

# Especificando y analizando variable: Deuda Pública Interna
dpbi.ts = ts(Data_series.ts[,7], start = 2006, frequency = 4)
dpbi.ts
plot(dpbi.ts, 
     ylab = "mmdp 2013=100", 
     xlab = "Período",
     lwd  = 3,
     main = "Deuda pública interna en México")
summary(dpbi.ts)

# Verificando Autocorrelación de: Ipriv , Iinf & R
acf(Ipriv.ts)
acf(Iinf.ts)
acf(R.ts)

# Verificando existencia de cambios estructurales en las series
# Ipriv
mod_ipriv = Fstats(Ipriv.ts ~ 1, from = .01)
sctest(mod_ipri)
strucchange:: breakpoints(Ipriv.ts ~ 1)
sc.ipriv = strucchange::breakpoints(Ipriv.ts ~ 1)
summary(sc.ipriv)
plot(Ipriv.ts)
lines(sc.ipriv)
# Se observa un cambio estructural en 2011(2)
# Iinf
mod_iinf = Fstats(Iinf.ts ~ 1, from = .01)
sctest(mod_iinf)
strucchange:: breakpoints(Iinf.ts ~ 1)
sc.iinf = strucchange::breakpoints(Iinf.ts ~ 1)
summary(sc.iinf)
plot(Iinf.ts)
lines(sc.iinf)
# Se observan cambios estructurales en 2009(3), 2012(4) & 2018(2)
# PIB
mod_PIB = Fstats(PIB.ts ~ 1, from = .01)
sctest(mod_PIB)
strucchange:: breakpoints(PIB.ts ~ 1)
sc.PIB = strucchange::breakpoints(PIB.ts ~ 1)
summary(sc.PIB)
plot(PIB.ts)
lines(sc.PIB)
# Se observan cambios estructurales en 2007(4), 2009(4), 2011(4), 2016(2), 2018(2)
# R
mod_R = Fstats(R.ts ~ 1, from = .01)
sctest(mod_R)
strucchange:: breakpoints(R.ts ~ 1)
sc.R = strucchange::breakpoints(R.ts ~ 1)
summary(sc.R)
plot(R.ts)
lines(sc.R)
# Se observan cambios estructurales en 2008(3), 2013(1), 2015(2), 2018(1)
# dpbi
mod_dpbi = Fstats(dpbi.ts ~ 1, from = .01)
sctest(mod_dpbi)
strucchange:: breakpoints(dpbi.ts ~ 1)
sc.dpbi = strucchange::breakpoints(dpbi.ts ~ 1)
summary(sc.dpbi)
plot(dpbi.ts)
lines(sc.dpbi)
# Se observan cambios estructurales en 2008 (4), 2010(4), 2012(4), 2014(4)

# Verificando estacionariedad de las variables (DFA) en sus niveles.
# Ho = La serie presenta raíz unitaria.
# NOTA: Se determinan los rezagos máximos por:
# (12(T/100)^(1/2))
# Nota: Para correr utilizar librería "tseries" y desactivar librería "aTSA"
adf.test(Ipriv.ts , alternative = "stationary", k = 9)
adf.test(Iinf.ts  , alternative = "stationary", k = 9)
adf.test(PIB.ts   , alternative = "stationary", k = 9)
adf.test(R.ts     , alternative = "stationary", k = 9)
adf.test(dpbi.ts  , alternative = "stationary", k = 9)
# Las series se reportan como no estacionarias en sus niveles según DFA.

# Verificando estacionariedad de las variables (PP) en sus niveles.
pp.lvl.Ipriv = ur.pp(Ipriv.ts, type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.lvl.Ipriv)
pp.lvl.Iinf  = ur.pp(Iinf.ts,  type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.lvl.Iinf)
pp.lvl.PIB   = ur.pp(PIB.ts,   type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.lvl.PIB)
pp.lvl.R     = ur.pp(R.ts,     type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.lvl.R)
pp.lvl.dpbi  = ur.pp(dpbi.ts,  type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.lvl.dpbi)
# Las series se reportan como no estacionarias en sus niveles según PP.

# Verificando las diferencias requeridas para cada variable.
ndiffs(Ipriv.ts)
ndiffs(Iinf.ts)
ndiffs(PIB.ts)
ndiffs(R.ts)
ndiffs(dpbi.ts)
# Se observa que se convierten en estacionarias en su 1era diferencia.

# Generando logaritmos de las variables.
# Se excluye a la tasa de interés por propiedades de logaritmos.
lnIpriv  = log(Ipriv.ts)
lnIinf   = log(Iinf.ts)
lnPIB    = log(PIB.ts)
lndpbi   = log(dpbi.ts)

# Generando las primeras diferencias de las series.
difflnIpriv  = diff(lnIpriv)
difflnIinf   = diff(lnIinf)
difflnPIB    = diff(lnPIB)
diffR        = diff(R.ts)
difflndpbi   = diff(lndpbi)

# Graficando series en sus primeras diferencias.
plot(difflnIpriv)
plot(difflnIinf)
plot(difflnPIB)
plot(diffR)
plot(difflndpbi)

# Verficando Raíces Unitarias (DFA) en primeras diferencias.
# Ho = La serie presenta raíz unitaria.
adf.Ipriv = ur.df(difflnIpriv, type = "trend", lags = 9, selectlags = "AIC")
summary(adf.Ipriv)
adf.Iinf  = ur.df(difflnIinf,  type = "trend", lags = 9, selectlags = "AIC")
summary(adf.Iinf)
adf.PIB   = ur.df(difflnPIB,   type = "trend", lags = 9, selectlags = "AIC")
summary(adf.PIB)
adf.R     = ur.df(diffR,       type = "trend", lags = 9, selectlags = "AIC")
summary(adf.R)
adf.dpbi  = ur.df(difflndpbi,  type = "trend", lags = 9, selectlags = "AIC")
summary(adf.dpbi)
# Únicamente se reporta a R como estacionaria en su 1era diferencia según DFA.

# Verificando Raíces Unitarias (PP) en primeras diferencias
# Ho = La serie presenta raíz unitaria
pp.Ipriv = ur.pp(difflnIpriv, type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.Ipriv)
pp.Iinf  = ur.pp(difflnIinf,  type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.Iinf)
pp.PIB   = ur.pp(difflnPIB,   type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.PIB)
pp.R     = ur.pp(diffR,       type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.R)
pp.dpbi  = ur.pp(difflndpbi,  type = "Z-tau", model = "trend", use.lag = 9)
summary(pp.dpbi)
# Se reportan a todas las series como estacionarias al 1% en 1eras diferencias 

# Verificando Raíces Unitarias (KPSS) en primeras diferencias
# Ho = No Raíz Unitaria, la serie es estacionaria.
kpss.Ipriv = ur.kpss(difflnIpriv, type = "tau", use.lag = 9)
summary(kpss.Ipriv)
kpss.Iinf  = ur.kpss(difflnIinf,  type = "tau", use.lag = 9)
summary(kpss.Iinf)
kpss.PIB   = ur.kpss(difflnPIB,   type = "tau", use.lag = 9)
summary(kpss.PIB)
kpss.R     = ur.kpss(diffR,       type = "tau", use.lag = 9)
summary(kpss.R)
kpss.dpbi  = ur.kpss(difflndpbi,  type = "tau", use.lag = 9)
summary(kpss.dpbi)
# Todas las series se reportan estacionarias al 5% 
# A excepción de Ipriv, todas las series se reportan estacionarias al 1% en 1eras dif.

# Verificando Raíces Unitarias (DF-GLS) en primeras diferencias
dfgls.Ipriv = ur.ers(difflnIpriv, type = "DF-GLS", model = "trend", lag.max = 9)
summary(dfgls.Ipriv)
dfgls.Iinf  = ur.ers(difflnIinf,  type = "DF-GLS", model = "trend", lag.max = 9)
summary(dfgls.Iinf)
dfgls.PIB   = ur.ers(difflnPIB,   type = "DF-GLS", model = "trend", lag.max = 9)
summary(dfgls.PIB)
dfgls.R     = ur.ers(diffR,       type = "DF-GLS", model = "trend", lag.max = 9)
summary(dfgls.R)
dfgls.dpbi  = ur.ers(difflndpbi,  type = "DF-GLS", model = "trend", lag.max = 9)
summary(dfgls.dpbi)
# Se reportan todas las series como no estacionarias en sus primeras diferencias

# Verificando Raíces Unitarias (Zivot & Andrews Test) en primeras diferencias
z.Ipriv = ur.za(difflnIpriv, model = "trend")
summary(z.Ipriv)
z.Iinf  = ur.za(difflnIinf,  model = "trend")
summary(z.Iinf)
z.PIB   = ur.za(difflnPIB,   model = "trend")
summary(z.PIB)
z.R     = ur.za(diffR,       model = "trend")
summary(z.R)
z.dpbi  = ur.za(difflndpbi,  model = "trend")
summary(z.dpbi)
# Todas las series son estacionarias en sus 1eras diferencias al 1% 

#                 De los tests anteriores comprobamos que:
#   Todas las series son I(1), es decir, estacionarias en 1era diferencia.

#     A continuación, se procede a realizar la estimación del modelo econométrico
# Se procede con el modelo 1 hasta llegar al modelo 4, agregando una variable a c/modelo

################################################################################

#    MODELO 1 : lnIpriv = Bo + B1lnIinf + u             

# Analizando visualmente relación en las series 
plot(lnIpriv, 
     ylab = "En logaritmos", 
     xlab = "Período",
     lwd  = 3,
     lty  = 1,
     ylim = c(3.7, 5.0),
     main = "Inversión Privada e Inversión en Obra Pública")
lines(lnIinf, col = "blue", lwd = 3, lty = 2)
legend("bottomleft", c("lnIpriv","lnIinf"), lwd = 1.5, lty = 1:2)

# Estimación del modelo en el corto plazo
# Muestra del modelo para corto plazo
Muestra_1 = ts.intersect(lnIpriv, lnIinf)
View(Muestra_1)

# Generando Modelo de corto plazo
Mod_1.cp = lm(lnIpriv ~ lnIinf, Muestra_1)
summary(Mod_1.cp)
# Observamos que en el corto plazo:
# La Iinf responde positivamente significativa al 10%

# Generando residuales de Modelo
res.Mod_1.cp = Mod_1.cp$residuals 
res.Mod_1.cp = ts(res.Mod_1.cp, start = 2006, frequency = 4) 

# Analizando normalidad de los residuos
jarque.bera.test(res.Mod_1.cp)
shapiro.test(res.Mod_1.cp)

# Identificando residuos atípicos
boxplot1 = boxplot(res.Mod_1)
names(boxplot1)
boxplot1$out
View(res.Mod_1.cp)

# Cointegración de Engle y Granger
# Generando Modelo de cointegración
Mod_1 = lm(lnIpriv ~ lnIinf)
summary(Mod_1)

# Generando residuales de Modelo 
res.Mod_1 = residuals.lm(Mod_1)            
res.Mod_1 = ts(res.Mod_1,start = 2006, frequency = 4)           

# Analizando normalidad de los residuos
jarque.bera.test(res.Mod_1)
shapiro.test(res.Mod_1)
# Los residuos tienen distribución normal según JB
# Los residuos no tienen distribución normal según Shapiro

#Prueba ADF para cointegración Engle y Granger
coint.Mod_1 = ur.df(res.Mod_1, type = "none", lags = 9, selectlags = "AIC")
summary(coint.Mod_1)
# Los residuos no son estacionarios al 5%

# Generando vector de variables explicativas
x1 = ts.intersect(lnIinf)

# Análisis de cointegración
coint.test(lnIpriv, x1, d = 0)
# Dado que p.value >= 0.10 
# No existe relación de largo plazo (No existe cointegración)

# Cointegración Phillips-Ouiliaris
# Generando vector de todas las variables del modelo
z1 = ts.intersect(lnIpriv, lnIinf)
test.z1 = ca.po(z1, demean = "trend", lag = "long", type = "Pu", tol = NULL)
summary(test.z1)
# Dado que t < valores críticos
# No existe relación de largo plazo (No existe cointegración)
# Así mismo, los niveles de significancia nos denotan una regresión espuria

# Definir rezagos óptimos
VARselect(z1, lag.max = 10, type = "const")
VARselect(z1, lag.max = 10, type = "const")$selection

# Cointegración de Johansen
Joh.1 = ca.jo(z1, K = 5, type = "trace", ecdet = "const", spec = "transitory")
summary(Joh.1)
# No existe relación de largo plazo (No existe cointegración)
