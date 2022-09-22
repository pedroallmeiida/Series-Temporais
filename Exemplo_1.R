library(TSA)
library(forecast)
library(astsa) ## os datasets do livro stoffer and shumway


tsplot(soi )

ST_decompose = decompose(soi)
names(ST_decompose)

### Grafico com as decomposicoes
plot(ST_decompose)

### Tendencia 
Tend = ST_decompose$trend
tsplot(Tend)

## Sazonalidade
Saz = ST_decompose$seasonal
tsplot(Saz)

### Autocorrelação
par(mfrow = c(1,1))
acf( soi )
pacf( soi )

x = rnorm(100)
acf(x)
pacf(x)

### Suavizacao 

library(zoo)
set.seed(1964)
x = runif( 10, 0, 10 )
M = rollmean(x, k = 2)
M


