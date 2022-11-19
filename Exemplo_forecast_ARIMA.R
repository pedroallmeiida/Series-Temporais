## Bibliotecas de séries temporais
library(TSA)
library(forecast)
library(fpp3)


serie <- global_economy %>%
  filter(Code == "CAF")

head(serie) 
tail(serie)


#### Comportamento da serie
plot_serie = serie %>%
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")
plot_serie

### ACF e PACF das series 
acf = serie %>%
  ACF(Exports, lag_max = 20) %>% 
  autoplot() + labs(title="ACF")
acf

pacf = serie %>%
  PACF(Exports, lag_max = 20) %>% 
  autoplot() + labs(title="PACF") 
pacf

gridExtra::grid.arrange(plot_serie, acf, pacf,layout_matrix=rbind(c(1,1),c(2,3) ))

#### Verificando se a série é estacionária 
serie %>%
  features(Exports, unitroot_kpss)

#### Aplicando o operador diferença 
plot_serieD = serie %>%
  autoplot( difference(Exports) ) +
  labs(title="Central African Republic exports",
       y="% of GDP")
plot_serieD

### ACF e PACF das series 
acf_D = serie %>%
  ACF(difference(Exports), lag_max = 20) %>% 
  autoplot() + labs(title="ACF")
acf_D

pacf_D = serie %>%
  PACF(difference(Exports), lag_max = 20) %>% 
  autoplot() + labs(title="PACF") 
pacf_D

gridExtra::grid.arrange(plot_serieD, acf_D, pacf_D,layout_matrix=rbind(c(1,1),c(2,3) ))

#### Forecast 

# Definindo a base de dados em treinamento e teste
train <- serie %>%
  filter_index("1981" ~ "2012")
tail(train)

test <- serie %>%
  filter_index("2013" ~ . );test

#### Comportamento da serie
plot_serie = train %>%
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")
plot_serie

### ACF e PACF das series 
acf = train %>%
  ACF(Exports, lag_max = 20) %>% 
  autoplot() + labs(title="ACF")
acf

pacf = train %>%
  PACF(Exports, lag_max = 20) %>% 
  autoplot() + labs(title="PACF") 
pacf

gridExtra::grid.arrange(plot_serie, acf, pacf,layout_matrix=rbind(c(1,1),c(2,3) ))

#### Verificando se a série é estacionária 
train %>%
  features(Exports, unitroot_kpss)

#### Aplicando o operador diferença 
plot_serieD = train %>%
  autoplot( difference(Exports) ) +
  labs(title="Central African Republic exports",
       y="% of GDP")
plot_serieD

### ACF e PACF das series 
acf_D = train %>%
  ACF(difference(Exports), lag_max = 20) %>% 
  autoplot() + labs(title="ACF")
acf_D

pacf_D = train %>%
  PACF(difference(Exports), lag_max = 20) %>% 
  autoplot() + labs(title="PACF") 
pacf_D

gridExtra::grid.arrange(plot_serieD, acf_D, pacf_D,layout_matrix=rbind(c(1,1),c(2,3) ))

#### Verificando se a série diferenciada é estacionária 

train %>%
  features(difference(Exports), unitroot_kpss)


#### FORECAST

caf_fit <- train %>%
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima012 = ARIMA(Exports ~ pdq(0,1,2)),
        arima212 = ARIMA(Exports ~ pdq(2,1,2)),
        rb = ARIMA(Exports ~ pdq(0,1,0)),
        auto = ARIMA(Exports ) )

caf_fit %>% select(auto)

## Desempenho dos modelos
glance(caf_fit)

#### Comportamento dos residuos
caf_fit %>%
  select(rb) %>%
  gg_tsresiduals()



# GERANDO UMA PREVISAO h = 2 passos 
caf_fc <- caf_fit %>% forecast(h = 5)
caf_fc

# Grafico da serie com as previsoes 
caf_fc %>% filter( (.model == 'rb') | (.model == 'arima212')  ) %>%
  autoplot(serie, level = NULL) +
  labs(
    y = "Exportações",
    title = "Previsão"
  ) +
  guides(colour = guide_legend(title = "Forecast"))



accuracy(caf_fc, test)









caf_fc




