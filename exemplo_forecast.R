## Bibliotecas de séries temporais
library(TSA)
library(forecast)
library(fpp3)

### Carregando banco de dados em formato csv
tute1 <- readr::read_csv("C:/Users/pedro/Dropbox/disciplinas_UEPB/Series_Temporais/tute1.csv")
View(tute1)

### transformando parao formato tstible
serie <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)
serie

### Plot das series
serie %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

### ESTUDO PARA A VARIAVEL SALES 
 
### ACF e PACF das series 
acf_sales = serie %>% 
  ACF(Sales, lag_max = 20) %>% 
  autoplot() + labs(title="ACF da variável Sales")

pacf_sales = serie %>% 
  PACF(Sales, lag_max = 20) %>% 
  autoplot() + labs(title="PACF da variável Sales")

gridExtra::grid.arrange(acf_sales, pacf_sales, ncol=2)


### Estudando a sazonalidade

serie %>%
  gg_season(Sales, labels = "both") +
  labs(y = "Quantidade de vendas",
       title = "Sazonalidade: Sales")

### subgraficos 

serie %>%
  gg_subseries(Sales) +
  labs(
    y = "Quantidade de vendas",
    title = "Sazonalidade: Sales"
  )

### Extraindo os componentes de uma serie temporal 

### Decomposição aditiva
serie %>%
  model(
    classical_decomposition(Sales, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição classica aditiva do total de vendas")

### Decomposição multiplicativa
serie %>%
  model(
    classical_decomposition(Sales, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição classica aditiva do total de vendas")


#### Forecast ---- 

## para identificar os anos iniciais e finais
head(serie)
tail(serie)

# Definindo a base de dados em treinamento e teste
train <- serie %>%
  filter_index("1981 Q1" ~ "2005 Q1")
test <- serie %>%
  filter_index("2005 Q1" ~ .);test

### verificando se esta correto
head(train)
tail(train)

# Estimando o modelo de suavizacao
sales_fit <- train %>%
  model(
    ETS = ETS(Sales)
  )

# GERANDO UMA PREVISAO h = 4 passos 
sales_fc <- sales_fit %>% forecast(h = 4)
sales_fc

# Grafico da serie com as previsoes 
sales_fc %>%
  autoplot(train, level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


### analise de residuos 
sales_fit %>% gg_tsresiduals()



### MEdidas de avaliacao 
accuracy(sales_fc, test)



