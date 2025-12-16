## Bibliotecas de séries temporais
library(TSA)
library(forecast)
library(fpp3)

### Carregando banco de dados em formato csv
tute1 <- readr::read_csv("https://raw.githubusercontent.com/pedroallmeiida/Series-Temporais/refs/heads/main/Exemplos/tute1.csv")
View(tute1)

### transformando parao formato tstible
serie <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)
serie

tail(serie)


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
  filter_index("1981 mar" ~ "2004 dec")
tail(train)

test <- serie %>%
  filter_index("2005 mar" ~ .);test

### verificando se esta correto
head(test)
tail(test)


# Estimando o modelo de suavizacao
sales_fit <- train %>%
  model(
    # SES (Suavização Exponencial Simples)
    SES = ETS(Sales ~ error("A") + trend("N") + season("N")),
    
    # Holt aditivo
    HoltA = ETS(Sales ~ error("A") + trend("A") + season("N")),
    
    # Holt multiplicativo
    HoltM = ETS(Sales ~ error("M") + trend("A") + season("N")),
    
    # Holt-Winters aditivo
    HW_Add = ETS(Sales ~ error("A") + trend("A") + season("A")),
    
    # Holt-Winters multiplicativo
    HW_Mult = ETS(Sales ~ error("M") + trend("A") + season("M"))
  )
sales_fit



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
    y = "Quantidade de vendas",
    title = "Previsão de vendas trimestrais para vendas em 2005"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


### Medidas de avaliacao 
accuracy(sales_fc, test)


### analise de residuos 
sales_fit %>% gg_tsresiduals()



#### DEPOIS QUE A ESCOLHA DO MELHOR MODELO FOR FEITA FAÇA A PREVISAO PARA VALORES FUTUROS 
## NEsse exemplo dado que o melhor modelo foi o ETS5, vamos usar ele para fazer as previsões

##

# Estimando o modelo de suavizacao
sales_fit <- serie %>%
  model(
    ETS5 = ETS( Sales ~ error("A") + trend( "N" ) + season("M")) 
  )
sales_fit



# GERANDO UMA PREVISAO h = 4 passos 
sales_fc <- sales_fit %>% forecast(h = 4)
sales_fc

# Grafico da serie com as previsoes 
sales_fc %>%
  autoplot(serie, level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "Quantidade de vendas",
    title = "Previsão de vendas trimestrais para vendas em 2006"
  ) +
  guides(colour = guide_legend(title = "Forecast"))




