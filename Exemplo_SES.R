library(TSA)
library(forecast)
library(fpp3)

### carregando os dados
# Exports of goods and services from Brazil from 1960 to 2017.

View(br_economy)

br_economy <- global_economy %>%
  filter(Country == "Brazil")



br_economy %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Brasil")

br_economy

### Usando a função ses
SES = ses(br_economy$Exports, h = 3)
autoplot(SES)

### Usando a função ETS 
fit <- br_economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))

fit

fc <- forecast( fit, h = 3 )
fc


fc %>%
  autoplot(br_economy) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  labs(y="% of GDP", title="Exports: Brasil") +
  guides(colour = "none")

