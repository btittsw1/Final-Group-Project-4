library(fpp3)
library(tsibble)
library(seasonal)


#cleaning the data
credit <- read.csv('credit.csv')

credit$month <- 492:1

credit$month <- yearmonth(credit$month)

credit_ts <- tsibble(credit, index = month)

autoplot(credit_ts)
gg_season(credit_ts)

#Trying different models

fit <- credit_ts %>% 
        model(TSLM()) 

report(fit)

