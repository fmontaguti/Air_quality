## Fernando Hold Montaguti

## PM2.5

library(knitr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

getwd()
setwd("C:/Users/fmontaguti/Documents/Air_Quality")

files = list.files()
#Cria uma lista com os documentos do diretório
temp = lapply(files, read.csv)
#Aplica uma função separado em cada elemento de uma lista
temp2 = do.call(rbind,temp)
#Aplica uma função comulativa em elementos de uma lista
Data = temp2[2:18]
#Seleciona apenas algumas colunas no dataset
Data = Data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15,17)]

Data %>%
    group_by(station) %>%
    summarise(
      PM2.5 = median(PM2.5, na.rm = TRUE),
      PM10 = median(PM10, na.rm = TRUE),
      SO2 = median(SO2, na.rm = TRUE),
      NO2 = median(NO2, na.rm = TRUE),
      CO = median(CO, na.rm = TRUE),
      O3 = median(O3, na.rm = TRUE)
    )

A = Data %>%
    group_by(year) %>%
    summarise(
        PM2.5 = median(PM2.5, na.rm = TRUE),
        PM10 = median(PM10, na.rm = TRUE),
        NO2 = median(NO2, na.rm = TRUE),
        O3 = median(O3, na.rm = TRUE)
      ) 

B = gather(A,Polution, Concentration, PM2.5:O3)

ggplot(B,aes(x=year,y=Concentration,group=Polution,color=Polution))+
  geom_line(size=0.8)+
  geom_point(size=2)+
  labs(title="Median Concentration by Year",x="Year",y="Concentration [ppm]")+
  theme(legend.position="top")+
  theme(plot.title = element_text(hjust = 0.5))


#Cria a os vetores resposta

Mean = lapply(Data[5:10], mean, na.rm = TRUE)
Min = lapply(Data[5:10], quantile, c(0) ,na.rm = TRUE)
Q1 = lapply(Data[5:10], quantile, c(0.25) ,na.rm = TRUE)
Q2 = lapply(Data[5:10], quantile, c(0.50) ,na.rm = TRUE)
Q3 = lapply(Data[5:10], quantile, c(0.75) ,na.rm = TRUE)
Max = lapply(Data[5:10], quantile, c(1) ,na.rm = TRUE)
SD = lapply(Data[5:10], sd, na.rm = TRUE)
VAR = lapply(Data[5:10], var, na.rm = TRUE)

#Unifica os vetores

Statistics = rbind(Min,Q1,Q2,Q3,Max,Mean,SD,VAR)
Statistics

COR=cor(Data[5:15],use = "complete.obs")
corrplot.mixed(COR,lower.col = "black", number.cex = .7)

Linear_model = lm(PM2.5~PM10+SO2+NO2+CO+WSPM,Data)
summary(Linear_model)

plot(Linear_model)

confint(Linear_model)



