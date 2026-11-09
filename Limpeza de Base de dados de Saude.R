#Instalacao de pacotes(Bibliotecas)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")

##Carregar os pacotes instalados (Bibliotecas)

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

#Carregar Ficheiro

doencas_df<-read.csv("C:/Users/Gaspar Bendzane/Downloads/doencas.csv")
populacao_df<-read.csv("C:/Users/Gaspar Bendzane/Downloads/populacao.csv")

#Visualizacao de Dados
head(doencas_df)
head(populacao_df)

##Limpeza de dados

##Verificacao de duplicados

sum(duplicated(doencas_df))
sum(duplicated(populacao_df))

##Verificar as linhas duplicadas

doencas_df[duplicated(doencas_df),]
populacao_df[duplicated(populacao_df),]

##Verificar todas as ocorrencias(incluindo a primeira)  

doencas_df[duplicated(doencas_df)|duplicated(doencas_df,fromLast = TRUE),]
populacao_df[duplicated(populacao_df)|duplicated(populacao_df,fromLast = TRUE),]

#Remocao de duplicados

doencas_df<-distinct(doencas_df)
populacao_df<-distinct(populacao_df)

##Confirmar Resultado
sum(duplicated(doencas_df));nrow(doencas_df)
sum(duplicated(populacao_df));nrow(populacao_df)

##Verificacao de dados Omissos
colSums(is.na(doencas_df))
colSums(is.na(populacao_df))

##Remocao de linhas com dados omisso

doencas_df<-drop_na(doencas_df,idade,dias_internamento)

##Verificar os NA

doencas_df %>% filter(is.na(idade)) %>% count(provincia)


##Calculo de media

doencas_df%>%summarise(across(where(is.numeric),~mean(.x,na.rm=TRUE)))
populacao_df%>%summarise(across(where(is.numeric),~mean(.x,na.rm=TRUE)))

##Calculo da mediana
doencas_df%>%group_by(doenca)%>%summarise(across(where(is.numeric),~median(.x,na.rm=TRUE)))
populacao_df%>%group_by(provincia)%>%summarise(across(where(is.numeric),~median(.x,na.rm=TRUE)))

##Calcule de minimo

doencas_df%>%group_by(doenca)%>%summarise(across(where(is.numeric),~min(.x,na.rm=TRUE)))
populacao_df%>%group_by(provincia)%>%summarise(across(where(is.numeric),~min(.x,na.rm=TRUE)))


##Calculo de maximo
doencas_df%>%group_by(doenca)%>%summarise(across(where(is.numeric),~max(.x,na.rm=TRUE)))
populacao_df%>%group_by(provincia)%>%summarise(across(where(is.numeric),~max(.x,na.rm=TRUE)))


##Calculo de soma

doencas_df%>%group_by(doenca)%>%summarise(across(where(is.numeric),~sum(.x,na.rm=TRUE)))
populacao_df%>%group_by(provincia)%>%summarise(across(where(is.numeric),~sum(.x,na.rm=TRUE)))
 
#Guardar os resumos em objectos

resumo_doencas<-doencas_df%>%group_by(doenca)%>%summarize(across(where(is.numeric),list(media=~mean(.x,na.rm=TRUE),mediana=~median(.x,na.rm = TRUE),max=~max(.x,na.rm=TRUE),soma=~sum(.x,na.rm = TRUE))))
View(resumo_doencas)


##Instalacao de bibliotecas ou pacotes

install.packages("skimr")

library(skimr)

##Grafico de histograma

ggplot(doencas_df,aes(x=idade))+geom_histogram(bins=20,fill="steelblue",colour = "white")

##Grafico de barras

ggplot(doencas_df,aes(x=provincia))+geom_bar(fill="steelblue")+labs(title = "Casos por provincia",x="Privincia",y="Numero de casos")





