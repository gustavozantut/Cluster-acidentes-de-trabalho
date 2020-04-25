install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

#importando dataset de acidentes de trabalho 
data = read.csv(file.choose(), encoding = "latin1", sep = ",", check.names = F)

data

#excluindo linha sem informação
data <-data[1:793,]

#transformando dados em numericos
data$`Assistência Médica` <-as.numeric(data$`Assistência Médica`)
data$`Incapacidade Temporária` <-as.numeric(data$`Incapacidade Temporária`)

#cria coluna com total de acidentes
data$total <-data$`Assistência Médica`+data$`Incapacidade Temporária`+data$`Incap Temporária menos 15 dias`+data$`Incap Temporária mais 15 dias`+data$`Incapacidade Permanente`+data$Óbito

#adicionando a informação de região  
i=1
data$Região <- NA
j=ncol(data) 

for(i in 1:nrow(data)){
  if(data[i,2] == "Amazonas"||data[i,2] == "Acre"||data[i,2] == "Rondônia"||data[i,2] == "Roraima"||data[i,2] == "Amapá"||data[i,2] == "Pará"||data[i,2] == "Tocantins"){
    data[i,j] <- "Norte"
  }   
  if(data[i,2] == "Maranhão"||data[i,2] == "Piauí"||data[i,2] == "Rio Grande do Norte"||data[i,2] == "Ceará"||data[i,2] == "Paraíba"||data[i,2] == "Bahia"||data[i,2] == "Pernambuco"||data[i,2] == "Alagoas"||data[i,2] == "Sergipe"){
    data[i,j] <- "Nordeste"
  }  
  if(data[i,2] == "Distrito Federal"||data[i,2] == "Goiás"||data[i,2] == "Mato Grosso do Sul"||data[i,2] == "Mato Grosso"){
    data[i,j] <- "Centro-Oeste"
  } 
  if(data[i,2] == "Minas Gerais"||data[i,2] == "Espírito Santo"||data[i,2] == "Rio de Janeiro"||data[i,2] == "São Paulo"){
    data[i,j] <- "Sudeste"
  }
  if(data[i,2] == "Santa Catarina"||data[i,2] == "Paraná"||data[i,2] == "Rio Grande do Sul"){
    data[i,j] <- "Sul"
  }  
  
}

#plotar total de acidentes por ano
ggplot(data) +
  geom_jitter(aes(x = Ano, y = total, color = data$`Unidade da Federação`)) +
  ggtitle("Total Acidentes por Ano")

df_Região <- data %>% group_by(Região,Ano) %>% summarise(total=mean(total))

#plotando MEDIA por regiao/ano
ggplot(df_Região) +
geom_jitter(aes(x = Ano, y = total, color = Região)) +
  ggtitle("MEDIA por Região/Ano")

df_Estado <- data %>% group_by(`Unidade da Federação`) %>% summarise(total=mean(total))

df_Estado

#plotando MEDIA por estado
ggplot(df_Estado) +
  geom_jitter(aes(x = `Unidade da Federação`, y = total, color = `Unidade da Federação`)) +
  ggtitle("MEDIA por Estado")


data2<- data_frame(data)

data2

#cria dataframe com totais por estado  
df1 <- data2 %>% group_by(`Unidade da Federação`, Região) %>% summarise(`Assistência Médica` = sum(`Assistência Médica`),
                                                                        `Incapacidade Temporária` = sum(`Incapacidade Temporária`),
                                                                        `Incap Temporária menos 15 dias` = as.double(sum(`Incap Temporária menos 15 dias`)),
                                                                        `Incap Temporária mais 15 dias` = as.double(sum(`Incap Temporária mais 15 dias`)),
                                                                        `Incapacidade Permanente` = as.double(sum(`Incapacidade Permanente`)),
                                                                        Total_Óbito = as.double(sum(Óbito)),
                                                                        `Acidentes Liquidados` = as.double(sum(`Acidentes Liquidados`)),
                                                                        Total = as.double(sum(total)))

df1
'''
%>% summarise(Total_Assist_Med = sum(data$`Assistência Médica`),
                                                             Total.incap.temp = sum(data$`Incapacidade Temporária`),
                                                             Total.incap.temp.menor.15 = sum(data$`Incap Temporária menos 15 dias`),
                                                             Total.incap.temp.maior.15 = sum(data$`Incap Temporária mais 15 dias`),
                                                             Total.incap.perm = sum(data$`Incapacidade Permanente`),
                                                             Total.obito = sum(data$Óbito),
                                                             Total.liquidado = sum(data$`Acidentes Liquidados`),
                                                             Total=sum(data$total))
'''
View(df1)

#exclui linhas com estado = ignorado
df1 <- df1[df1$`Unidade da Federação` != "Ignorado",]


#incluir população economicamente ativa em 2000
i=1
j=ncol(df1)+1
for(i in 1:nrow(df1)){
  if(df1[i,1] == "Amazonas"){
    df1[i,j] <- 2812557
  }
  if(df1[i,1] == "Acre"){
    df1[i,j] <- 557526
  }
  if(df1[i,1] == "Rondônia"){
    df1[i,j] <- 1379787 
  }
  if(df1[i,1] == "Roraima"){
    df1[i,j] <- 324397 
  }
  if(df1[i,1] == "Amapá"){
    df1[i,j] <- 477032
  }
  if(df1[i,1] == "Pará"){
    df1[i,j] <- 6192307 
  }
  if(df1[i,1] == "Tocantins"){
    df1[i,j] <- 1157098
  }
  if(df1[i,1] == "Maranhão"){
    df1[i,j] <- 5651475 
  }
  if(df1[i,1] == "Piauí"){
    df1[i,j] <- 2843278 
  }
  if(df1[i,1] == "Rio Grande do Norte"){
    df1[i,j] <- 2776782 
  }
  if(df1[i,1] == "Ceará"){
    df1[i,j] <- 7430661
  }
  if(df1[i,1] == "Paraíba"){
    df1[i,j] <- 3443825 
  }
  if(df1[i,1] == "Bahia"){
    df1[i,j] <- 13070250
  }
  if(df1[i,1] == "Pernambuco"){
    df1[i,j] <- 7918344
  }
  if(df1[i,1] == "Alagoas"){
    df1[i,j] <- 2822621
  }
  if(df1[i,1] == "Sergipe"){
    df1[i,j] <- 1784475 
  }
  if(df1[i,1] == "Distrito Federal"){
    df1[i,j] <- 2051146 
  }
  if(df1[i,1] == "Goiás"){
    df1[i,j] <- 5003228
  }
  if(df1[i,1] == "Mato Grosso do Sul"){
    df1[i,j] <- 2078001 
  }
  if(df1[i,1] == "Mato Grosso"){
    df1[i,j] <- 2504353 
  }
  if(df1[i,1] == "Minas Gerais"){
    df1[i,j] <- 17891494
  }
  if(df1[i,1] == "Espírito Santo"){
    df1[i,j] <- 3097232 
  }
  if(df1[i,1] == "Rio de Janeiro"){
    df1[i,j] <- 14391282 
  }
  if(df1[i,1] == "São Paulo"){
    df1[i,j] <- 37032403 
  }
  if(df1[i,1] == "Santa Catarina"){
    df1[i,j] <- 5356360
  }
  if(df1[i,1] == "Paraná"){
    df1[i,j] <- 9569458
  }
  if(df1[i,1] == "Rio Grande do Sul"){
    df1[i,j] <- 10187798 
  }
}
names(df1)[11] <- ("População(2020)")

View(df1)

#kmens para os totais
#podemos observar que fora o estado de sp os demais estão bem misturados por estarmos avaliando apenas os totais
km1 = kmeans(df1[,-c(1,2)],6, iter.max = 10000)
plot(df1[,-c(1,2)], col = (km1$cluster + 1) , main="K-Means com 6 clusters", pch=20, cex=2 )

#criando dataframe com média anual dos dados
df_med <- df1

df_med

i = 1
for(i in 1:nrow(df1)){
  if(df_med[i,1] == "Tocantins"){
    df_med[i,c(3:10)] <- df_med[i,c(3:10)]/24
  }
  else if(df_med[i,1] == "Amapá"){
    df_med[i,c(3:10)] <- df_med[i,c(3:10)]/24
  }
  else if(df_med[i,1] == "Roraima"){
    df_med[i,c(3:10)] <- df_med[i,c(3:10)]/24
  }
   else{
    df_med[i,c(3:10)] <- df_med[i,c(3:10)]/30
  }
}

df_med
#kmeans para o medias anuais
#podemos observar que fora o estado de sp os demais continuam bem misturados por estarmos avaliando apenas a media anual
km3 = kmeans(df_med[,-c(1,2)],6, iter.max = 10000)
plot(df_med[,-c(1,2)], col = (km3$cluster + 1) , main="K-Means com 6 clusters - Médias Anuais", pch=20, cex=2 )

#dataframe de media anual dividido pela populacao
df_pop <- df_med

df_pop[,c(3:10)] <- df_pop[,c(3:10)]/df_pop$`População(2020)`

#kmeans media anual/pop
#podemos osbservar que após dividir as médias anuais pela população, os pontos parecem criar agrupamentos mais bem definidos
km2 = kmeans(df_pop[,c(9, 11)],5, iter.max = 10000)
plot(df_pop[,-c(1,2)], col = (as.factor(df_pop$Região)) , main="K-Means com 5 clusters - Região", pch=20, cex=2 )
plot(df_pop[,-c(1,2)], col = (as.factor(km2$cluster)) , main="K-Means com 5 clusters", pch=20, cex=2 )

centro <- data.frame(km2$centers)
colnames(centro) <- colnames(km2$centers)


# PODEMOS OBSERVAR QUE A REGIÃO NORTE TEM UM PROPORCAO ALTA DE ASSISTENCIA MEDICA POR ACIDENTE LIQUIDADO
# JA A REGIÃO SUL E SUDOESTE POSSUEM UMA PROPORCAO BAIXÍSSIMA E PROPORÇÃO ALTA DE LIQUIDADOS

ggplot(df_pop,aes(x=df_pop$`Assistência Médica`, y=df_pop$`Acidentes Liquidados`, shape=df_pop$Região, col=as.factor(km2$cluster))) +
  geom_jitter(size=scale(df_pop$`População(2020)`)+3,alpha=.7)


#AQUI PODEMOS OBSERVAR QUE A REGIÃO CENTRO OESTE TEM UM TAXA ALTA DE OBITO EM RELACAO
#AO TOTAL DE ACIDENTES LIQUIDADOS 

ggplot(df_pop, aes(x = df_pop$Total_Óbito, y = df_pop$`Acidentes Liquidados`, shape=df_pop$Região, col = as.factor(km2$cluster))) +
  geom_jitter(size=scale(df_pop$`População(2020)`)+3,alpha=.7)

ggplot(df_pop, aes(x=df_pop$`População(2020)`, y=df_pop$`Acidentes Liquidados`, shape=df_pop$Região, col=as.factor(km2$cluster))) +
  geom_jitter(size=scale(df_pop$`População(2020)`)+3,alpha=.7) 


#testando scale-----------------

sdf <- as.data.frame(scale(df_pop[,-c(1, 2)]))
sdf[, 10] <- df_pop[, 1]
sdf[, 11] <- df_pop[, 2]

sdf

# VERIFICANDO KMEANS ACIDENTES X POPULAÇÃO
km5 = kmeans(sdf[,c(7,9)],5, iter.max = 10000)
plot(sdf[,-c(10, 11)], col = (as.factor(sdf$Região)), main="K-Means com 5 clusters - Região", pch=20, cex=2 )
plot(sdf[,-c(10, 11)], col = (as.factor(km5$cluster)) , main="K-Means com 5 clusters - Clusters", pch=20, cex=2 )

ggplot(sdf,aes(x = sdf$`População(2020)`, y = sdf$`Acidentes Liquidados`, shape = sdf$Região, col=as.factor(km5$cluster))) +
  geom_jitter(size = scale(sdf$`População(2020)`)+3,alpha=.7)

km5 = kmeans(sdf[,c(1,2,3,6)],5, iter.max = 10000)

ggplot(sdf, aes(x = sdf$`Incapacidade Temporária`, y = sdf$Total_Óbito, shape = sdf$Região, col=as.factor(km5$cluster))) +
  geom_jitter(size = scale(sdf$`População(2020)`)+3,alpha=.7)

ggplot(sdf, aes(x = sdf$`Incap Temporária menos 15 dias`, y = sdf$`Assistência Médica`, shape = sdf$Região, col=as.factor(km5$cluster))) +
  geom_jitter(size=scale(sdf$`População(2020)`)+3,alpha=.7)

#tirando sao paulo----------------------------------------------------------------
df_sp <- as.data.frame(df_pop[df_pop$`Unidade da Federação` != "São Paulo",])

df_sp

km6 = kmeans(df_sp[,c(3, 4, 5,8)],5, iter.max = 10000)
plot(df_sp[,-c(1, 2)], col = (as.factor(df_sp$Região)), main="K-Means com 5 clusters - Região", pch=20, cex=2 )
plot(df_sp[,-c(1, 2)], col = (as.factor(km6$cluster)) , main="K-Means com 5 clusters", pch=20, cex=2 )

ggplot(df_sp, aes(x = df_sp$`Assistência Médica`, y = df_sp$Total_Óbito, shape = df_sp$Região, col = as.factor(km6$cluster))) +
  geom_jitter(size=scale(df_sp$`População(2020)`)+3,alpha=.7)

