# Marcus Vinicius Almeida Florencio 12021BCC016
# Cauã Pereira Neres 12021BCC005


#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 1 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================

dados <- read.table(file = "diabetes.txt", header = TRUE, sep = ";")  
dados


set.seed(016005)

dados <- dados[sample(nrow(dados)),]

dados$Diabetic <- as.factor(dados$Diabetic)
dados

summary(dados)

n <- round(nrow(dados)*0.8) #determinando 80% do conjunto para separar o treinamento do modelo

treinamento <- dados[1:n,] # separando o treinamento
teste <- dados[-(1:n),] # separando o teste


#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< 1) Letra A >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================


#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< 1) Letra B >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================

library(rpart)
library(rpart.plot)

arvore.dados <- rpart(data = treinamento, formula = Diabetic ~ ., method = "class")
rpart.plot(arvore.dados)

rpart.plot(arvore.dados, extra = 101)

previsao <- predict(arvore.dados, newdata = teste, type = "class")
previsao
mean(previsao == teste$Diabetic)


library(ggplot2)
ggplot(data = treinamento, aes(x = Pregnancies, fill = Diabetic))+
  geom_bar()


isDiabetic <- function(valor){
  if(valor$Pregnancies <2){
    return(0)
  }else if(valor$BMI < 22){
    return(0)
  }else if(valor$SerumInsulin < 52){
    if(valor$Age < 38){
      return(0)
    }else{
      return(1)
    }
  }else if(valor$Age >= 38){
    return(1)
  }else{
    if(valor$PlasmaGlucose >= 98){
      return(1)
    } else{
      if(valor$BMI >= 38){
        return(1)
      }else{
        return(0)
      }
    }
  }
  
}

isDiabetic(treinamento[1,])


previsao <- c()
for (i in 1:3000) {
  previsao[i] <- isDiabetic(teste[i,])
}
previsao

mean(previsao == teste$Diabetic)
# [1] 0.878 de precisao

#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< 1) Letra C >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================

library(randomForest)

treinamentoDados_padronizados <- scale(treinamento)

floresta <- randomForest(formula = Diabetic ~., data = treinamento, importance = TRUE)
floresta


?ramdomForest()

previsao.floresta <- predict(floresta, newdata = teste, type = "class")


previsao.floresta
mean(previsao.floresta == teste$Diabetic)
# [1] 0.9366667 de precisao

#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< 1) Letra D >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================

table(previsao, teste$Diabetic)

757 / (222 + 757)
# 77% de precisao dado q ele tem diabete


table(previsao.floresta, teste$Diabetic)
878 / (101 + 878)
# 89% de precisao dado q ele tem diabete

#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< 1) Letra E >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================
# modelo 2 se saiu melhor, pq o 2 além de ter uma precisão maior, ele tbm se saiu melhor
# no erro do tipo 2.



#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================
#Exercício 2

library(rvest)
library(dplyr)
library(ggplot2)
library(factoextra)

dados <- read.table("semente.txt", sep = ",", header = TRUE)
str(dados)

?gsub

dados$V1 <- gsub("mm2", "", dados$V1)
print(dados)


#agrupamento <- dados[, c("V1","V2","V3","V4","V5","V6","V7")]
agrupamento <- scale(dados[,-1])
print(agrupamento)

matriz_distancias <- dist(agrupamento[, -1])
modelo_agrupamento <- hclust(matriz_distancias, method = "complete")

plot(modelo_agrupamento)
#plot(modelo_agrupamento, hang = -1, main = "Medições das propriedades geométricas de sementes",
# xlab = "Sementes", ylab = "Distância", sub = NULL)
fviz_dend(modelo_agrupamento, k = 3)

aglomerados <- cutree(modelo_agrupamento, k = 3)

modelo_kmeans <- kmeans(agrupamento[, -1], centers = 3)
table(modelo_kmeans$cluster)

table(dados$V1[modelo_kmeans$cluster == 1])
table(dados$V2[modelo_kmeans$cluster == 1])
table(dados$V3[modelo_kmeans$cluster == 1])
table(dados$V4[modelo_kmeans$cluster == 1])
table(dados$V5[modelo_kmeans$cluster == 1])
table(dados$V6[modelo_kmeans$cluster == 1])
table(dados$V7[modelo_kmeans$cluster == 1])

table(dados$V1[modelo_kmeans$cluster == 2])
table(dados$V2[modelo_kmeans$cluster == 2])
table(dados$V3[modelo_kmeans$cluster == 2])
table(dados$V4[modelo_kmeans$cluster == 2])
table(dados$V5[modelo_kmeans$cluster == 2])
table(dados$V6[modelo_kmeans$cluster == 2])
table(dados$V7[modelo_kmeans$cluster == 2])

table(dados$V1[modelo_kmeans$cluster == 3])
table(dados$V2[modelo_kmeans$cluster == 3])
table(dados$V3[modelo_kmeans$cluster == 3])
table(dados$V4[modelo_kmeans$cluster == 3])
table(dados$V5[modelo_kmeans$cluster == 3])
table(dados$V6[modelo_kmeans$cluster == 3])
table(dados$V7[modelo_kmeans$cluster == 3])

