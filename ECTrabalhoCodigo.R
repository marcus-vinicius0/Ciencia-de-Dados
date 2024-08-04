# Marcus Vinícius Almeida Florêncio 12021BCC016
# Cauã Pereira Neres 12021BCC005

# Trabalho ESTC

#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 1 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================
# (a) Acresente ao conjunto de dados uma nova coluna chamada infection. Essa
# variável será yes se a criança foi diagnosticada com infecção grave e no caso
# contrário. Lembre-se que essa variável deve ser do tipo categórica (factor).

dados <- read.csv(file = "SBI.csv", header = TRUE)  
dados

dados["infection"] <- !("NotApplicable" == dados[,8])
dados

dados$infection <- as.factor(dados$infection)

summary(dados)

#==============================================================================
# (b) Retire do conjunto de dados as vari´aveis X, id e sbi.
#==============================================================================

dados <- dados[,-c(1,2,8)]
dados

#==============================================================================
# (c) Separe o conjunto de dados em dois novos conjuntos, um para treino e um
# para teste. O conjunto para treino dever´a ter 80% dos dados iniciais.
#==============================================================================

dados <- dados[sample(nrow(dados)),] # embaralhando o conjunto de dados


dados
n <- round(nrow(dados)*0.8) #determinando 80% do conjunto para separar o treinamento do modelo

treinamento <- dados[1:n,] # separando o treinamento
teste <- dados[-(1:n),] # separando o teste

#==============================================================================
# (d) Crie um modelo de árvore de decisão para classificar a variável 
# infection a partir das outras variáveis do conjunto de treinamento. Plote a 
# representaçãoo gráfica da árvore resultante. A partir do conjunto de teste e
# da função predict, verifique a acurácia do modelo. Por fim, crie uma matriz
# de confusão da previsão versus respostas verdadeiras.
#==============================================================================

library(rpart)
library(rpart.plot)

?rpart

?predict

arvore.dados <- rpart(data = treinamento, formula = infection ~ ., method = "class")
rpart.plot(arvore.dados)

rpart.plot(arvore.dados, extra = 101)

previsao <- predict(arvore.dados, newdata = teste, type = "class")
previsao
mean(previsao == teste$infection)

#Matriz de confusão
table(previsao, teste$infection)

# (e) Repita o item acima (exceto a parte referente `a representa¸c˜ao gr´afica) para
# um modelo de floresta aleat´oria

library(randomForest)

floresta <- randomForest(formula = infection ~., data = treinamento, importance = TRUE)
floresta

previsao.floresta <- predict(floresta, newdata = teste, type = "class")
mean(previsao.floresta == teste$infection)

#Matriz de confusão
table(previsao.floresta, teste$infection)

#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================

dados <- read.table("pinguim.csv", sep = ",", header = TRUE)
str(dados)


#==============================================================================
# (a) Plote o gr ́afico deflipper_length_mmversusbody_mass_g.
#==============================================================================
plot(dados$flipper_length_mm, dados$body_mass_g,
     xlab = "Flipper Length (mm)", ylab = "Body Mass (g)",
     main = "Flipper Length vs. Body Mass")



#==============================================================================
# (b) Utilize a fun ̧c ̃aocor()para calcular a correla ̧c ̃ao entre as vari ́av
#eis flip-per_length_mmebody_mass_g.
#==============================================================================
correlacao <- cor(dados$flipper_length_mm, dados$body_mass_g)
correlacao

#==============================================================================
# (c) A partir das duas respostas anteriores, responda:  h ́a alguma rela ̧c ̃ao ent
# reflipper_length_mmebody_mass_g?  Qu ̃ao forte  ́e essa rela ̧c ̃ao
# ?  A rela ̧c ̃ao ́e positiva ou negativa?
#==============================================================================
#De acordo com o resultado da correlação, a relação é forte, indicando que o peso em gramas
# e o tamanho da asa em milímetros dos pinguins estão diretamente relacionadas.
# Com isso podemos concluir que a relação é positiva.


#==============================================================================
# (d)  Utilize a fun ̧c ̃aolm()para determinar a reta do modelo de regress ̃ao line
# arsimples.  Considere flipper_length_mm como a vari ́avel explanat ́or
# ia x ebody_mass_gcomo a vari ́avel resposta y.
#==============================================================================
modelo <- lm(body_mass_g ~ flipper_length_mm, data = dados)
coeficientes <- coef(modelo)
print(coeficientes)


#==============================================================================
# (e)  Explique o coeficiente angular da reta encontrada em (d)..
#==============================================================================
#e)Concluimos que a em média de 1 milímetro no comprimento da asa, o peso coporal médio do pinguim
#aumenta mais ou menos 50.15327 gramas.


#==============================================================================
# (f) A partir do modelo linear encontrado em (d), qual seria o peso m ́edio de u
# m pinguim que possui uma asa de 204 mm?  Vocˆe poderia utilizar esse modelo
# para estimar o peso m ́edio de um pinguim que tivesse uma asa de 168 mm?
# Justifique sua resposta.
#==============================================================================

#O peso médio de um pinguim que possui uma asa de 204 mm seria de 11200 gramas.
#Sim,sso é justificado pelo fato de que estamos fazendo uma interpolação dentro 
# do intervalo de dados observados, onde o modelo foi ajustado.


#==============================================================================
# (g)  De acordo com a vari ́avelisland,  os pinguins habitam 3 ilhas diferentes
# .Divida  o  conjunto  em  trˆes  outros  conjuntos  de  tal  forma  que  cada  novo
# conjunto contenha apenas pinguins de uma  ́unica ilha.
#==============================================================================
dados$island <- factor(dados$island)
subconjuntos <- split(dados, dados$island)
print(subconjuntos)


#==============================================================================
# (h)  A partir do conjunto que cont ́em apenas os pinguins da ilha Biscoe, crie u
# mconjunto que contenha apenas as fˆemeas; em seguida, utilize esse conjunto
# das fˆemeas da ilha Biscoe e o modelo de aglomerados hier ́arquicos com om ́etod
# o ward.D2 para agrupar os pinguins. Em seguida, plote o dendograma referente ao 
# modelo criado..
#==============================================================================
subconjuntoBiscoe <- subconjuntos[["Biscoe"]]

femeasBiscoe <- subconjuntoBiscoe[subconjuntoBiscoe$sex == "FEMALE", ]
print(femeasBiscoe)

agrupamento <- femeasBiscoe[, c("species", "culmen_length_mm", "culmen_depth_mm", "flipper_length_mm", "body_mass_g")]

matriz_distancias <- dist(agrupamento[, -1])  
modelo_agrupamento <- hclust(matriz_distancias, method = "ward.D2")

plot(modelo_agrupamento, hang = -1, main = "Dendrograma dos Pinguins Fêmeas da Ilha Biscoe",
     xlab = "Pinguins Fêmeas", ylab = "Distância", sub = NULL)


#==============================================================================
# # (i) A partir da an ́alise do gr ́afico plotado em (h), em qual altura vocˆ
# e cortaria odendograma?  Justifique sua resposta.  Quantos aglomerados resultaram 
# docorte?  Identifique a propor ̧c ̃ao de cada esp ́ecie dentro de cada aglomerad
# o.Comente os resultados obtidos.
#==============================================================================


fviz_dend(modelo_agrupamento, k = 12)

aglomerados <- cutree(modelo_agrupamento, k = 12)
femeasBiscoe$aglomerados <- factor(aglomerados)

table(aglomerados)

table(femeasBiscoe$species[aglomerados == 1])
table(femeasBiscoe$species[aglomerados == 2])
table(femeasBiscoe$species[aglomerados == 3])
table(femeasBiscoe$species[aglomerados == 4])
table(femeasBiscoe$species[aglomerados == 5])
table(femeasBiscoe$species[aglomerados == 6])
table(femeasBiscoe$species[aglomerados == 7])
table(femeasBiscoe$species[aglomerados == 8])
table(femeasBiscoe$species[aglomerados == 9])
table(femeasBiscoe$species[aglomerados == 10])
table(femeasBiscoe$species[aglomerados == 11])
table(femeasBiscoe$species[aglomerados == 12])

# 12 seria a ,melhor altura a ser cortada na minha visão, pois vamos distribuir bem
# o resultado dos aglomerados em 12 aglomerados. Assim vamos deixar nossos aglomerados
# bem distribuidos.

proporcao_especies <- lapply(1:12, function(i) {
  table(femeasBiscoe$species[aglomerados == i]) / sum(aglomerados == i)
})

for (i in 1:12) {
  cat("Proporção de espécies no aglomerado", i, ":\n")
  print(proporcao_especies[[i]])
}

# No aglomerado 1, há apenas pinguins da espécie Adelie.
# No aglomerado 2, a grande maioria (85.71%) são da espécie Adelie, enquanto uma pequena proporção (14.29%) são da espécie Gentoo.
# Nos aglomerados 3, 4 e 5, todos os pinguins são da espécie Adelie.
# Nos aglomerados 6 a 12, todos os pinguins são da espécie Gentoo.


#==============================================================================
#(j)  Utilize o conjunto do item (h) para construir um modelo k-means comk= 2e 
#outro modelo k-means comk= 3.  Comente os resultados obtidos.
#==============================================================================
modelo_kmeans_com_2 <- kmeans(agrupamento[, -1], centers = 2)
modelo_kmeans_com_2$cluster

femeasBiscoe$sex[modelo_kmeans_com_2$cluster == 1]
femeasBiscoe$sex[modelo_kmeans_com_2$cluster == 2]


modelo_kmeans_com_3 <- kmeans(agrupamento[, -1], centers = 3)
modelo_kmeans_com_3$cluster

femeasBiscoe$sex[modelo_kmeans_com_3$cluster == 1]
femeasBiscoe$sex[modelo_kmeans_com_3$cluster == 2]
femeasBiscoe$sex[modelo_kmeans_com_3$cluster == 3]


#==============================================================================
#(k)  Considere o modelo obtido em (d) comk= 3.  Plote um gr ́afico em que oeixox
# ́e  dado  pela  vari ́avel flipper lenght e  o  eixoy ́e  dado  pela  vari ́avelb
# ody mass e cada aglomerado tenha uma cor diferente.  Por fim, acrescentea este gr ́afico, os centr ́oides de cada aglomerado.
#==============================================================================
modelo <- lm(body_mass_g ~ flipper_length_mm, data = dados)

coeficientes <- coef(modelo)
print(coeficientes)

dados$predicao <- predict(modelo)

k <- 3

agrupamento <- kmeans(dados[, c("flipper_length_mm", "body_mass_g")], centers = k)

plot(dados$flipper_length_mm, dados$body_mass_g, col = agrupamento$cluster, 
     pch = 19, xlab = "Flipper Length", ylab = "Body Mass", main = "Dados Agrupados")


points(agrupamento$centers[, 1], agrupamento$centers[, 2], col = 1:k, pch = 4, cex = 2)
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 4)

#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 3 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================

# (a) Aplique o modelo de aglomerados hier´arquicos com o m´etodo ward.D2 para
# este conjunto e, em seguida, apresente o dendograma resultante do modelo

library(factoextra)
library(ggplot2)

azeites <- read.table(file = "olive.txt", header = TRUE, sep = ",")  
azeites

azeites$region <- as.factor(azeites$region)

dados_padronizados <- scale(azeites[,-1])

matriz_distancias <- dist(dados_padronizados)

modelo <- hclust(matriz_distancias, method = "ward.D2")

plot(modelo)
#==============================================================================
# (b) Corte o dendograma em uma altura que resulte em 5 diferentes aglomerados.
# Identifique a proporção de cada região (Sul, Norte, Sardínia) que está 
# dentro de cada um dos cinco aglomerados
#==============================================================================

fviz_dend(modelo, k =5)

#analisando cada aglomerado
aglomerados <- cutree(modelo, k = 5)
azeites$aglomerados <- factor(aglomerados)

table(azeites$region[aglomerados == 1])
table(azeites$region[aglomerados == 2])
table(azeites$region[aglomerados == 3])
table(azeites$region[aglomerados == 4])
table(azeites$region[aglomerados == 5])


#==============================================================================
# (c) Aplique agora o modelo K-means com k = 5. Comente os resultados 
# encontrados
#==============================================================================


modelo_kmeans <- kmeans(dados_padronizados, centers = 5)
modelo_kmeans$cluster

table(azeites$region[modelo_kmeans$cluster == 1])

# Nesse cluster ficou concentrado azeites da Região Northern Italy e alguns da
# Southern Italy 
# 
# Northern Italy       Sardinia Southern Italy 
# 81                      0              9 

table(azeites$region[modelo_kmeans$cluster == 2])


# Nesse cluster ficou concentrado somente azeites da Região Northern Italy
# Northern Italy       Sardinia Southern Italy 
# 60                      0              0 

table(azeites$region[modelo_kmeans$cluster == 3])

# Nesse cluster ficou concentrado azeites da Região Southern Italy e alguns da
# Northern Italy 

# Northern Italy       Sardinia Southern Italy 
# 5                       0             97 

table(azeites$region[modelo_kmeans$cluster == 4])
# Nesse cluster ficou concentrado azeites da Região Sardinia e alguns da
# Northern Italy 

# Northern Italy       Sardinia Southern Italy 
# 5                     98              0 

table(azeites$region[modelo_kmeans$cluster == 5])
# Nesse cluster ficou concentrado somente azeites da Região Southern Italy
# Northern Italy       Sardinia Southern Italy 
# 0                      0            217 

#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 4 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================

# Considere trˆes urnas com as seguintes configura ̧c ̃oes:  a urna I con-t ́em 6 bolas pret
# as, 3 brancas e 4 vermelhas; a urna II cont ́em 3 bolas pretas, 5brancas e 2 vermelhas;
# a urna III cont ́em 4 bolas pretas, 2 brancas e 2 verme-lhas.  Lan ̧ca-se um dado equilibrado. 
# Se sair 5, uma bola da urna I  ́e retirada;se sair 1,  4 ou 6,  ent ̃ao uma bola da urna II
# ́e retirada;  se sair 2 ou 3,  ent ̃aouma bola da urna III  ́e retirada.  Estime a probabilid
# ade da bola retirada servermelha.

urna1 <- c("preta", "preta", "preta", "preta", "preta", "preta", "branca", "branca", "branca", "vermelha", "vermelha", "vermelha")
urna2 <- c("preta", "preta", "preta", "branca", "branca", "branca", "branca", "branca", "vermelha", "vermelha")
urna3 <- c("preta", "preta", "preta", "preta", "branca", "branca", "vermelha", "vermelha")

jogada <- function() {
  dado <- sample(1:6, 1)
  if (dado == 5) {
    bola_retirada <- sample(urna1, 1)
  } else if (dado %in% c(1, 4, 6)) {
    bola_retirada <- sample(urna2, 1)
  } else {
    bola_retirada <- sample(urna3, 1)
  }
  return(bola_retirada)
}

n_jogadas <- 100000
bolas_vermelhas <- replicate(n_jogadas, jogada())
probabilidade_vermelha <- sum(bolas_vermelhas == "vermelha") / n_jogadas

cat("A probabilidade estimada da bola retirada ser vermelha é:", probabilidade_vermelha)

#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 5 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================

# Uma urna cont´em bilhetes numerados de 1 at´e 30, todos do mesmo
# tamanho. Considere o seguinte experimento: retirar um bilhete da urna, 
# registrar o resultado e devolver o bilhete para a urna. Vocˆe continuará a 
# sortear bilhetes at´e que todos os n´umeros sejam retirados. Seja N o n´umero 
# de sorteios que vocˆe precisou realizar at´e que todos os n´umeros fossem 
# registrados. Utilize o M´etodo de Monte Carlo para estimar E[N], isto ´e, 
# para estimar a esperança de N.

bilhete <- function(valor){
  resultados <- sample(1:30, size = 1, replace = TRUE)
  resultados
  
  
  
  valor <- valor[valor != resultados]
  valor
  
  return(valor)
}


experimento <- function(){
  v <- c()
  v <- 1:30
  v
  
  i <- 0
  while (length(v) != 0) {
    v <- bilhete(v)
    i <- i+1
  }
  return(i)
}


resultados <- c()
for (k in 1:10000) {
  resultados[k] <- experimento()
}

resultados
mean(resultados)

# A esperança é aproximadamente [1] 119.5617

#==============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 6 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#==============================================================================

# Um dado ser ́a lan ̧cado at ́e que o n ́umero 4 seja obtido pela terceiravez
# .  SejaXa vari ́avel aleat ́oria que conta o n ́umero de lan ̧camentos que foramnece
# ss ́arios para obter o n ́umero 4 pela terceira vez.

simular_lancamentos <- function() {
  lancamentos <- 0
  numero_quatro <- 0
  
  while (numero_quatro < 3) {
    resultado <- sample(1:6, 1)  # Lançamento do dado
    if (resultado == 4) {
      numero_quatro <- numero_quatro + 1
    }
    lancamentos <- lancamentos + 1
  }
  
  return(lancamentos)
}

#==============================================================================
# (a) Estime, via Monte Carlo, a esperan ̧ca deX.
#==============================================================================
n_simulacoes <- 100000
resultados <- replicate(n_simulacoes, simular_lancamentos())
esperanca_X <- mean(resultados)
esperanca_X

#==============================================================================
# (b) Estimar a probabilidade de X ser menor do que 10
#
#==============================================================================

probabilidade_menor_10 <- sum(resultados < 10) / n_simulacoes
cat("Esperança de X:", esperanca_X, "\n")
cat("Probabilidade de X ser menor do que 10:", probabilidade_menor_10)











