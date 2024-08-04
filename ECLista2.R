# Alunos:
#   Marcus Vinícius Almeida Florêncio 12021BCC016
#   Cauã Pereira Neres                12021BCC005


#===============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#===============================================================================

# Harold Frederick Shipman (Nottingham, 14 de janeiro de 1946 — 
# Wakefield, 13 de janeiro de 2004), conhecido como “Doutor Morte”, foi 
# um médico e assassino em série britânico condenado pela morte de muitos 
# pacientes entre as décadas de 1970 e 1990. Dr. Shipman é, talvez, o assassino 
# em série mais prolífico da História Moderna. O arquivo dados.txt contém 
# informações sobre o sexo, a idade, o local da morte (casa do paciente; 
# hospital; casa de repouso) e o ano da morte das vítimas de Shipman. Antes de
# responder as questões abaixo, abra o arquivo dados.txt e compreenda sua 
# estrutura. Importe o arquivo para o R e utilize-o para responder os seguintes 
# itens.

#===============================================================================
# (a) Escolha um gráfico apropriado para representar as frequências das 
# categorias da variável sexo. Comente os resultados encontrados.
#===============================================================================

dados <- read.table(file = "dados.txt", header = TRUE, sep = ";")  
dados

summary(dados)

tabela <- table(dados$Genero)
tabela

dados$Genero <- as.factor(dados$Genero)
dados


summary(dados)

library(ggplot2)

ggplot(data = dados, aes(x = Genero))+
  geom_bar()

# O numéro de mulheres mortas foi 3x maior do que o número de homens,
# assim podemos deduzir que ele tinha uma preferência em matar mulheres.

#===============================================================================
# (b) Apresente o histograma da variável idade em 8 (argumento bins na 
# geometria do histograma) intervalos. Comente os resultados obtidos. Analise 
# este gráfico para cada gênero.
#===============================================================================

ggplot(data = dados, aes(x = Idade))+
  geom_histogram(binwidth = 8, position = "dodge")+
  geom_bar()+
  facet_wrap(~Genero)

# Ele tinha uma preferência em matar pessoas mais velhas acima de 60 anos,
# Ele matou muitas mulheres na faixa de idade entre 70 e 90 anos.
# E a maioria dos homens também foi nessa faixa.

#===============================================================================
# (c) Apresente o boxplot da variável idade. Comente os resultados obtidos.
#===============================================================================

ggplot(data = dados, aes(y = Idade))+
  geom_boxplot()

# Tem alguns outliers entre 40 e 60
# A mediana é próxima de 80
# O segundo e o terceiro quartil estão entre aproximadamente
# 70 e 85 anos

#===============================================================================
# (d) Apresente um gráfico para representar o local da morte. Comente os 
# resultados obtidos.
#===============================================================================

summary(dados)

tabela <- table(dados$LocalDaMorte)
tabela

dados$LocalDaMorte <- as.factor(dados$LocalDaMorte)

summary(dados)

ggplot(data = dados, aes(x = LocalDaMorte))+
  geom_bar()

# A grande de maioria(+200) dos pacientes morreram nas próprias casas
# E a outra minoria(-20) morreram no hospital ou enfermagem domiciliar
# Tinha uma preferência por matar na casa dos pacientes

#===============================================================================
# (e) Analise graficamente o ano da morte das vítimas de Harold Shipman.
#===============================================================================

ggplot(data = dados, aes(x = AnoDaMorte))+
  geom_bar()

# Sua primeira vítima foi em 1975
# Sua última vítima foi em 1998
# Inicialmente ele assassinava pouco, mas a partir de 1984 ele aumentou 
# consideravelmente o número de vítimas.
# Em 1991 ele teve um hiato
# E a partir de 1992 ele voltou a assasinar, só que dessa vez ele voltava
# Fazendo mais vítimas em um período do que ele já tinha feito antes

#===============================================================================
# (f) Com base nas informações obtidas nos itens anteriores, escreva um 
# parágrafo sobre o padrão e o perfil das vítimas de Harold Shipman
#===============================================================================

# De acordo com os dados analisados, pode-se concluir que Harold tinha uma
# preferência em fazer vítimas do sexo feminino nas suas próprias casas, as 
# vítimas possuiam idade entre 70 e 85 anos, a partir de 1993 o número de 
# vítimas era muito maior.
# Provavelmente ele preferia assassiná-los em casa pois não haveriam testemunhas
# Além disso, ele provavelmente preferia essa faixa, pois não levantaria muita 
# suspeita uma vez que as vítimas já eram idosas.

#===============================================================================
#<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exercício 4 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#===============================================================================

# O conjunto cogumelos.csv contém informações sobre 23 espécies de cogumelos 
# dos gêneros Agaricus e Lepiota, retiradas do Guia de Campo da 
# Sociedade Audubon para Cogumelos da América do Norte (1981). Cada espécie é 
# classificada (class) como comestível (edible = e) ou venenosa (poisonous =
# p). Detalhes sobre cada uma das variáveis do conjunto estão no Kaggle ou em UC
# Irvine Machine Learning
# Repository.
# 
# Embaralhe o conjunto e, em seguida, separe-o em treinamento (80%) e teste 
# (20%). Estude o conjunto de treinamento a partir de uma análise gráfica (nesta
# parte faça algumas perguntas interessantes e encontre um gráfico que ajudará 
# na sua resposta; exemplos: quantas espécies venenosas há no treinamento? e 
# comestíveis?; a forma, a cor ou o odor pode influenciar na classificação? etc). 
# A partir das conclusões e observações obtidas, crie um modelo de árvore de 
# decisão para classificar um cogumelo como comestível ou venenoso. Avalie a
# taxa de acerto e comente o resultado obtido.

cogumelos <- read.csv(file = "cogumelos.csv", header = TRUE)  
cogumelos

#===============================================================================

cogumelos <- cogumelos[sample(nrow(cogumelos)),]

n <- round(nrow(cogumelos)*0.8)

n 

treinamento <- cogumelos[1:n,]
teste <- cogumelos[-(1:n),]

nrow(teste)

nrow(treinamento)


#===============================================================================


summary(cogumelos)


ggplot(data = treinamento, aes(x = class, y = odor))+
  geom_point()

ggplot(data = treinamento, aes(x = class, y = spore.print.color))+
  geom_point()


resposta <- c()
for( j in 1:nrow(teste)){
 if((teste$spore.print.color[j] == "r") || ((teste$odor[j] == "y")|| (teste$odor[j] == "s") || (teste$odor[j] == "p") || (teste$odor[j] == "m") || (teste$odor[j] == "f") || (teste$odor[j] == "c") )){
   resposta[j] <- "p"
 } else if( ((teste$spore.print.color[j] == "b") || (teste$spore.print.color[j] == "o") || (teste$spore.print.color[j] == "u") || (teste$spore.print.color[j] == "y")) || ( (teste$odor[j] == "l")|| (teste$odor[j] == "n") || (teste$odor[j] == "a"))){
   resposta[j] <- "e"
 } else {
   resposta[j] <- "outro"
 }
  
}


resposta
mean(resposta == teste$class)

# spore.print.color e odor são variáveis que ajudam bastante, a identificar
# se um cogumelo é comestível ou não.
# Por exemplo um cogumelo da spore.print.color = verde já indica que ele é venenoso
# Essas variáveis ajudaram muito pois elas possuem espécificos elementos que são
# comuns em apenas um dos grupos, sendo venenoso ou comestível.
# Por isso consegui o incrível resultado de alcançar 99% de precisão no modelo
