# Aula 5
m <- mtcars

plot(m$mpg~m$wt)
set.seed(33)
va<-sample(32)

# Separa a base do treino pela base de teste
treino<-m[va[1:24],]
teste<-m[va[25:32],]

# Transforma��o Lienar
# Hip�tese: wt tem comportamento exponencial
# Para linealizar wt utilizaremos log, inverso
cor(m$mpg, m$wt)
cor(m$mpg, log(m$wt))

plot(m$mpg~m$wt,col=m$cyl)

#Modelo de regress�o Linear
#mod<-lm(mpg~wt,data=treino)

#Logaritmos
#mod<-lm(mpg~log(wt),data=treino)
#Polinonimos
#mod<-lm(mpg~poly(wt,2),data=treino)
#mod<-lm(mpg~poly(wt,3),data=treino)
#Experimentos para ajuste de curva
#aos pontos de treino
#mod<-lm(mpg~poly(wt,4),data=treino)
#mod<-lm(mpg~poly(wt,16),data=treino)
# An�li. par. Regress�o


#Vari. Cyl como categ�rica
  mod<-lm(mpg~wt+as.factor(cyl), data=treino)

summary(mod)

#Previs�o em teste
p<-predict(mod, newdata=teste)

#compara��o previsto vs real vs Error
cbind(p, teste$mpg, p-teste$mpg)

# SSE
sse <- sum((p-teste$mpg)^2)


## CLASSIFICADORES

d <- read.csv("train.csv")

# Probabilidade de sobreviv�ncia do Titanic

nrow(d[d$Survived==1,])/891


#Utilizando sql no R
install.packages("sqldf")
library(sqldf)


sqldf("select Survived,count(*) from d group by Survived")


table(d$Survived)

prop.table(table(d$Survived))












































