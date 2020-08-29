

library(readxl)
Nota_Alunos <- read_excel("D:/Nota de Alunos - Parte 1.xlsx")
View(Nota_Alunos)

#-----Tabela de Frequência para Gênero-----#
freq_genero <- table(Nota_Alunos$Genero)
freq_genero
prop_genero <- prop.table(freq_genero)
perc_genero <- round(prop_genero*100,digits = 2)
coluna_freq <- c(freq_genero,sum(freq_genero))
coluna_perc <- c(perc_genero,sum(perc_genero))
names(coluna_freq)[length(coluna_freq)] <- "Total"
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq

#-----Tabela de Frequência para Conceito----#
freq_conceito <- table(Nota_Alunos$Conceito)
freq_conceito
prop_conceito <- prop.table(freq_conceito)
perc_conceito <- round(prop_conceito*100,digits = 2)
coluna_freq <- c(freq_conceito,sum(freq_conceito))
coluna_perc <- c(perc_conceito,sum(perc_conceito))
names(coluna_freq)[length(coluna_freq)] <- "Total"
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq

#-----Tabela de Frequência para Nota Final-----#
intervalos <- cut(Nota_Alunos$Nota_Final,breaks=0:10,right = F)
freq_notas <- table(intervalos)
freq_notas
prop_notas <- prop.table(freq_notas)
perc_notas <- round(prop_notas*100, digits = 2)
coluna_freq <- c(freq_notas,sum(freq_notas))
coluna_perc <- c(perc_notas,sum(perc_notas))
names(coluna_freq)[length(coluna_freq)] <- "Total"
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq

#-----Gráfico de Pizza-------#
rotulos <- paste(perc_genero,"%",sep="")
pie(freq_genero,main="Gráfico de Pizza: Gênero dos Alunos",
	labels = rotulos,col = rainbow(7))
legend(1,1,names(freq_genero),col = rainbow(7),pch = 15)



#-----Gráfico de Barras ou Colunas-----#
barplot(freq_conceito)
barplot(freq_conceito,horiz = T)
freq_cruzada <- table(Nota_Alunos$Genero,Nota_Alunos$Conceito)
#freq_cruzada
barplot(freq_cruzada, beside = T, main = "Conceito vs Gênero",
	ylab = "Número de Aluno",col = c("darkblue","red"))
legend(1,30,rownames(freq_cruzada),col = c("darkblue","red"),
	pch = 15)


#-----Histograma para Nota Final----#
hist(Nota_Alunos$Nota_Final, breaks=0:10, right = F, col = "green",
	xlab = "Notas", ylab = "Frequência", main = "Distribuição de Notas" )
 
#----Gráfico de Séries------#
plot(Nota_Alunos$Prova_1, type = 'l',xlab = "ID Aluno", ylab = "Nota")
lines(Nota_Alunos$Prova_2,col = "blue")
lines(Nota_Alunos$Prova_3,col = "red")

#----Gráfico de Caixa------#
boxplot(Nota_Alunos$Nota_Final ~ Nota_Alunos$Disciplina,
	main = "Nota Final por Disciplina",
    xlab = "Disciplina", col = c("orange","green"))

mean(Nota_Alunos$Prova_1)

wt <- c(5,  5,  4,  1)
x <- c(3.7,3.3,3.5,2.8)
xm <- weighted.mean(x, wt)
m <- mean(x)
m
xm

median(Nota_Alunos$Prova_1)

tabela_freq <- table(Nota_Alunos$Prova_1)
subset(tabela_freq,
		tabela_freq == max(tabela_freq))


#---Percentil 35---#
quantile(Nota_Alunos$Prova_1,.35)

#---Decil 2---#
quantile(Nota_Alunos$Prova_1,.20)

#---Quartil 3---#
quantile(Nota_Alunos$Prova_1,.75)

diff(range(Nota_Alunos$Prova_1))

var(Nota_Alunos$Prova_1)
sd(Nota_Alunos$Prova_1)

n <- length(Nota_Alunos$Prova_1)
((n-1)/n)*var(Nota_Alunos$Prova_1)
sqrt(((n-1)/n)*var(Nota_Alunos$Prova_1))

x_barra <- mean(Nota_Alunos$Prova_1)
s <- sd(Nota_Alunos$Prova_1)
CV <- s*100/x_barra
CV

summary(Nota_Alunos$Prova_1)

summary(Nota_Alunos)

IC_Media_Conhecida <- function(x, sigma, confianca){
  n <- length(x)
  x_barra <- mean(x)
  IC_inf = x_barra - qnorm(1-(confianca/2),0,1)*(sigma/sqrt(n))
  IC_sup = x_barra + qnorm(1-(confianca/2),0,1)*(sigma/sqrt(n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}


IC_Media_Desconhecida <- function(x, confianca){
  n <- length(x)
  x_barra <- mean(x)
  s <- sd(x)
  IC_inf = x_barra - qt(1-(confianca/2),n-1)*(s/sqrt(n))
  IC_sup = x_barra + qt(1-(confianca/2),n-1)*(s/sqrt(n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}

IC_Proporcao <- function(x, confianca, evento){
  n <- length(x)
  x <- x[x == evento]
  prop <- length(x)/n
  IC_inf = prop - qnorm(1-(confianca/2),0,1)*(sqrt((prop*(1-prop))/n))
  IC_sup = prop + qnorm(1-(confianca/2),0,1)*(sqrt((prop*(1-prop))/n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}

Teste_Media_Conhecida <- function(x, sigma, mi_zero, H_1 = c(igual, menor, maior)){
  n <- length(x)
  media <- mean(x)
  z <- (media-mi_zero)/(sigma/sqrt(n))
  if (H_1 == "menor"){
    valor_p <- pnorm(z, 0, 1, lower.tail = T)
  }else if (H_1 == "maior"){
    valor_p <- pnorm(z, 0, 1, lower.tail = F)
  }else {valor_p <- 2*pnorm(z, 0, 1)}
  saida <- cbind(media, mi_zero, z, valor_p)
  saida
}

Teste_Media_Desconhecida <- function(x, mi_zero, H_1 = c(igual, menor, maior)){
  n <- length(x)
  media <- mean(x)
  desvio <- sd(x)
  t <- (media-mi_zero)/(desvio/sqrt(n))
  if (H_1 == "menor"){
    valor_p <- pt(t, n-1, lower.tail = T)
  }else if (H_1 == "maior"){
    valor_p <- pt(t, n-1, lower.tail = F)
  }else {valor_p <- 2*pt(t, n-1)}
  saida <- cbind(media, desvio, mi_zero, t, valor_p)
  saida
}

Teste_Proporcao <- function(x, p_zero, evento, H_1 = c(igual, menor, maior)){
  n <- length(x)
  x <- x[x == evento]
  prop <- length(x)/n
  z <- (prop-p_zero)/(sqrt((p_zero*(1-p_zero))/n))
  if (H_1 == "menor"){
    valor_p <- pnorm(z, 0, 1, lower.tail = T)
  }else if (H_1 == "maior"){
    valor_p <- pnorm(z, 0, 1, lower.tail = F)
  }else {valor_p <- 2*pnorm(z, 0, 1)}
  saida <- cbind(prop, p_zero, z, valor_p)
  saida
}

