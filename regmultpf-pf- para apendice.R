#carrega o banco de dados do trabalho
  #pop = popula��o de trabalho
dados<-read.table("z-pop.txt", h=T, sep="\t")

#l� as primeiras linhas do banco de dados
head(dados)

#carega o pacote GGally, servir� para gerar os gr�ficos de pares
library(GGally)

#cria o gr�fico de  pares, com dispers�o, distribui��o e correla��o das vari�veis do banco de dados
#dados[-1] remove a primeira coluna
ggpairs(dados[-1])

#faz a correla��o entre os dados
mc<-cor(dados[-1],use="complete.obs")

#carrega a biblioteca para fazer gr�ficos coloridos de correla��o
library(corrplot)

#faz o teste de correla��o com o pacote corrplot
mcp<-cor.mtest(mc, conf.level = .95)$p

#desenha o gr�fico de correla��o colorido
corrplot((mc),type="upper",
         method = "ellipse",p.mat = mcp, insig = "label_sig", pch.col="black",pch.cex=.7,
         sig.level = c(.001,.01,.05))

#carrega o pacote para avalia��o da colinearidade dos dados
library(mctest)

#cria o modelo para testar a colinearidade dos dados de altura/dados clim�ticos
  #vs = vari�vel silvicultural; vc = vari�vel clim�tica
c_vs<-lm(dados$vs~vc1+vc2+vc3, data=dados)

#detecta colinearidade geral
omcdiag(c_vs)

#detecta colinearidade individual, para cada regressor na matriz X
imcdiag(c_vs, all=TRUE)

#ver quais s�o as vari�veis a serem removidas, 
  #pois precisa s� das vari�veis com m�dias, n�o vari�veis acumuladas, 
  #e nem outras vari�veis silviculturais
names(dados[c(vc1,vc3,vc6,vc8,vc9)])

#remove as vari�veis desnecess�rias
da_vs<-dados[-c(vc1,vc3,vc6,vc8,vc9)]

#cria o modelo
mod_vs<-lm(vs~.,data=da_vs)

#resume os dados do modelo linear
summary.lm(mod_vs)

#stepwise: escolhe o modelo de acordo com o menor AIC
step(mod_vs,direction="backward")

#cria o modelo em fun��o da escolha pelo AIC
mod_vs<-lm(vs~vc3,data=da_vs)

#resume os dados do modelo linear escolhido
summary.lm(mod_vs)

#predi��o do modelo escolhido
rs_vs<-predict(mod_vs)

#desenha o histograma da predi��o do modelo escolhido
ggplot(data.frame(rs_vs),aes(x=rs_vs))+
  geom_histogram(aes(y=..density..),color="black",fill="gray",binwidth = .3)+
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histograma")

#desenha o gr�fico qq (de res�duos)
ggplot(data.frame(rs_vs),aes(sample=rs_vs))+ 
  stat_qq() + 
  stat_qq_line()+labs(x="te�rico", y="amostra")+
  ggtitle("QQ-Plot")

#desenha o boxplot dos res�duos
ggplot(data.frame(rs_vs),aes(y=rs_vs))+
  geom_boxplot(fill="gray")+
  coord_flip()+
  ggtitle("Boxplot")

#teste de normalidade Shapiro-Wilk
shapiro.test(rs_vs)

#carrega a biblioteca de testes de normalidade
library(nortest)

#Teste de normalidade de Lilliefors
lillie.test(rs_vs)

#teste de normalidade de Anderson-Darling
ad.test(rs_vs)

#teste de normalidade de Cramer-Von Mises
cvm.test(rs_vs)