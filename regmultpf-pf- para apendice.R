#carrega o banco de dados do trabalho
  #pop = população de trabalho
dados<-read.table("z-pop.txt", h=T, sep="\t")

#lê as primeiras linhas do banco de dados
head(dados)

#carega o pacote GGally, servirá para gerar os gráficos de pares
library(GGally)

#cria o gráfico de  pares, com dispersão, distribuição e correlação das variáveis do banco de dados
#dados[-1] remove a primeira coluna
ggpairs(dados[-1])

#faz a correlação entre os dados
mc<-cor(dados[-1],use="complete.obs")

#carrega a biblioteca para fazer gráficos coloridos de correlação
library(corrplot)

#faz o teste de correlação com o pacote corrplot
mcp<-cor.mtest(mc, conf.level = .95)$p

#desenha o gráfico de correlação colorido
corrplot((mc),type="upper",
         method = "ellipse",p.mat = mcp, insig = "label_sig", pch.col="black",pch.cex=.7,
         sig.level = c(.001,.01,.05))

#carrega o pacote para avaliação da colinearidade dos dados
library(mctest)

#cria o modelo para testar a colinearidade dos dados de altura/dados climáticos
  #vs = variável silvicultural; vc = variável climática
c_vs<-lm(dados$vs~vc1+vc2+vc3, data=dados)

#detecta colinearidade geral
omcdiag(c_vs)

#detecta colinearidade individual, para cada regressor na matriz X
imcdiag(c_vs, all=TRUE)

#ver quais são as variáveis a serem removidas, 
  #pois precisa só das variáveis com médias, não variáveis acumuladas, 
  #e nem outras variáveis silviculturais
names(dados[c(vc1,vc3,vc6,vc8,vc9)])

#remove as variáveis desnecessárias
da_vs<-dados[-c(vc1,vc3,vc6,vc8,vc9)]

#cria o modelo
mod_vs<-lm(vs~.,data=da_vs)

#resume os dados do modelo linear
summary.lm(mod_vs)

#stepwise: escolhe o modelo de acordo com o menor AIC
step(mod_vs,direction="backward")

#cria o modelo em função da escolha pelo AIC
mod_vs<-lm(vs~vc3,data=da_vs)

#resume os dados do modelo linear escolhido
summary.lm(mod_vs)

#predição do modelo escolhido
rs_vs<-predict(mod_vs)

#desenha o histograma da predição do modelo escolhido
ggplot(data.frame(rs_vs),aes(x=rs_vs))+
  geom_histogram(aes(y=..density..),color="black",fill="gray",binwidth = .3)+
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histograma")

#desenha o gráfico qq (de resíduos)
ggplot(data.frame(rs_vs),aes(sample=rs_vs))+ 
  stat_qq() + 
  stat_qq_line()+labs(x="teórico", y="amostra")+
  ggtitle("QQ-Plot")

#desenha o boxplot dos resíduos
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