dados<-read.table("tabelar-cuia.txt", h=T, sep="\t")
head(dados)

dados$idade <- NULL
head(dados)

library(GGally)

ggpairs(dados[-1])


mc<-cor(dados[-1],use="complete.obs")

library(corrplot)

mcp<-cor.mtest(mc, conf.level = .95)$p
corrplot((mc),type="upper",
         method = "ellipse",p.mat = mcp, insig = "label_sig", pch.col="black",pch.cex=.7,
         sig.level = c(.001,.01,.05))

library(mctest)
head(dados)
dados[-1]

calt<-lm(dados$alt~tm+pm+pa+etom+etoa+h2od, data=dados)
omcdiag(calt)

cdap<-lm(dados$dap~tm+pm+pa+etom+etoa+h2od, data=dados)
omcdiag(cdap)

