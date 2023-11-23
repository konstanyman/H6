#setwd("D:/R/")    

# 6.harjoitukset: monimuuttujamenetelmät

library(foreign)
dat<-read.spss("data_demo6_2019r.sav", to.data.frame=TRUE)
attach(dat)

# Uusi data, jossa vain hyötymuuttujat 10 kpl

data_pca <- dat[,2:length(dat)]

# korrelaatiokertoimet

data.kor2<-cor(data_pca, method = "pearson", use = "complete.obs")
data.kor2

# korrelaatiokertoimien p-arvot

install.packages("Hmisc")
library(Hmisc)
res2 <- rcorr(as.matrix(data_pca))
res2

# korrelaatiokertoimet ja p-arvot rinnakkain

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res3<-rcorr(as.matrix(data_pca[,1:10]))
flattenCorrMatrix(res3$r, res3$P)

# korrelaatioiden voimakkuudet kuviona

install.packages("corrplot")
library(corrplot)
corrplot(data.kor2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Korrelaatiomatriisin sopivuus pääkomponenttianalyysiin

install.packages ("psych")
library (psych)

cortest.bartlett(data.kor2, n =53,diag=TRUE)
KMO(data.kor2)

# pääkomponenttianalyysi

pca <- prcomp ( data.kor2 , center = T, scale = T)
pca
summary(pca)

# ominaisarvot

(pca$sdev)^2

# Scree plot

screeplot(pca, type="lines") +
  abline(h=1, lty=2)

#  Valitaan kolme pääkomponenttia, varimax-rotaatio

pca.chosen <- pca$ rotation [ ,1:3]
pca.varimax <- varimax (pca.chosen )
pca.varimax

#  Valitaan kolme pääkomponenttia, promax-rotaatio

pca.chosen <- pca$ rotation [ ,1:3]
pca.promax <- promax (pca.chosen )
pca.promax

# Miten pääkomponentit selittävät muuttujia

p<-ncol(data_pca)
n<-nrow(data_pca)
e<-eigen(data.kor2) 
L<-e$values #placing the eigenvalues in L
Vm<-matrix(0,nrow=p,ncol=p) #creating a p x p matrix with zeroes.
#Vm is an orthogonal matrix since all correlations between variable are 0.
diag(Vm)<-L #putting the eigenvalues in the diagonals
Vm #check-- matrix with eigenvalues on the diagonals
comp.matrix<-e$vectors %*% sqrt(Vm) #sometimes referred to as P matrix
#or eigenvectors x sqrt(Vm): P %*% t(P) is equal to the R matrix.
comp.matrix

#Pääkomponentti 1 selittämä osuus muuttujien vaihtelusta

comp.matrix[,1]^2 

#Pääkomponentti 2 selittämä osuus muuttujien vaihtelusta

comp.matrix[,2]^2

#Pääkomponentti 3 selittämä osuus muuttujien vaihtelusta

comp.matrix[,3]^2

# kommunaliteetit

comp.matrix[,1]^2 +comp.matrix[,2]^2 +comp.matrix[,3]^2 

 

# pääkomponenttien keskinäiset korrelaatiot

library(foreign)
dat2<-read.spss("data_demo6pc_2019r.sav", to.data.frame=TRUE)
attach(dat2)

cor.test(apuvalineet,media,method="pearson")
cor.test(apuvalineet,laheiset,method="pearson")
cor.test(media,laheiset, method="pearson")

# K-means klusterointi pääkomponenttipistemäärillä

dat3 = dat2[,c(12,13,14)]

# 2 klusteria

set.seed(7)
km2 = kmeans(dat3, 2, nstart=100)
km2

#3 klusteria

set.seed(7)
km3 = kmeans(dat3, 3, nstart=100)
km3

#4 klusteria

set.seed(7)
km4 = kmeans(dat3, 4, nstart=100)
km4

#5 klusteria

set.seed(7)
km5 = kmeans(dat3, 5, nstart=100)
km5



 
