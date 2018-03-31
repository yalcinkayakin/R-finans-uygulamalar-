#clear the memory
rm(list=ls())

#load the dataset
setwd("C:/DataMining/Databases_for_mining/dataset_for_soft_dev_and_comparison/clustering/kohonen_som/Tuto")
D <- read.table("waveform_som_1.txt",sep="\t",dec=".",header=T)
print(summary(D))

#Z value
Z <- scale(D,center=T,scale=T)

#kohonen library
library(kohonen)

#SOM
set.seed(100)
carte <- som(Z,grid=somgrid(15,10,"hexagonal"))

#print
print(carte)

#summary
print(summary(carte))

#architecture of the grid
print(carte$grid)

#progress of the learning process
plot(carte,type="changes")

#colours for the nodes
degrade.bleu <- function(n){
  return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))
}

#count plot
plot(carte,type="count",palette.name=degrade.bleu)

#node membership of individuals
print(carte$unit.classif)

#count the instances in each node
nb <- table(carte$unit.classif)
print(nb)

#check if there are empty nodes
print(length(nb))

#plot distance to neighbours
plot(carte,type="dist.neighbours")

#codebooks
plot(carte,type="codes",codeRendering = "segments")

#codebooks values for the two first nodes
print(carte$codes[1:2,])

#colours for heatmap
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

#codebooks according to the nodes - heatmap
par(mfrow=c(6,4))
for (j in 1:ncol(D)){
  plot(carte,type="property",property=carte$codes[,j],palette.name=coolBlueHotRed,main=colnames(D)[j],cex=0.5)
}
par(mfrow=c(1,1))

#correlation with x and y axes
#v : variable, w : weight, grille : SOM map
weighted.correlation <- function(v,w,grille){
  x <- grille$grid$pts[,"x"]
  y <- grille$grid$pts[,"y"]
  mx <- weighted.mean(x,w)
  my <- weighted.mean(y,w)
  mv <- weighted.mean(v,w)
  numx <- sum(w*(x-mx)*(v-mv))
  denomx <- sqrt(sum(w*(x-mx)^2))*sqrt(sum(w*(v-mv)^2))
  numy <- sum(w*(y-my)*(v-mv))
  denomy <- sqrt(sum(w*(y-my)^2))*sqrt(sum(w*(v-mv)^2))
  #correlation for the two axes
  res <- c(numx/denomx,numy/denomy)
  return(res)
}

#calculation for each column of the codebooks
CORMAP <- apply(carte$codes,2,weighted.correlation,w=nb,grille=carte)
print(CORMAP)

#graphical representation
plot(CORMAP[1,],CORMAP[2,],xlim=c(-1,1),ylim=c(-1,1),type="n")
lines(c(-1,1),c(0,0))
lines(c(0,0),c(-1,1))
text(CORMAP[1,],CORMAP[2,],labels=colnames(Z),cex=0.75)
symbols(0,0,circles=1,inches=F,add=T)

#dispersion of codebooks
#weighted variance
sigma2 <- sqrt(apply(carte$codes,2,function(x,effectif){m<-sum(effectif*(x-weighted.mean(x,effectif))^2)/(sum(effectif)-1)},effectif=nb))
print(sort(sigma2,decreasing=T))

#distance matrix between nodes
dc <- dist(carte$codes)

#hierarchical clustering
cah <- hclust(dc,method="ward.D2",members=nb)
plot(cah,hang=-1,labels=F)

#viewing the 3 groups
rect.hclust(cah,k=3)

#create the groups
groupes <- cutree(cah,k=3)
print(groupes)

#viewing the groups into the map
plot(carte,type="mapping",bgcol=c("steelblue1","sienna1","yellowgreen")[groupes])
add.cluster.boundaries(carte,clustering=groupes)

#affecter chaque individu à sa classe
ind.groupe <- groupes[carte$unit.classif]
print(ind.groupe)

#moyennes coditionnelles - à mettre en relation avec le heatmap
m.cond <- sapply(D,function(x,id.groupe){tapply(x,id.groupe,mean)},id.groupe=ind.groupe)
print(m.cond)

#3D plot
library(plot3D)
points3D(D$V7,D$V11,D$V15,colvar=ind.groupe,col=c("steelblue1","sienna1","yellowgreen"),phi=35,theta=70)

#interactive 3D plot
library(plot3Drgl)
plotrgl(lighting = T)
