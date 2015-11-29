# script of main code
# note that the study not specified to facilitate change in the code

source("Requirements.R")
source("funcoes/Functions.R")

gse <- c("GSE51808")
link <- getLinkDownloadMatrix(gse)
downloadMatrix(link)
dados <- readMyData(gse)
dados[[1]][1:6,1:3]

categoria <- c("Convalescent", "Healthy control", "Dengue Fever", "Dengue Hemorrhagic Fever")

metadados <- doMeta(gse)
dfMeta <- as.data.frame(metadados[[1]])
#coloring <- sample(colours(),4)
coloring <- c("blue", "green", "pink", "red")
dfMeta <- doColourPalette(dfMeta, categoria, coloring)
#dfMeta <- doColourPalette(dfMeta, categoria)

km <- kmeans(t(dados[[1]]), centers=2)
plot(t(dados[[1]]),col=km$cluster, pch=19)

transDi <- cluster::diana(t(dados[[1]]))
hc <- as.dendrogram(transDi)

#pca
pca <- prcomp(as.matrix(t(dados[[1]])), cor=T, scale=F)
pairs(pca$x[,1:3], col=dfMeta$col, pch=19)
plot(
    pca$x,
    col=dfMeta$col,
    pch=19,
    main = "PCA",
    xlab=paste0("PC1: ", summary(pca)$importance[2,1]*100, "%"),
    ylab=paste0("PC2: ", summary(pca)$importance[2,2]*100, "%")
)
legend(
    "bottomleft", pch=rep(19,length(coloring)),
    col=coloring,
    legend=categoria
)


pc1 <- c(max(pca$x[,1]),  min(pca$x[,1]))
mean(pc1)
#[1] -0.7176172
 
pc2 <- c(max(pca$x[,2]),  min(pca$x[,2]))
mean(pc2)
#[1] -0.8934473



#cluster analysis

km2 <- kmeans(t(dados[[1]]), centers=2)
plot(t(dados[[1]]),col=km$cluster, pch=19)
clu2 <- t(t(km2$cluster))
dfMeta2 <- cbind(dfMeta,clu2)

plot(
    pca$x,
    col=dfMeta2$clu2,
    pch=19,
    main = "PCA",
    xlab=paste0("PC1: ", summary(pca)$importance[2,1]*100, "%"),
    ylab=paste0("PC2: ", summary(pca)$importance[2,2]*100, "%")
)



transDi <- cluster::diana(t(dados[[1]]))
hc <- as.dendrogram(transDi)
plot(cutree(hc,3))


labelColors <- dfMeta$col

colLab <- function(n) {
    if (is.leaf(n)) {
        a <- attributes(n)
        labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
        attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
    }
    n
}

clusMember <- colnames(dados[[1]])
clusMember[grep("II",rownames(abundance.x))]<-2
names(clusMember)<-rownames(abundance.x)

library(ggdendro)
ggdendrogram()





