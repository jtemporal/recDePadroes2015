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
coloring <- c("cyan", "purple")
dfMeta <- doColourPalette(dfMeta, categoria, coloring)

#pca
pca <- prcomp(as.matrix(t(dados[[1]])), cor=T, scale=F)
pairs(pca$x[,1:3], col=dfMeta$col, pch=19)
plot(
    pca$x,
    col=dfMeta$col,
    pch=19,
    main = "PCA hipotetico2",
    xlab=paste0("PC1: ", summary(pca)$importance[2,1]*100, "%"),
    ylab=paste0("PC2: ", summary(pca)$importance[2,2]*100, "%")
)
legend(
    "top", pch=rep(19,length(coloring)),
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

km <- kmeans(t(dados[[1]]), centers=2)
plot(t(dados[[1]]),col=km$cluster, pch=19)

transDi <- cluster::diana(t(dados[[1]]))
hc <- as.dendrogram(transDi)
plot(cutree(hc,3))


