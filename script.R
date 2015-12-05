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
write.table(x=dfMeta, 
            append=F, 
            sep="\t",
            col.names=T,
            file="tabelaDeCores.txt",
            row.names=F)
#dfMeta <- doColourPalette(dfMeta, categoria)

km <- kmeans(t(dados[[1]]), centers=2)
pdf("kmeans2centers.pdf")
plot(t(dados[[1]]),col=km$cluster, pch=19)
dev.off()

transDi <- cluster::diana(t(dados[[1]]))
pdf("diana.pdf")
plot(transDi)
dev.off()

d = dist(t(dados[[1]]))
hc <- hclust(d)
pdf("hclust.pdf")
plot(hc)
dev.off()

#pca
pca <- prcomp(as.matrix(t(dados[[1]])), cor=T, scale=F)

pdf("pca-pairs-1to3.pdf")
pairs(pca$x[,1:3], col=dfMeta$col, pch=19)
dev.off()

pdf("pca-1e2.pdf")
plot(
    pca$x,
    col=dfMeta$col,
    pch=19,
    main = "PCA",
    xlab=paste0("PC1: ", summary(pca)$importance[2,1]*100, "%"),
    ylab=paste0("PC2: ", summary(pca)$importance[2,2]*100, "%")
)
legend(
    "top", pch=rep(19,length(coloring)),
    col=coloring,
    legend=categoria
)
dev.off()






