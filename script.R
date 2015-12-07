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

t.dados <- t(dados[[1]])

km <- kmeans(t.dados, centers=2)
pdf("kmeans2centers.pdf")
plot(t.dados,col=km$cluster, pch=19)
dev.off()

km3 <- kmeans(t.dados, centers=3)
pdf("kmeans3centers.pdf")
plot(t.dados,col=km3$cluster, pch=19)
dev.off()

p <- cluster::pam(t.dados, k=2)
pdf("pam2centers.pdf")
plot(t.dados,col=p$clustering, pch=19)
dev.off()

p3 <- cluster::pam(t.dados, k=3)
pdf("pam3centers.pdf")
plot(t.dados,col=p3$cluster, pch=19)
dev.off()

transDi <- cluster::diana(t.dados)
pdf("diana.pdf")
plot(transDi)
dev.off()

d = dist(t.dados)
hc <- hclust(d)
pdf("hclust.pdf")
plot(hc)
dev.off()

#pca
pca <- prcomp(as.matrix(t.dados), cor=T, scale=F)

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

# plotando pca usando o ggplot
dfMeta2 <- dfMeta
dfMeta2$Species <- NA

for (i in 1:length(categoria)) {
    dfMeta2$Species[grep(categoria[i], dfMeta2[,2])] = categoria[i]
    }

dataset = data.frame(species = dfMeta2[,"Species"], pca = pca$x)

prop.pca = pca$sdev^2/sum(pca$sdev^2)

p2 <- ggplot(dataset) + 
geom_point(aes(pca.PC1, pca.PC2, colour = species, 
    shape = species), size = 2.5) +
  labs(x = paste("PC1 (", scales::percent(prop.pca[1]), ")", sep=""),
       y = paste("PC2 (", scales::percent(prop.pca[2]), ")", sep=""))

plot(p2)







df.t.dados <- data.frame(Species=dfMeta2$Species, t.dados)

lda.dados <- lda(formula = Species ~ ., 
                 data = df.t.dados, 

lda.dados$prior
#plot bonito


library(gplots)
heatmap.2(
    #as.matrix(temps), 
    as.matrix(t.dados),
#    dendrogram=TRUE,
    na.rm=TRUE,
        scale="none",
        #RowSideColor=colors.l,
        #ColSideColors=colors.l.1,
        #col=jet.colors(75),
        key=FALSE,
        symkey=FALSE,
        density.info="none",
        trace="none",
        Rowv=TRUE,
        Colv=NA,
        cexRow=1,
        cexCol=0.6,
        keysize=1,
#       dendrogram=TRUE,
        #main = "nondel vs control"
        #labCol=NA
        #labRow=NA
)