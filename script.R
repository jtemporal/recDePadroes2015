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

t.dados <- t(dados[[1]])

# plots
# to generate the pdfs uncomment the line above and the one under the plor line
# kmeans
km <- kmeans(t.dados, centers=2)
# pdf("kmeans2centers.pdf")
plot(t.dados,col=km$cluster, pch=19, xlab=NA, ylab=NA)
# dev.off()

km3 <- kmeans(t.dados, centers=3)
# pdf("kmeans3centers.pdf")
plot(t.dados,col=km3$cluster, pch=19, xlab=NA, ylab=NA)
# dev.off()

# pam
p <- cluster::pam(t.dados, k=2)
# pdf("pam2centers.pdf")
plot(t.dados,col=p$clustering, pch=19, xlab=NA, ylab=NA)
# dev.off()

p3 <- cluster::pam(t.dados, k=3)
# pdf("pam3centers.pdf")
plot(t.dados,col=p3$cluster, pch=19, xlab=NA, ylab=NA)
# dev.off()

# diana
di <- cluster::diana(t.dados)
# pdf("diana.pdf")
plot(di)
# dev.off()

# hclust
d = dist(t.dados)
hc <- hclust(d)
#pdf("hclust.pdf")
plot(hc)
#dev.off()

# pca
pca <- prcomp(as.matrix(t.dados), cor=T, scale=F)

#pdf("pca-pairs-1to3.pdf")
pairs(pca$x[,1:3], col=dfMeta$col, pch=19)
#dev.off()

# pdf("pca-1e2.pdf")
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
# dev.off()

# pdf("pca-1e2-cluster-k2.pdf")
plot(
    pca$x,
    col=km$cluster,
    pch=19,
    main = "PCA - cores do kmeans (k=2)",
    xlab=paste0("PC1: ", summary(pca)$importance[2,1]*100, "%"),
    ylab=paste0("PC2: ", summary(pca)$importance[2,2]*100, "%")
)
# dev.off()

# ploting pca usando o ggplot
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

# pdf("pca_with_ggplot.pdf")
plot(p2)
# dev.off()



# gerando apendices
write.table(x=dfMeta, # dado a ser escrito
            append=F, # boleano se o arq ja existe escreve linhas extras (T) ou sobrescreve (F)
            sep="\t", # o tipo do separador, nesse caso \t indica tab
            col.names=T, # boleano para os nomes das colunas
            file="tabelaDeCores.txt", # nome do arquivo
            row.names=F, # boleano para os nomes das linhas
            quote=F) # boleano para presenca de aspas

write.table(x=km$cluster, 
            append=F, 
            sep="\t",
            col.names=F,
            file="kmeans_k2_clusters.txt",
            row.names=T,
            quote=F)

write.table(x=p$clustering, 
            append=F, 
            sep="\t",
            col.names=F,
            file="pam_k2_clusters.txt",
            row.names=T,
            quote=F)
