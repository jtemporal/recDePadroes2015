#rec
#Author: Jessica Caroline Alves Nunes Temporal 7547611

#################################
########### QUESTAO 1 ###########
#################################

#imagem

#################################
########### QUESTAO 2 ###########
#################################

# os dados foram baixados de: ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE1nnn/GSE1828/matrix/
# depois de olhar o txt baixado, percebe-se que as primeiras 68 linhas são metadado e podem
# ser desconsideradas para as analises
# so when reading in the data we skipped those lines (skip=68)
data = read.delim("GSE1828_series_matrix.txt", skip = 68, header = TRUE)
dataNoNA = na.omit(data)
dim(data)
#[1] 3841   29
dim(dataNoNA)
#[1] 2424   29
# mostra que existem nas no df. sendo assim escolhi usar o objeto dataNoNA que desconsidera
# as linhas que possuem NA evitando o erro de definir um valor onde não existe

# escolha das colunas de interesse
head(dataNoNA)
# coluna ID_REF que nao precisamos, entao removemos
dataNoNA = dataNoNA[,2:29]
# colunas da classe C: cepas B115, B147 e B13 correspondentes as colunas:
# "GSM31953" "GSM31954" "GSM31955" "GSM31956" "GSM31957" "GSM31958" "GSM31959" "GSM31960" "GSM31961" "GSM31962" "GSM31963" "GSM31964"
# classe A: cepas FAMEMA e Berenice corrspondentes as colunas:
# "GSM31949" "GSM31950" "GSM31951" "GSM31952" "GSM31965" "GSM31966" "GSM31967" "GSM31968"
# entao removendo as colunas que não são essas acima
dataNoNA = dataNoNA[,3:22]


#opcional[
## sabemos que a classe A sao cepas FAMEMA e Berenice ## assintomaticas ## dividimos de acordo com os dados do site
#A <- cbind(M[,1:4], M[,17:20]) # talvez usar merge

## sabemos que a classe C sao cepas B115, B147 e B13 ## assintomaticas ## dividimos de acordo com os dados do site
#C <- M[,5:16]
#]

# calculando o PCA para a amostra, t(dataNoNA) calcula a transposta para uso da funcao prcomp
pcaDataNoNa = prcomp(t(dataNoNA))

# os valores para PCA se encontram no item x do objeto pcaDataNoNa
# para o calculo bidimensional do PCA usamos apenas as duas primeiras componentes: PC1 e PC2
subPCAdataNoNA = pcaDataNoNa$x[,1:2]

# definição das cores baseado na classe definida na questao
samples = colnames(dataNoNA)
colors = as.data.frame(samples)
colors$classe = "C"
colors$classe[1:4] = "A"
colors$classe[17:20] = "A"
colors$col = "cyan"
colors$col[grep("A", colors$classe)] = "purple"

# eh possivel obter o valor de porcentagem armazenada nos dois primeiros PCs
percPC1 = summary(pcaDataNoNa)$importance[2,1]*100
percPC2 = summary(pcaDataNoNa)$importance[2,2]*100

# para plotar o grafico bidimensional com as porcentagens armazen
plot(
	subPCAdataNoNA, col=colors$col, pch=19,
	main="PCA - Classe A vs Classe C",
	xlab= paste0("PC1\n(porcentagem armazenada nesse PC: ", percPC1 ,"%)"),
	ylab = paste0("PC2 (porcentagem armazenada nesse PC: ", percPC2 ,"%)")
	)
# legenda para as cores dos pontos
legend(
	x = 12.5, y=16, pch=c(19,19),
	col=c("cyan","purple"), 
	legend=c("Classe C", "Classe A" )
	)
# definição da linha hipotetica 
abline(
	a = 15, b=3 ,col="blue", lwd=3
	)

## de acordo com o grafico mostrado pelo codigo acima e a linha hipotetica definida no grafico em azul
## as amostras tem agrupamento claro que pode ser visualizado pelo plot se olharmos pra linha desenhada
## em azul e supondo que ela fosse a melhor fronteira de decisao parar classificar uma nova amostra que
## apareca fica claro que é possivel usar os dois primeiros componentes principais para classificar as 
## amostras as porcentagens da variancia "armazenada" no PC1 e no PC2 sao 36.757% e 32.262% respectivamente

#################################
########### QUESTAO 3 ###########
#################################

# O plot do PCA tridimensional pode ser uma enrascada tendo vista que o dependendo do referencial usado
# o gráfico não será informativo, por exemplo no caso da figura 4, é dificil observar as reais distancias
# de uma amostra ou grupo de amostras para a outra amostra ou grupo de amostras. Além disso a imagem não
# mostra uma clara aproximação da as amostras de RNA amplificado de urina com as amostras tecido-especificas.
# Uma questão é que se o cancer analisado é de prostata, as amostras de Urina vindas de individuos masculinos
# deveriam se aproximar de algum cubo azul (amostra caracteristica) mostrando a similaridade, enquanto as
# amostras de urina de individuos femininos deveriam estar o mais longe possivel mostrando que de fato
# mulheres não poderiam ter cancer de prostata. Porem não fica claro essa diferenciação.
# Do ponto de vista de reconhecimento de padrões podemos ressaltar que não existe um plano que indique a 
# separação dos clusters mencionados na descrição da figura.

#################################
########### QUESTAO 4 ###########
#################################

# incompleta

A = read.table("7547611 A.txt", sep = "\t", colClasses = "numeric")

b = read.table("7547611 b.txt", sep = "\t", colClasses = "numeric")

#A$V3 = A$V2^2 #criando a terceira coluna 

#b$V3 = b$V3^2

#valores para a janela de plot
xmax = max(max(A$V1),max(b$V1))
xmin = min(min(A$V1), min(b$V1))
ymax = max(max(A$V2),max(b$V2))
ymin = min(min(A$V2), min(b$V2))

# cria uma janela "limpa" de plot
plot(A,type='n', ylim = c(ymin,ymax), xlim=c(xmin,xmax))

#colocar os pontos coloridos
points(b, col="cyan", lwd=2)
points(A, col="purple", lwd=2)

firstPoint = function(A, B) {
	indY = which.min(A$V2)
	res = B[indY,]
	return(res)
}

#A: pontos "de cima"
#b: pontos "de baixo"
primeiroPonto = firstPoint(A,b)


