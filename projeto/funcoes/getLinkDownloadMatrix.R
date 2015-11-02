# funcao getLinkDownloadMatrix()
# recebe gse como parametro
# gse eh vetor de caracteres com o(s) ID(s) do(s) estudo(s)
# retorna l, uma lista com nome(s) do(s) estudo(s) e o(s) link(s) do(s) estudo(s)
getLinkDownloadMatrix <- function(gse){
	#l eh inicializada como uma lista vazia
	l <- list("null") 
    #criado um laço for onde i vai ate a quanidade de posicoes do vetor gse
    for (i in seq_along(gse)){ 
    	#url recebe uma string do endereco de download, do ID, da posicao i do vetor gse
    	url <- paste0( "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", gse[i])
    	#ira armazenar todas as informacoes da pagina do endereco de download(url) em parseURL
    	parseURL <- XML::htmlParse(url)
    	#pegara todos os links de parseURL e armazenara em links
    	links <- XML::xpathSApply(parseURL, "//a/@href")
    	#link recebe (todos) o(s) link(s) de links que possuem "matrix/"
    	link <- links[grep("matrix/", links)]
    	#Rcurl passa uma tabela de linhas do site (do link) para mat
    	mat <- RCurl::getURL(link)
    	#mat recebe a funcao unlist, que transforma uma lista em um vetor, com parametros strsplit 
    	#strsplit quebra a string mat no momento em que encontra " " 
    	mat <- unlist(strsplit(mat, " "))
    	#mat recebe apenas seu ultimo indice, onde tem o nome do arquivo
    	mat <- mat[length(mat)]
    	#mat recebe a funcao unlist, que transforma uma lista em um vetor, com parametros strsplit 
    	#strsplit quebra a string mat no momento em que encontra "\n" 
    	mat <- unlist(strsplit(mat, "\n"))
    	# link recebe uma string composta pelo link concatenado com mat
    	link <- paste0(link,mat)
    	# criamos vetor aux com os dados link e mat
    	aux <- c(link,mat)
    	# o vetor l, na posição i, recebe aux
    	l[[i]] <- aux
    }
	return(l)
}

