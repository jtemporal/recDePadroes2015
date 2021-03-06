# Função readMyData()
# esta função irá ler os dados disponibilizado pelos estudos de interesse
# criando objetos com o nome de cada estudo ambiente de trabalho
# recebe gse como parametro
# gse eh vetor de caracteres com o(s) ID(s) do(s) estudo(s)
# retorna tally, uma lista em que cada nivel possui os dados de um estudo
readMyData <- function(gse){
    # tally eh inicializado como uma lista com uma string que será substituida
    tally <- list("null")
    for (i in seq_along(gse)){ 
	files <- list.files(pattern = gse[i])
	# descomprime o arquivo de matrix(.gz)
	R.utils::gunzip(files, ext="gz")
	# consultar findMatrixBegin.R para maiores detalhes
	files <- list.files(pattern=gse[i])
	print(files)
        x <- findMatrixBegin(files)
        # le a matrix de estudo do arquivo
        data <- read.table(file = files, header = T, skip = x, fill = T, blank.lines.skip = T)
	# removendo porssiveis linhas que possuam "NA"
        data <- na.omit(data)
        # renomeando as linhas de data
        rownames(data) <- data$ID_REF
        # removendo a coluna de ID
        data <- data[,-1]
        # cria um novo objeto com o nome do estudo com os dados da matrix
	assign(gse[i], data)
	# tally recebe, na posicao i, os dados do estudo lido
	tally[[i]] <- data
    }	
    # renomeia os niveis da lista para corresponder aos estudos lidas
    names(tally) <- gse
    return(tally)
}
