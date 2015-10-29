# funcao downloadMatrix()
# recebe gseList como parametro
# gseList eh uma lista contendo o link(s) para download e nome(s) do(s) arquivo(s)
# retorna uma string confirmando a conclusao do(s) download(s)
downloadMatrix <- function(gseList){ 
	
	for (i in 1:length(gseList)){

		downloader::download(gseList[[i]][1],gseList[[i]][2])

	}
	return("Todos os downloads foram concluídos")
}
