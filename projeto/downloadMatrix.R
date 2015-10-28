downloadMatrix <- function(gseList){
	
	for (i in 1:length(gseList)){

		downloader::download(gseList[[i]][1],gseList[[i]][2])

	}
	return("Todos os downloads foram concluídos")
}