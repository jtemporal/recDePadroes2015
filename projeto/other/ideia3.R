# Trabalho de Rec de Padroes
# Jessica Temporal
# Raissa Poch
# Wilbert Dener

# Requirements:
# XML
# RCurl
# downloader

# gse: # GSE do estudo que queremos fazer download dos dados
# vai retornar o link para download e o nome do objeto para ser salvo
getLinkMatrix <- function(gse){
	
    l <- list("null")
    
    for (i in seq_along(gse)){

    	url <- paste0("http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", gse[i])
    	parseURL <- XML::htmlParse(url)
    	links <- XML::xpathSApply(doc, "//a/@href")
    	link <- links[grep("matrix/", links)]
    	mat <- RCurl::getURL(link)
    	mat <- unlist(strsplit(mat, " "))[length(mat)]
    	mat <- unlist(strsplit(x, "\n"))
    	link <- paste0(link,mat)
    	aux <- c(link,mat)
    	l[[i]] <- aux

    }
	
	return(l)
	# colocar barra de loading
}

gse <- "GSE51808"
link <- getLinkMatrix(gse)

downloadMatrix <- function(gseList){

    for (i in 1:length(gseList)){

        downloader::download(gseList[[i]][1],gseList[[i]][2])

    }

    return("Todos os downloads foram concluídos")
}

downloadMatrix(link)

