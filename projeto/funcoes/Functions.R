# Functions

# Função getLinkDownloadMatrix
getLinkDownloadMatrix <- function(gse){
    l <- list("null")
    for (i in seq_along(gse)){
        url <- paste0( "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", gse[i])
    	parseURL <- XML::htmlParse(url)
    	links <- XML::xpathSApply(parseURL, "//a/@href")
    	link <- links[grep("matrix/", links)]
    	mat <- RCurl::getURL(link)
    	mat <- unlist(strsplit(mat, " "))
    	mat <- mat[length(mat)]
    	mat <- unlist(strsplit(mat, "\n"))
    	link <- paste0(link,mat)
    	aux <- c(link,mat)
    	l[[i]] <- aux
    }
	return(l)
}

# Função downloadMatrix
downloadMatrix <- function(gseList){ 
	for (i in 1:length(gseList)){
		downloader::download(gseList[[i]][1],gseList[[i]][2])
	}
	return("Todos os downloads foram concluídos")
}

# Função findMatrixBegin
findMatrixBegin <- function(mat){
    x = readLines(con = mat)
    lineNum = grep("series_matrix_table_begin",x)
    return(lineNum)
}

# Função readMyData
readMyData <- function(gse){
    tally <- list("null")
    for (i in seq_along(gse)){
	x <- 69 
	files <- list.files(pattern = gse[i])
	system(paste0("gunzip ",files)) # esse comando só funciona no linux 
	files <- list.files(pattern = gse[i])
        data <- read.table(file = files, header = T, skip = x, fill = T)
	assign(gse[i], data)
	tally[[i]] <- data
    }	
    name <- ls(pattern = "GSE")
    names(tally) <- name 
    return(tally)
}

