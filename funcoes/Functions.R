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
    x <- readLines(con = mat)
    lineNum <- grep("series_matrix_table_begin",x)
    return(lineNum)
}

# Função readMyData
readMyData <- function(gse){
    tally <- list("null")
    for (i in seq_along(gse)){
	files <- list.files(pattern = gse[i])
	system(paste0("gunzip ",files)) # esse comando só funciona no linux
	files <- list.files(pattern = gse[i])
	x <- findMatrixBegin(files[i])
        data <- read.table(file = files, header = T, skip = x, fill = T, blank.lines.skip = T)
        data <- na.omit(data)
        rownames(data) <- data$ID_REF
        data <- data[,-1]
        assign(gse[i], data)
	tally[[i]] <- data
    }
    names(tally) <- gse
    return(tally)
}

# Função doMeta
doMeta <- function(gse){
    l <- list("null")
    for (j in seq_along(gse)){
        x <- readLines(con = paste0("http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", gse[j]))
        gsmIndex <- grep("GSM", x)
        gsm <- strsplit(x=x[gsmIndex], split='">')
        patient <- strsplit(x=x[gsmIndex + 1], split='">')
        for (k in seq_along(gsmIndex)){
            patient[[k]] <- patient[[k]][2]
            patient[[k]] <- substr(patient[[k]], start=1, stop=nchar(patient[[k]])-5)
            gsm[[k]] <- substr(gsm[[k]][length(gsm[[k]])], start=1, stop=10)
        }
        gsm <- unlist(gsm)
        patient <- unlist(patient)
        l[[j]] <- cbind(gsm,patient)
    }
    names(l) <- gse 
    return(l)
}

# Função doColourPalette
doColourPalette <- function(df, type = "none", col = "black"){
    if (type == "none"){
        return("Você esqueceu de informar a categoria, corrija o codigo e rode novamente :)")
    }
    else {
        if (col != "black" && length(col) == length(type)){
            df$col = "black"
            for (i in 1:length(type)) {
                df$col[grep(type[i], df[,2])] = col[i]
            }
            return(df)
        }
        else if (col != "black" && length(col) != length(type)){
            return("A quantidade de cores não condiz com a quantidade de categorias\nCorrija um dos vetores e rode novamente! :)")
        }
        else {
                df$col = col
                col = sample(colours(),length(type))
                for (i in 1:length(type)) {
                    df$col[grep(type[i], df[,2])] = col[i]
                }
                return(df)
            } 
        }
    }
