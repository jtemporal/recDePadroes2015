readMyData <- function(gse){
    tally <- list("null")
    for (i in seq_along(gse)){
	x <- 69 # change this 
	files <- list.files(pattern = gse[i])
	system(paste0("gunzip ",files)) # esse comando só funciona no linux muahahaha
	files <- list.files(pattern = gse[i])
        data <- read.table(file = files, header = T, skip = x, fill = T)
	assign(gse[i], data)
	tally[[i]] <- data
    }	
    name <- ls(pattern = "GSE")
    names(tally) <- name # renomeia lista
    
    return(tally)
}
