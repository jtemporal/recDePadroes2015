# Função doColourPalette
doColourPalette <- function(df, type = "none", col = "black"){
    if (type == "none"){
        return("Você esqueceu de informar a categoria, corrija o codigo e rode novamente :)")
    }
    else {
        if (col != "black"){
            df$col = "black"
            for (i in 1:length(type)) {
                df$col[grep(type[i], df[,2])] = col[i]
            }
            return(df)
        }
        else if (col == "black"){
            df$col = col
            col = sample(colours(),length(type))
            for (i in 1:length(type)) {
                df$col[grep(type[i], df[,2])] = col[i]
            }
            return(df)
        }
        else {
            df$col = col 
            col = sample(colours(),length(type))
            for (i in 1:length(type)) {
                df$col[grep(type[i], df[,2])] = col[i]
            }
        }
        return(df)
    }
}


