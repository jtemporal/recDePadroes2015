# Função doColourPalette
doColourPalette <- function(df, type = "none", col = "black", num = 0){
    if (type == "none"){
        return("Você esqueceu de informar a categoria, corrija o codigo e rode novamente :)")
    }
    else {
        if (col != "black" && num == 0){
            df$col = "black"
            for (i in 1:length(type)) {
                df$col[grep(type[i], df$patient)] = col[i]
            }
            return(df)
        }
        else if (col == "black" && num != 0){
            df$col = col
            col = sample(colours(),num)
            for (i in 1:length(type)) {
                df$col[grep(type[i], df$patient)] = col[i]
            }
            return(df)
        }
        else {
            
            
            
        }
        return(df)
    }
}


