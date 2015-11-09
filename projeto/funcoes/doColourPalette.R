# Função doColourPalette
# recebe df, type e col como parametro
# df eh uma matriz, type e col sao string
# a funcao adiciona cores para diferenciar os tipos de enfermidades 
# retorna df
doColourPalette <- function(df, type = "none", col = "black"){
    # se type não possuir tipos de enfermidades 
    if (type == "none"){
        return("Você esqueceu de informar a categoria, corrija o codigo e rode novamente :)")
    }
    else {
        # se col for igual a black, significa que  foram passado cores a col 
        if (col != "black"){
            # cria uma nova coluna col em df e adiciona a cor "black"
            df$col = "black"
            for (i in 1:length(type)) {
                # procura o tipo indicado em type[i] na coluna 2 de df e adiciona a cor de col[i] na coluna col de df
                df$col[grep(type[i], df[,2])] = col[i]
            }
            return(df)
        }
        # se col for igual a black, significa que não foram passado cores a col 
        else if (col == "black"){
            # cria uma nova coluna col em df e adiciona a cor "black"
            df$col = col
            # col recebe cores aleatorias
            col = sample(colours(),length(type))
            for (i in 1:length(type)) {
                # procura o tipo indicado em type[i] na coluna 2 de df e adiciona a cor de col[i] na coluna col de df
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


