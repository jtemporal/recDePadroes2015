# Função doColourPalette
# esta função cria cores para os plots baseado nas categorias dos dados
# passados pelo usuário
# recebe df, type e col como parametro
# df eh um data.frame, type e col sao vetores de string 
# df deve ter duas colunas inicialmente, a segunda deve obrigatoriamente conter
# a categoria de cada amostra
# retorna df com uma nova coluna (col) que contem as cores
doColourPalette <- function(df, type = "none", col = "black"){
    # se type não possuir tipos de enfermidades 
    if (type == "none"){
        return("Você esqueceu de informar a categoria, corrija o codigo e rode novamente :)")
    }
    else {
        # se col for igual a black, significa que  foram passado cores a col 
        if (col != "black" && length(col) == length(type)){
            # cria uma nova coluna col em df e adiciona a cor "black"
            df$col = "black"
            for (i in 1:length(type)) {
                # procura o tipo indicado em type[i] na coluna 2 de df e adiciona a cor de col[i] na coluna col de df
                df$col[grep(type[i], df[,2])] = col[i]
            }
            return(df)
        }
        # se a quantidade de categorias não for a mesma que a quantidade de cores
        else if (col != "black" && length(col) != length(type)){
            return("A quantidade de cores não condiz com a quantidade de categorias\nCorrija um dos vetores e rode novamente! :)")
        }
        # se col for igual a black, significa que não foram passado cores pelo usuário
        else {#col == "black"
                # cria uma nova coluna col em df e preenche com  a cor "black"
                df$col = col
                # col recebe cores aleatorias
                col = sample(colours(),length(type))
                for (i in 1:length(type)) {
                    # procura o tipo indicado em type[i] na coluna 2 de df e adiciona a cor de col[i] na coluna col de df
                    df$col[grep(type[i], df[,2])] = col[i]
                }
                return(df)
            } 
        }
    }

