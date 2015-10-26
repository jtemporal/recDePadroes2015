# aula do dia 26/10/2015
# na aula de hoje vamos considerar que ao inves de duas cores de bolas (verde e
# nao verdes) teremos as verdes, as vermelhas e as que nao sao nem verdes nem 
# vermelhas(o resto).

# lembre-se que p1 + p2 + presto = 1

N = 100
x1 = 10
x2 = 20

p1 = seq(0, 1, len=100)
p2 = seq(0, 1, len=100)

# funcao de distbruicao de probabilidades
distProb2D = function(p1, p2, x1, x2,  N){
    res = p1^x1*p2^x2 * (1-p1-p2)^(N-x1-x2)
    return (res)
}

# faz o calculo da mascara
fazMsk = function(p1,p2, x1,x2,pontos){
    msk = matrix(0, length(p1), length(p2))
    for (i in 1:length(p1)){
        for (j in 1:length(p2)){
		    if (p1[i] + p2[j] <= 1){ 
		    	msk[i,j] = distProb2D(p1[i], p2[j], x1, x2, pontos)
		    }
		}
    }
    return(msk)
}



