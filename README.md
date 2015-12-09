# Projeto para disciplina de Reconhecimento de Padroes 2015

Esse projeto teve como objetivo usar o conhecimento adquirido em sala de aula.

Escolhemos dados de dengue disponiveis publicamente no banco de dados GEO.
-------------------
Contribuidores:
-------------------
* [Jessica Temporal](https://github.com/jtemporal)
* [Raissa de F. Poch](https://github.com/raissapoch)
* [Wilbert Dener](https://github.com/wilbertdener)
 

Plots
-------------------
PCA calculado pela funcao prcomp, PC1 x PC2
![pca](https://github.com/jtemporal/recDePadroes2015/blob/master/plots/pca-1com2-legendado.png)

PCA calculado pela funcao prcomp: pareado PC1 x PC2, PC1 x PC3, PC2 x PC3
![pcaPairs](https://github.com/jtemporal/recDePadroes2015/blob/master/plots/pca-pairs.png)

Cluster gerado com a funcao kmeans (k=2)
![kmeans](https://github.com/jtemporal/recDePadroes2015/blob/master/plots/kmeans-2centers.png)

Cluster gerado com a funcao pam (k=2)
![pam](https://github.com/jtemporal/recDePadroes2015/blob/master/plots/pam-2centers.png)

Dendrograma gerado com a funcao hclust
![dendroHclust](https://github.com/jtemporal/recDePadroes2015/blob/master/plots/dendro-hclust-cloredLeaf.jpg)

Dendrograma gerado com a funcao diana
![dendroDiana](https://github.com/jtemporal/recDePadroes2015/blob/master/plots/dendro-Diana-cloredLeaf.jpg)

PCA 3D com os 3 primeiros PCs pode ser encontrado [nesse link](https://youtu.be/8B2cN3rDB0E)
PCA 3D com PC 1 3 e 4 pode ser encontrado [nesse link](https://youtu.be/IZK3VB4bxbs)

-------------------
IMPORTANTE
-------------------
Se estiver rodando o script no linux você precisa ter as seguintes bibliotecas instaladas:
- XML
- Curl

para tanto, você pode fazer os seguintes comandos no seu terminal:

```
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libxml2-dev
```
