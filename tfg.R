library(readxl) #Libreria para leer base de datos
library(openxlsx)
library(stats) #Libreria necesaria parar cSEM
library(cSEM) #Libreria para ec estructurales
library(ggplot2) #Libreria necesaria para la librería factoextra
library(MASS) #Libreria necesaria para factoextra
library(factoextra) #libreria necesaria para calcular número de clústeres
library(klaR) #libreria necesaria para el kmodes
library(e1071) #libreria para cmeans
library(cluster) #libreria para pam, silhouette
library(corrplot)#gráfico correlaciones
library(psych) #test de Barlett
library (psy)
library(ppclust) #libreria para fukuyama, xiebeni,etc
library(dendextend)
library(lavaan) #libreria para el invarianza configuracional

######################################## DATOS #################################
# Cargamos los datos
bd<- read_excel("C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\DATOS\\BD.xlsx")

# Analizar  datos
str(bd)
summary(bd)

# Agrupación variables según tipo
#DICOTÓMICAS
gen_exp_prop <- bd[,c("gender", "expepais", "propipais")]

#CONTINUAS
## Calcular PCA
#### Asignar variables
env_mim_coer <- bd[,c("v97", "v98", "v99", "v39", "v40", "v41", "v42", "v43", "v44")]
env_dyna <- bd[,c("v97", "v98", "v99")]
mime_press <- bd[,c("v39", "v40", "v41")]
coerc_press<- bd[,c("v42", "v43", "v44")]

### Calcular matriz de correlacion y determinante
cor_IE = round(cor(env_mim_coer),3); cor_IE ; det(cor_IE)
cor_IE_env = round(cor(env_dyna),3); cor_IE_env; det(cor_IE_env)
cor_IE_mime = round(cor(mime_press),3); cor_IE_mime; det(cor_IE_mime)
cor_IE_coerc = round(cor(coerc_press),3); cor_IE_coerc; det(cor_IE_coerc)

### Dibujar la matriz de correlaciones
# Dibujar la matriz de correlaciones
corrplot(cor(env_mim_coer), order = "hclust", tl.col = 'black', tl.cex = 1)
title(main = "Matriz de Correlaciones - Conjunto Completo")

corrplot(cor(env_dyna), order = "hclust", tl.col = 'black', tl.cex = 1)
title(main = "Matriz de Correlaciones - Environment Dynamism")

corrplot(cor(mime_press), order = "hclust", tl.col = 'black', tl.cex = 1)
title(main = "Matriz de Correlaciones - Mimetic Pressure")

corrplot(cor(coerc_press), order = "hclust", tl.col = 'black', tl.cex = 1)
title(main = "Matriz de Correlaciones - Coercitive Pressure")


### test de Bartlett
cortest.bartlett(cor_IE, n=nrow(env_mim_coer))
cortest.bartlett(cor_IE, n=nrow(env_dyna))
cortest.bartlett(cor_IE, n=nrow(mime_press))
cortest.bartlett(cor_IE, n=nrow(coerc_press))

### test KMO
KMO(cor_IE)
KMO(cor_IE_env)
KMO(cor_IE_mime)
KMO(cor_IE_coerc)

### Calculo autovalores
autovalores=eigen(cor(env_mim_coer))
autovalores$values
autovalores_env=eigen(cor(env_dyna))
autovalores_env$values
autovalores_mime=eigen(cor(mime_press))
autovalores_mime$values
autovalores_coerc=eigen(cor(coerc_press))
autovalores_coerc$values

### Calculo de comunalidades
icm=solve(cor_IE)
h2.zero <- round(1 -1/(diag(icm)), 2)
h2.zero
icm=solve(cor_IE_env)
h2.zero <- round(1 -1/(diag(icm)), 2)
h2.zero
icm=solve(cor_IE_mime)
h2.zero <- round(1 -1/(diag(icm)), 2)
h2.zero
icm=solve(cor_IE_coerc)
h2.zero <- round(1 -1/(diag(icm)), 2)
h2.zero

### Número optimo de factores
fa.parallel(env_mim_coer, fa = "pc", n.iter = 100, show.legend = FALSE, main = "PCA conjunto")
fa.parallel(env_dyna, fa = "pc", n.iter = 100, show.legend = FALSE, main = "PCA Environment Dynamism")
fa.parallel(mime_press, fa = "pc", n.iter = 100, show.legend = FALSE, main = "Mimetic Pressure")
fa.parallel(coerc_press, fa = "pc", n.iter = 100, show.legend = FALSE, main = "Coercitive Pressure")

### Factorial
pca <-(principal(env_mim_coer, nfactors=2, rotate="varimax"))$scores
pca_env <-(principal(env_dyna, nfactors=1, rotate="varimax"))$scores
pca_mime <-(principal(mime_press, nfactors=1, rotate="varimax"))$scores
pca_coerc <-(principal(coerc_press, nfactors=1, rotate="varimax"))$scores

## Agrupar variables para clustering
env_mim_coer_pca <- cbind(pca_env, pca_mime, pca_coerc)

#MIXTAS
mixtas <- bd[,c("gender", "expepais", "propipais")]
mixtas <- cbind(mixtas, pca_env, pca_mime, pca_coerc)

######################################## NÚMERO DE CLUSTERES #############################
# DICOTÓMICAS
## k-modes
elbow_dico_method <- fviz_nbclust(gen_exp_prop, FUNcluster=kmodes, method = "wss", k.max = 8) +theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15)) +labs(title = "Elbow method", subtitle = "Optimal number of clusters")
silhouette_dico_method <- fviz_nbclust(gen_exp_prop, FUNcluster=kmodes, method = "silhouette", k.max = 8) +theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15)) +labs(title = "Silhouette method", subtitle = "Optimal number of clusters")

print(elbow_dico_method)
print(silhouette_dico_method)

#CONTINUAS
## k-modes
elbow_conti_kmodes_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=kmodes, method="wss",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters")
silhouette_conti_kmodes_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=kmodes, method="silhouette",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")

print(elbow_conti_kmodes_method)
print(silhouette_conti_kmodes_method)

## k-means
elbow_conti_kmeans_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=kmeans, method="wss",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters")
silhouette_conti_kmeans_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=kmeans, method="silhouette",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_conti_kmeans_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=kmeans, method="gap_stat",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(elbow_conti_kmeans_method)
print(silhouette_conti_kmeans_method)
print(gap_conti_kmeans_method)

## pam
elbow_conti_pam_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=pam, method="wss",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters")
silhouette_conti_pam_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=pam, method="silhouette",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_conti_pam_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=pam, method="gap_stat",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(elbow_conti_pam_method)
print(silhouette_conti_pam_method)
print(gap_conti_pam_method)

## c-means
silhouette_conti_cmeans_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=cmeans, method="silhouette",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_conti_cmeans_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=cmeans, method="gap_stat",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(silhouette_conti_cmeans_method)
print(gap_conti_cmeans_method)

## hclust
elbow_conti_hclust_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=hcut, method="wss",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters")
silhouette_conti_hclust_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=hcut, method="silhouette",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_conti_hclust_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=hcut, method="gap_stat",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(elbow_conti_hclust_method)
print(silhouette_conti_hclust_method)
print(gap_conti_hclust_method)

# MIXTAS
## k-modes
elbow_mixto_kmodes_method <- fviz_nbclust(x=mixtas, FUNcluster=kmodes, method="wss",diss=dist(mixtas, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters") # NO FORMA CODO
silhouette_mixto_kmodes_method <- fviz_nbclust(x=mixtas, FUNcluster=kmodes, method="silhouette",diss=dist(mixtas, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_mixto_kmodes_method <- fviz_nbclust(x=mixtas, FUNcluster=kmodes, method="gap_stat",diss=dist(mixtas, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(elbow_mixto_kmodes_method)
print(silhouette_mixto_kmodes_method)
print(gap_mixto_kmodes_method)

## k-means
elbow_mixto_kmeans_method <- fviz_nbclust(x=mixtas, FUNcluster=kmeans, method="wss",diss=dist(mixtas, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters") # SI HAY CODO
silhouette_mixto_kmeans_method <- fviz_nbclust(x=mixtas, FUNcluster=kmeans, method="silhouette",diss=dist(mixtas, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_mixto_kmeans_method <- fviz_nbclust(x=mixtas, FUNcluster=kmeans, method="gap_stat",diss=dist(mixtas, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(elbow_mixto_kmeans_method)
print(silhouette_mixto_kmeans_method)
print(gap_mixto_kmeans_method)

## pam
elbow_mixto_pam_method <- fviz_nbclust(x=mixtas, FUNcluster=pam, method="wss",diss=dist(mixtas, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters") #SI HAY CODO
silhouette_mixto_pam_method <- fviz_nbclust(x=mixtas, FUNcluster=pam, method="silhouette",diss=dist(mixtas, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_mixto_pam_method <- fviz_nbclust(x=mixtas, FUNcluster=pam, method="gap_stat",diss=dist(mixtas, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(elbow_mixto_pam_method)
print(silhouette_mixto_pam_method)
print(gap_mixto_pam_method)

## c-means
silhouette_mixto_cmeans_method <- fviz_nbclust(x=mixtas, FUNcluster=cmeans, method="silhouette",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_mixto_cmeans_method <- fviz_nbclust(x=mixtas, FUNcluster=cmeans, method="gap_stat",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(silhouette_mixto_cmeans_method)
print(gap_mixto_cmeans_method)

## hclust
elbow_mixto_hclust_method <- fviz_nbclust(x=mixtas, FUNcluster=hcut, method="wss",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters")
silhouette_hclust_pam_method <- fviz_nbclust(x=mixtas, FUNcluster=hcut, method="silhouette",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_conti_hclust_method <- fviz_nbclust(x=mixtas, FUNcluster=hcut, method="gap_stat",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(elbow_conti_hclust_method)
print(silhouette_hclust_pam_method)
print(gap_conti_hclust_method)

####################################### CLUSTERES ##############################

# DICOTÓMICAS
## k-modes
set.seed(100)
kmodes_dico_cluster <- kmodes(gen_exp_prop, 3, iter.max = 8)
kmodes_dico_cluster

bd$kmodes_dico <- kmodes_dico_cluster$cluster
kmodes_dico_1 <- subset(bd, kmodes_dico == 1)
kmodes_dico_2 <- subset(bd, kmodes_dico == 2)
kmodes_dico_3 <- subset(bd, kmodes_dico == 3)

summary(kmodes_dico_1)
summary(kmodes_dico_2)
summary(kmodes_dico_3)

# CONTINUAS
## k-means
set.seed(100)
kmeans_conti_cluster <- kmeans(env_mim_coer_pca, 2, iter.max = 8)
table(kmeans_conti_cluster$cluster)
kmeans_conti_cluster

bd$kmeans_conti <- kmeans_conti_cluster$cluster
kmeans_conti_1 <- subset(bd, kmeans_conti == 1)
kmeans_conti_2 <- subset(bd, kmeans_conti == 2)

summary(kmeans_conti_1)
summary(kmeans_conti_2)

## pam
set.seed(100)
pam_conti_cluster <- pam(env_mim_coer_pca, 2)
table(pam_conti_cluster$cluster)
pam_conti_cluster

bd$pam_conti <- pam_conti_cluster$cluster
pam_conti_1 <- subset(bd, pam_conti == 1)
pam_conti_2 <- subset(bd, pam_conti == 2)

summary(pam_conti_1)
summary(pam_conti_2)

## cmeans
set.seed(100)
cmeans_conti_cluster <- cmeans(env_mim_coer_pca, 2, iter.max = 8)
table(cmeans_conti_cluster$cluster)
cmeans_conti_cluster

bd$cmeans_conti <- cmeans_conti_cluster$cluster
cmeans_conti_1 <- subset(bd, cmeans_conti == 1)
cmeans_conti_2 <- subset(bd, cmeans_conti == 2)

summary(cmeans_conti_1)
summary(cmeans_conti_2)

## ÍNDICES DE VALIDACIÓN C-means
### Fukuyama Sugeno
# Declarar función con parámetros
fukuyama_sugeno_indice <- function(datos, centros, miembro, m = 2) {
  n <- nrow(datos)
  c <- nrow(centros)
  
  # Calcular el centroide global
  centroide_centro <- colMeans(datos)
  # Inicializar sumas para numerador y denominador
  numerador <- 0
  denominador <- 0
  #Calculo del índice Fukuyama
  for (i in 1:n) {
    for (j in 1:c) {
      d_ij <- sqrt(sum((datos[i, ] - centros[j, ])^2))
      numerador <- numerador + (miembro[i, j]^m) * d_ij^2
    }
  }
  for (j in 1:c) {
    d_jG <- sqrt(sum((centros[j, ] - centroide_centro)^2))
    denominador <- denominador + d_jG^2
  }
  indice <- numerador - denominador
  return(indice)
}
optimal_clusters_fukuyama_sugeno <- function(datos, max_clusters = 10) {
  indices <- numeric(max_clusters - 1)
  m <- 2
  #Realiza el cluster cmeans para cada indice
  for (k in 2:max_clusters) {
    clustering_result <- cmeans(datos, centros = k, m = m, iter.max = 100, verbose = FALSE)
    centros <- clustering_result$centros
    miembro <- clustering_result$miembro
    indices[k - 1] <- fukuyama_sugeno_indice(datos, centros, miembro, m)
  }
  #Nº que minimiza el índice
  optimal_clusters <- which.min(indices) + 1 
  #Gráfica de la función
  plot(2:max_clusters, indices, type = "b", pch = 19, frame = FALSE,
       xlab = "Número de Clústeres", ylab = "Fukuyama-Sugeno indice",
       main = "Fukuyama-Sugeno para determinar el número óptimo de clústeres")
  return(optimal_clusters)
}
# Muestra el nº óptimo
optimal_result_fukuyama <- optimal_clusters_fukuyama_sugeno(env_mim_coer_pca)
optimal_result_fukuyama

### Xie-Beni
# Declarar función con parámetros
xie_beni_indice <- function(datos, cmeans_result) {
  U <- cmeans_result$miembro
  centros <- cmeans_result$centros
  N <- nrow(datos)
  K <- ncol(U)
  
  # Cálculo del índice
  numerador <- sum(sapply(1:N, function(i) {
    sum(U[i, ]^2 * colSums((t(centros) - datos[i, ])^2))
  }))
  min_dist <- min(dist(centros, method = "euclidean"))
  denominador <- N * min_dist^2
  numerador / denominador
}

optimal_clusters_xie <- function(datos, max_clusters = 10) {
  xie_beni_vals <- numeric(max_clusters - 1)
  #Realiza el cluster cmeans para cada indice
  for (k in 2:max_clusters) {
    cmeans_result <- cmeans(datos, centros = k, iter.max = 100, verbose = FALSE)
    xie_beni_vals[k - 1] <- xie_beni_indice(datos, cmeans_result)
  }
  
  # Gráfica de la función
  plot(2:max_clusters, xie_beni_vals, type = "b", pch = 19, frame = FALSE,
       xlab = "Número de Clústeres", ylab = "Xie-Beni indice",
       main = "Xie-Beni para determinar el número óptimo de clústeres")
  #Nº que minimiza el índice
  list(xie_beni = which.min(xie_beni_vals) + 1)
}
#Nº que minimiza el índice
optimal_result_xie <- optimal_clusters_xie(env_mim_coer_pca)
optimal_result_xie

### Coeficiente de entropia y partición
# Declarar función con parámetros
entropia_indice <- function(U) {
  -sum(U * log(U + 1e-10)) / nrow(U)
}

particion_coefficient <- function(U) {
  sum(U^2) / nrow(U)
}

optimal_clusters_coefficients <- function(datos, max_clusters = 10) {
  entropia_vals <- numeric(max_clusters - 1)
  particion_vals <- numeric(max_clusters - 1)
  
  for (k in 2:max_clusters) {
    cmeans_result <- cmeans(datos, centros = k, iter.max = 100, verbose = FALSE)
    U <- cmeans_result$miembro
    entropia_vals[k - 1] <- entropia_indice(U)
    particion_vals[k - 1] <- particion_coefficient(U)
  }
  
  # Gráfica de la función Entropía
  plot(2:max_clusters, entropia_vals, type = "b", pch = 19, frame = FALSE,
       xlab = "Número de Clústeres", ylab = "Coeficiente de Entropía",
       main = "Coeficiente de Entropía para determinar el número óptimo de clústeres")
  
  # Gráfica de la función Coeficiente de Partición
  plot(2:max_clusters, particion_vals, type = "b", pch = 19, frame = FALSE,
       xlab = "Número de Clústeres", ylab = "Coeficiente de Partición",
       main = "Coeficiente de Partición para determinar el número óptimo de clústeres")
  
  list(entropia = which.min(entropia_vals) + 1, particion = which.max(particion_vals) + 1)
}
#Nº que devuelve el índice
optimal_result_coefficients <- optimal_clusters_coefficients(env_mim_coer_pca)
optimal_result_coefficients


## jerárquico
set.seed(100)
hcut_conti_cluster <- hcut(env_mim_coer_pca, 2, iter.max = 8)
table(hcut_conti_cluster$cluster)
hcut_conti_cluster

bd$hcut_conti <- hcut_conti_cluster$cluster
hcut_conti_1 <- subset(bd, hcut_conti == 1)
hcut_conti_2 <- subset(bd, hcut_conti == 2)
hcut_conti_3 <- subset(bd, hcut_conti == 3)

summary(hcut_conti_1)
summary(hcut_conti_2)
summary(hcut_conti_3)

# MIXTO
## k-means
set.seed(100)
kmeans_mixto_cluster <- kmeans(mixtas, 2, iter.max = 8)
table(kmeans_mixto_cluster$cluster)
kmeans_mixto_cluster

bd$kmeans_mixto <- kmeans_mixto_cluster$cluster
kmeans_mixto_1 <- subset(bd, kmeans_mixto == 1)
kmeans_mixto_2 <- subset(bd, kmeans_mixto == 2)

summary(kmeans_mixto_1)
summary(kmeans_mixto_2)

## pam
set.seed(100)
pam_mixto_cluster <- pam(mixtas, 2)
table(pam_mixto_cluster$cluster)
pam_mixto_cluster

bd$pam_mixto <- pam_mixto_cluster$cluster
pam_mixto_1 <- subset(bd, pam_mixto == 1)
pam_mixto_2 <- subset(bd, pam_mixto == 2)

summary(pam_mixto_1)
summary(pam_mixto_2)

## jerárquico
set.seed(100)
hcut_mixto_cluster <- hcut(mixtas, 2, iter.max = 8)
table(hcut_mixto_cluster$cluster)
hcut_mixto_cluster

bd$hcut_mixto <- hcut_mixto_cluster$cluster
hcut_mixto_1 <- subset(bd, hcut_mixto == 1)
hcut_mixto_2 <- subset(bd, hcut_mixto == 2)

summary(hcut_mixto_1)
summary(hcut_mixto_2)
############################# GRÁFICOS DE LOS CLÚSTERES ###################################

# DICOTÓMICAS
## K-modes

fviz_cluster(list(data = gen_exp_prop, cluster = kmodes_dico_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando K-modes",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

# CONTINUAS
## K-modes
fviz_cluster(list(data = env_mim_coer_pca, cluster = kmodes_conti_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando K-Modes",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

## K-means
fviz_cluster(list(data = env_mim_coer_pca, cluster = kmeans_conti_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando K-Means",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

## Pam
fviz_cluster(list(data = env_mim_coer_pca, cluster = pam_conti_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando PAM",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

## C-Means
fviz_cluster(list(data = env_mim_coer_pca, cluster = cmeans_conti_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando C-Means",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

# MIXTAS
## K-means
fviz_cluster(list(data = mixtas, cluster = kmeans_mixto_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando K-Means",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

## Pam
fviz_cluster(list(data = mixtas, cluster = pam_mixto_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando PAM",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

## Clústeres jerárquicos
# continuas
set.seed(100)
dendo_hclust_conti <- hclust(dist(env_mim_coer), method = "ward.D")
clusters_conti <- cutree(dendo_hclust_conti, k = 2)
dend_conti_colored <- color_branches(as.dendrogram(dendo_hclust_conti), k = 2)
plot(dend_conti_colored, main = "Dendograma de variables continuas", xlab = "Índice de Muestra", ylab = "Distancia", axes = FALSE)

# mixtas
set.seed(100)
dendo_hclust_mixto <- hclust(dist(mixtas), method = "ward.D")
clusters_mixto <- cutree(dendo_hclust_mixto, k = 2)
dend_mixto_colored <- color_branches(as.dendrogram(dendo_hclust_mixto), k = 2)
plot(dend_mixto_colored, main = "Dendograma de variables mixtas", xlab = "Índice de Muestra", ylab = "Distancia", axes = FALSE)

####################################### ECUACIONES ESTRUCTURALES ###############

#Definimos el modelo y las variables
modeloDef <- "
#Measurement models
GCompAdvantage =~ v84 + v85 + v86 + v87 + v88 
GDynamiCapa =~ v79 + v80 + v81 + v82 + v83
GPerformance =~ v45 + v46 + v47 + v48

#Structural model
GCompAdvantage ~ GDynamiCapa
GPerformance ~ GDynamiCapa + GCompAdvantage
"

#Análisis de ecuaciones estructurales de covarianza
modelo <- csem(.data = bd, .model = modeloDef, 
                    .disattenuate = FALSE, .resample_method = "bootstrap", .R=10000)

#Verificar los resultados del análisis 
verify(modelo)

#Calidad del modelo mediante rmsea
assess(modelo, "rmsea")
assess(modelo)
#Calcula el tamaño del efecto f2
calculatef2(modelo)
#Predecir modelo
predict(modelo)
prediction<-predict(modelo)
write.xlsx(prediction$Prediction_metrics, "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\MAEyRMSE.xlsx")
#Pruebas de inferencia: prueba hipotesis e intervalos
infer(modelo)
#Resumen del modelo: indices, coeficientes
summarize(modelo)
resumen_datos<-summarize(modelo)
View(resumen_datos)

resumen_datos$Estimates$Path_estimates
write.xlsx(resumen_datos$Estimates$Path_estimates,"C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\path.csv")
#Las cargas de los indicadores
resumen_datos$Estimates$Loading_estimates
write.xlsx(resumen_datos$Estimates$Loading_estimates,"C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\loading.csv")

#Casi toda la información
prueba<-assess(modelo)
exportToExcel(.postestimation_object = assess(modelo),
              .filename = "muchosValores.xlsx", .path = "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\")

exportToExcel(.postestimation_object = predict(modelo), .filename = "predict.xlsx", .path = "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\")

#modelo lineal
prediction<-predict(modelo, .benchmark = c("lm"))
write.xlsx(prediction$Prediction_metrics, "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\MAEyRMSE.xlsx")
#Calcular el HTMT
valoresHTMT <- calculateHTMT(modelo)
valoresHTMT$htmts
write.csv(valoresHTMT$htmts, "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\valoresHTMT.csv")

#calcular fornell
valorFornell <- calculateFLCriterion(modelo)
valorFornell

######################### ANÁLISIS MULTIGRUPO ##################################

# DICOTÓMICAS
## k-modes
#Estimar el modelo para dos grupo
modelo_kmodes_dico <- csem(.data = bd, .model = modeloDef, 
                .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                .resample_method = "bootstrap", .R=10000, .id='kmodes_dico')
verify(modelo_kmodes_dico)

#calcular MICOM
micom <- testMICOM(modelo_kmodes_dico)
print(micom)

#Realizar comparaciones para el modelo inicial
outoverall=testMGD(.object = modelo_kmodes_dico,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')

# Comparar el modelo pero con permutaciones
outoverall1=testMGD(.object = modelo_kmodes_dico,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')

print(outoverall1,.approach_mgd = 'Chin')

# Comprobar una relación en concreta del modelo
outMGA=testMGD(.object = modelo_kmodes_dico,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

# Ajustar el modelo multigrupo
ajuste_kmodes_dico_multigrupo <- cfa(modeloDef, data = bd, group = "kmodes_dico")

# Evaluar la invarianza configuracional
summary(ajuste_kmodes_dico_multigrupo, fit.measures = TRUE)

# CONTINUAS

## k-means
#Estimar el modelo para dos grupo
modelo_kmeans_conti <- csem(.data = bd, .model = modeloDef, 
                           .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                           .resample_method = "bootstrap", .R=10000, .id='kmeans_conti')
verify(modelo_kmeans_conti)

#calcular MICOM
micom <- testMICOM(modelo_kmeans_conti)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_kmeans_conti,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_kmeans_conti,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_kmeans_conti,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

# Ajustar el modelo multigrupo
ajuste_kmeans_conti_multigrupo <- cfa(modeloDef, data = bd, group = "kmeans_conti")

# Evaluar la invarianza configuracional
summary(ajuste_kmeans_conti_multigrupo, fit.measures = TRUE)

## PAM
#Estimar el modelo para dos grupo
modelo_pam_conti <- csem(.data = bd, .model = modeloDef, 
                           .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                           .resample_method = "bootstrap", .R=10000, .id='pam_conti')
verify(modelo_pam_conti)

#calcular MICOM
micom <- testMICOM(modelo_pam_conti)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_pam_conti,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_pam_conti,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_pam_conti,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

# Ajustar el modelo multigrupo
ajuste_pam_conti_multigrupo <- cfa(modeloDef, data = bd, group = "pam_conti")

# Evaluar la invarianza configuracional
summary(ajuste_pam_conti_multigrupo, fit.measures = TRUE)

## C-Means
#Estimar el modelo para dos grupo
modelo_cmeans_conti <- csem(.data = bd, .model = modeloDef, 
                         .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                         .resample_method = "bootstrap", .R=10000, .id='cmeans_conti')
verify(modelo_cmeans_conti)

#calcular MICOM
micom <- testMICOM(modelo_cmeans_conti)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_cmeans_conti,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_cmeans_conti,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_cmeans_conti,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

# Ajustar el modelo multigrupo
ajuste_cmeans_conti_multigrupo <- cfa(modeloDef, data = bd, group = "cmeans_conti")

# Evaluar la invarianza configuracional
summary(ajuste_cmeans_conti_multigrupo, fit.measures = TRUE)

## Jerárquico
#Estimar el modelo para dos grupo
modelo_hcut_conti <- csem(.data = bd, .model = modeloDef, 
                         .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                         .resample_method = "bootstrap", .R=10000, .id='hcut_conti')
verify(modelo_hcut_conti)

#calcular MICOM
micom <- testMICOM(modelo_hcut_conti)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_hcut_conti,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_hcut_conti,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_hcut_conti,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

# Ajustar el modelo multigrupo
ajuste_hcut_conti_multigrupo <- cfa(modeloDef, data = bd, group = "hcut_conti")

# Evaluar la invarianza configuracional
summary(ajuste_hcut_conti_multigrupo, fit.measures = TRUE)

# MIXTAS

## K-means
#Estimar el modelo para dos grupo
modelo_kmeans_mixto <- csem(.data = bd, .model = modeloDef, 
                         .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                         .resample_method = "bootstrap", .R=10000, .id='kmeans_mixto')
verify(modelo_kmeans_mixto)

#calcular MICOM
micom <- testMICOM(modelo_kmeans_mixto)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_kmeans_mixto,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_kmeans_mixto,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_kmeans_mixto,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

# Ajustar el modelo multigrupo
ajuste_kmeans_mixto_multigrupo <- cfa(modeloDef, data = bd, group = "kmeans_mixto")

# Evaluar la invarianza configuracional
summary(ajuste_kmeans_mixto_multigrupo, fit.measures = TRUE)

## PAM
#Estimar el modelo para dos grupo
modelo_pam_mixto <- csem(.data = bd, .model = modeloDef, 
                         .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                         .resample_method = "bootstrap", .R=10000, .id='pam_mixto')
verify(modelo_pam_mixto)

#calcular MICOM
micom <- testMICOM(modelo_pam_mixto)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_pam_mixto,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_pam_mixto,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_pam_mixto,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

# Ajustar el modelo multigrupo
ajuste_pam_mixto_multigrupo <- cfa(modeloDef, data = bd, group = "pam_mixto")

# Evaluar la invarianza configuracional
summary(ajuste_pam_mixto_multigrupo, fit.measures = TRUE)

## Jerárquico
#Estimar el modelo para dos grupo
modelo_hcut_mixto <- csem(.data = bd, .model = modeloDef, 
                         .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                         .resample_method = "bootstrap", .R=10000, .id='hcut_mixto')
verify(modelo_hcut_mixto)

#calcular MICOM
micom <- testMICOM(modelo_hcut_mixto)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_hcut_mixto,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_hcut_mixto,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_hcut_mixto,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

# Ajustar el modelo multigrupo
ajuste_hcut_mixto_multigrupo <- cfa(modeloDef, data = bd, group = "hcut_mixto")

# Evaluar la invarianza configuracional
summary(ajuste_hcut_mixto_multigrupo, fit.measures = TRUE)
