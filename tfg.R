library(readxl) #Libreria para leer base de datos
library(openxlsx)
library(stats) #Libreria necesaria parar cSEM
library(cSEM) #Libreria para ec estructurales
library(ggplot2) #Libreria necesaria para la librería factoextra
library(MASS) #Libreria necesaria para factoextra
library(factoextra) #libreria necesaria para calcular número de clústeres
library(klaR) #libreria necesaria para el kmodes
library(e1071) #libreria para cmeans
library(cluster) #libreria para pam
library(corrplot)#gráfico correlaciones
library(psych) #test de Barlett
library (psy)
library(ppclust) #libreria para fukuyama, xiebeni,etc

######################################## DATOS #################################
#Cargamos los datos
bd<- read_excel("C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\DATOS\\BD.xlsx")

#Analizar  datos
str(bd)
summary(bd)

#Agrupación variables según tipo
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
corrplot(cor(env_mim_coer), order = "hclust", tl.col='black', tl.cex=1)
corrplot(cor(env_dyna), order = "hclust", tl.col='black', tl.cex=1)
corrplot(cor(mime_press), order = "hclust", tl.col='black', tl.cex=1)
corrplot(cor(coerc_press), order = "hclust", tl.col='black', tl.cex=1)

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
elbow_dico_method <- fviz_nbclust(x=gen_exp_prop, FUNcluster=kmodes, method="wss",
                                  k.max=8, diss=dist(gen_exp_prop,method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters")
silhouette_dico_method <- fviz_nbclust(x=gen_exp_prop, FUNcluster=kmodes, method="silhouette", 
                                       k.max=8, diss=dist(gen_exp_prop,method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_dico_method <- fviz_nbclust(x= gen_exp_prop, FUNcluster=kmodes, method="gap_stat", 
                                k.max=8, diss=dist(gen_exp_prop,method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(elbow_dico_method)
print(silhouette_dico_method)
print(gap_dico_method)

#CONTINUAS
## k-modes
elbow_conti_kmodes_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=kmodes, method="wss",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters")
silhouette_conti_kmodes_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=kmodes, method="silhouette",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Silhouette method", subtitle = "Optimal number of clusters")
gap_conti_kmodes_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=kmodes, method="gap_stat",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Gap method", subtitle = "Optimal number of clusters")

print(elbow_conti_kmodes_method)
print(silhouette_conti_kmodes_method)
print(gap_conti_kmodes_method)

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
####################elbow_conti_cmeans_method <- fviz_nbclust(x=env_mim_coer, FUNcluster=cmeans, method="wss",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters")  # Da error (Explicación por Whatsapp)
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
####################elbow_mixto_cmeans_method <- fviz_nbclust(x=mixtas, FUNcluster=cmeans, method="wss",diss=dist(env_mim_coer, method="euclidean"))+theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), title = element_text(size = 15))+labs(title = "Elbow method", subtitle = "Optimal number of clusters")  # Da error (Explicación por Whatsapp)
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
kmodes_dico_cluster <- kmodes(gen_exp_prop, 3, iter.max = 8)
kmodes_dico_cluster

##No haría falta lo de abajo porque se obtiene arriba
bd$kmodes_dico <- kmodes_dico_cluster$cluster
kmodes_dico_1 <- subset(bd, kmodes_dico == 1)
kmodes_dico_2 <- subset(bd, kmodes_dico == 2)
kmodes_dico_3 <- subset(bd, kmodes_dico == 3)

summary(kmodes_dico_1)
summary(kmodes_dico_2)
summary(kmodes_dico_3)

# CONTINUAS
## k-modes
kmodes_conti_cluster <- kmodes(env_mim_coer_pca, 2, iter.max = 8)
kmodes_conti_cluster

bd$kmodes_conti <- kmodes_conti_cluster$cluster
kmodes_conti_1 <- subset(bd, kmodes_conti == 1)
kmodes_conti_2 <- subset(bd, kmodes_conti == 2)

## k-means
kmeans_conti_cluster <- kmeans(env_mim_coer_pca, 2, iter.max = 8)
table(kmeans_conti_cluster$cluster)
kmeans_conti_cluster

bd$kmeans_conti <- kmeans_conti_cluster$cluster
kmeans_conti_1 <- subset(bd, kmeans_conti == 1)
kmeans_conti_2 <- subset(bd, kmeans_conti == 2)
kmeans_conti_3 <- subset(bd, kmeans_conti == 3)

## pam
pam_conti_cluster <- pam(env_mim_coer_pca, 2)
table(pam_conti_cluster$cluster)
pam_conti_cluster

bd$pam_conti <- pam_conti_cluster$cluster
pam_conti_1 <- subset(bd, pam_conti == 1)
pam_conti_2 <- subset(bd, pam_conti == 2)

## cmeans
cmeans_conti_cluster <- cmeans(env_mim_coer_pca, 2, iter.max = 8)
table(cmeans_conti_cluster$cluster)
cmeans_conti_cluster

bd$cmeans_conti <- cmeans_conti_cluster$cluster
cmeans_conti_1 <- subset(bd, cmeans_conti == 1)
cmeans_conti_2 <- subset(bd, cmeans_conti == 2)

## Índices de validación para C-means
fuzzy_index <- c(1.1,1.2,1.4,1.6,1.8,2)

xb_conti <- c()
fs_conti <- c()
pc_conti <- c()
pe_conti <- c()

for (j in 1:length(fuzzy_index)) {
  model <- cmeans(env_mim_coer,2,FALSE,iter.max = 100,dist = "euclidean",method = "cmeans",fuzzy_index[j])
  xb_conti <- c(xb_conti,fclustIndex(model,env_mim_coer,"xie.beni"))
  fs_conti <- c(fs_conti,fclustIndex(model,env_mim_coer,"fukuyama.sugeno"))
  pc_conti <- c(pc_conti,fclustIndex(model,env_mim_coer,"partition.coefficient"))
  pe_conti <- c(pe_conti,fclustIndex(model,env_mim_coer,"partition.entropy"))
  
}
print(xb_conti)
print(fs_conti)
print(pc_conti)
print(pe_conti)

## jerárquico
hcut_conti_cluster <- hcut(env_mim_coer_pca, 3, iter.max = 8)
table(hcut_conti_cluster$cluster)
hcut_conti_cluster

bd$hcut_conti <- hcut_conti_cluster$cluster
hcut_conti_1 <- subset(bd, hcut_conti == 1)
hcut_conti_2 <- subset(bd, hcut_conti == 2)
hcut_conti_3 <- subset(bd, hcut_conti == 3)

# MIXTO
## k-modes
kmodes_mixto_cluster <- kmodes(mixtas, 3, iter.max = 8)
table(kmodes_mixto_cluster$cluster)
kmodes_mixto_cluster

bd$kmodes_mixto <- kmodes_mixto_cluster$cluster
kmodes_mixto_1 <- subset(bd, kmodes_mixto == 1)
kmodes_mixto_2 <- subset(bd, kmodes_mixto == 2)
kmodes_mixto_3 <- subset(bd, kmodes_mixto == 3)

## k-means
kmeans_mixto_cluster <- kmeans(mixtas, 2, iter.max = 8)
table(kmeans_mixto_cluster$cluster)
kmeans_mixto_cluster

bd$kmeans_mixto <- kmeans_mixto_cluster$cluster
kmeans_mixto_1 <- subset(bd, kmeans_mixto == 1)
kmeans_mixto_2 <- subset(bd, kmeans_mixto == 2)
kmeans_mixto_3 <- subset(bd, kmeans_mixto == 3)

## pam mixto
pam_mixto_cluster <- pam(mixtas, 3)
table(pam_mixto_cluster$cluster)
pam_mixto_cluster

bd$pam_mixto <- pam_mixto_cluster$cluster
pam_mixto_1 <- subset(bd, pam_mixto == 1)
pam_mixto_2 <- subset(bd, pam_mixto == 2)
pam_mixto_3 <- subset(bd, pam_mixto == 3)

## c-means
cmeans_mixto_cluster <- cmeans(mixtas, 3, iter.max = 8)
table(cmeans_mixto_cluster$cluster)
cmeans_mixto_cluster

bd$cmeans_mixto <- cmeans_mixto_cluster$cluster
cmeans_mixto_1 <- subset(bd, cmeans_mixto == 1)
cmeans_mixto_2 <- subset(bd, cmeans_mixto == 2)
cmeans_mixto_3 <- subset(bd, cmeans_mixto == 3)

# índices de validación para C-means
fuzzy_index <- c(1.1,1.2,1.4,1.6,1.8,2)

xb_mixto <- c()
fs_mixto <- c()
pc_mixto <- c()
pe_mixto <- c()

for (j in 1:length(fuzzy_index)) {
  model <- cmeans(mixtas,fuzzy_cluster[i],FALSE,iter.max = 100,dist = "euclidean",method = "cmeans",fuzzy_index[j])
  xb_mixto <- c(xb_mixto,fclustIndex(model,mixtas,"xie.beni"))
  fs_mixto <- c(fs_mixto,fclustIndex(model,mixtas,"fukuyama.sugeno"))
  pc_mixto <- c(pc_mixto,fclustIndex(model,mixtas,"partition.coefficient"))
  pe_mixto <- c(pe_mixto,fclustIndex(model,mixtas,"partition.entropy"))
}
print(xb_mixto)
print(fs_mixto)
print(pc_mixto)
print(pe_mixto)

## jerárquico
hcut_mixto_cluster <- hcut(mixtas, 3, iter.max = 8)
table(hcut_mixto_cluster$cluster)
hcut_mixto_cluster

bd$hcut_mixto <- hcut_mixto_cluster$cluster
hcut_mixto_1 <- subset(bd, hcut_mixto == 1)
hcut_mixto_2 <- subset(bd, hcut_mixto == 2)
hcut_mixto_3 <- subset(bd, hcut_mixto == 3)

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
             main = "Visualización de Clústeres usando K-Means",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

## C-Means
fviz_cluster(list(data = env_mim_coer_pca, cluster = cmeans_conti_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando K-Means",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

# MIXTAS
## K-modes
fviz_cluster(list(data = mixtas, cluster = kmodes_mixto_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando K-Modes",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

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

## C-Means
fviz_cluster(list(data = mixtas, cluster = cmeans_mixto_cluster$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             main = "Visualización de Clústeres usando C-Means",
             xlab = "Primera Dimensión",
             ylab = "Segunda Dimensión")

## Clústeres jerárquicos
# continuas
dendo_hclust_conti <- hclust(dist(env_mim_coer), method = "ward.D")
clusters_conti <- cutree(dendo_hclust_conti, k = 2)
dend_conti_colored <- color_branches(as.dendrogram(dendo_hclust_conti), k = 2)
plot(dend_conti_colored, main = "Dendograma de variables continuas", xlab = "Índice de Muestra", ylab = "Distancia", axes = FALSE)

# mixtas
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
assess(modelo)
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

#Realizar comparaciones
outoverall=testMGD(.object = modelo_kmodes_dico,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_kmodes_dico,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_kmodes_dico,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

# CONTINUAS
## k-mode
#Estimar el modelo para dos grupo
modelo_kmodes_conti <- csem(.data = bd, .model = modeloDef, 
                            .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                            .resample_method = "bootstrap", .R=10000, .id='kmodes_conti')
verify(modelo_kmodes_conti)

#calcular MICOM
micom <- testMICOM(modelo_kmodes_conti)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_kmodes_conti,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_kmodes_conti,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_kmodes_conti,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

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

# MIXTAS
## K-modes
#Estimar el modelo para dos grupo
modelo_kmodes_mixto <- csem(.data = bd, .model = modeloDef, 
                            .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                            .resample_method = "bootstrap", .R=10000, .id='kmodes_mixto')
verify(modelo_kmodes_mixto)

#calcular MICOM
micom <- testMICOM(modelo_kmodes_mixto)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_kmodes_mixto,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_kmodes_mixto,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_kmodes_mixto,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

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

## C-means
#Estimar el modelo para dos grupo
modelo_cmeans_mixto <- csem(.data = bd, .model = modeloDef, 
                            .disattenuate = F, .PLS_weight_scheme_inner = 'factorial',
                            .resample_method = "bootstrap", .R=10000, .id='cmeans_mixto')
verify(modelo_cmeans_mixto)

#calcular MICOM
micom <- testMICOM(modelo_cmeans_mixto)
print(micom)

#Realizar comparaciones
outoverall=testMGD(.object = modelo_cmeans_mixto,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = modelo_cmeans_mixto,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = modelo_cmeans_mixto,
               .parameters_to_compare = 'GCompAdvantage ~ GDynamiCapa',   
               .approach_mgd = 'Chin')
outMGA

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
