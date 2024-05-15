library(readxl) #Libreria para leer base de datos
library(stats) #Libreria necesaria parar cSEM
library(cSEM) #Libreria para ec estructurales
library(openxlsx)
library(ggplot2) #Libreria necesaria para la librería factoextra
library(MASS) #Libreria necesaria para factoextra
library(factoextra) #libreria necesaria para calcular número de clústeres
library(klaR) #libreria necesaria para el kmodes

######################################## DATOS #################################
#Cargamos los datos
base_datos<- read_excel("C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\DATOS\\BD.xlsx")

#DICOTÓMICAS
gen_exp_prop <- base_datos[,c("gender", "expepais", "propipais")]
#CONTINUAS
env_mim_coer <- base_datos[,c("v97", "v98", "v99", "v39", "v40", "v41", "v42", "v43", "v44")]
#MIXTAS
mixtas <- base_datos[,c("gender", "expepais", "propipais","v97", "v98", "v99", "v39", "v40", "v41", "v42", "v43", "v44")]

######################################## CLUSTERES #############################
#OBTENER NÚMERO DE CLÚSTERES
#DICOTÓMICAS (kmodes)
elbow_method <- fviz_nbclust(x= gen_exp_prop, FUNcluster=kmodes, method="wss", 
                             k.max = 8, diss=dist(gen_exp_prop,method="manhattan"))+theme_classic()
silhouette_method <- fviz_nbclust(x= gen_exp_prop, FUNcluster=kmodes, method="silhouette", 
                                  k.max = 8, diss=dist(gen_exp_prop,method="manhattan"))+theme_classic()
#gap_statistics ???

print(elbow_method)
print(silhouette_method)

#CONTINUAS


#MIXTAS


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
valorSalida <- csem(.data = base_datos, .model = modeloDef, 
                    .disattenuate = FALSE, .resample_method = "bootstrap", .R=10000)

#Verificar los resultados del análisis 
verify(valorSalida)

#Calidad del modelo mediante rmsea
assess(valorSalida, "rmsea")
#Calcula el tamaño del efecto f2
calculatef2(valorSalida)
#Predecir modelo
predict(valorSalida)
prediction<-predict(valorSalida)
write.xlsx(prediction$Prediction_metrics, "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\MAEyRMSE.xlsx")
#Pruebas de inferencia: prueba hipotesis e intervalos
infer(valorSalida)
#Resumen del modelo: indices, coeficientes
summarize(valorSalida)
resumen_datos<-summarize(valorSalida)
View(resumen_datos)

resumen_datos$Estimates$Path_estimates
write.xlsx(resumen_datos$Estimates$Path_estimates,"C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\path.csv")
#Las cargas de los indicadores
resumen_datos$Estimates$Loading_estimates
write.xlsx(resumen_datos$Estimates$Loading_estimates,"C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\loading.csv")

#Casi toda la información
assess(valorSalida)
exportToExcel(.postestimation_object = assess(valorSalida),
              .filename = "muchosValores.xlsx", .path = "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\")

exportToExcel(.postestimation_object = predict(valorSalida), .filename = "predict.xlsx", .path = "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\")

#modelo lineal
prediction<-predict(valorSalida, .benchmark = c("lm"))
write.xlsx(prediction$Prediction_metrics, "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\MAEyRMSE.xlsx")
#Calcular el HTMT
valoresHTMT <- calculateHTMT(valorSalida)
valoresHTMT$htmts
write.csv(valoresHTMT$htmts, "C:\\Users\\henri\\OneDrive\\Escritorio\\universidad\\tfg\\PRUEBAS DE R\\tablas\\valoresHTMT.csv")

#calcular fornell
valorFornell <- calculateFLCriterion(valorSalida)
valorFornell

#calcular MICOM
#micom <- testMICOM(valorSalida)


######################### análisis multigrupo ##################################
outoverall=testMGD(.object = valorSalida,
                   .type_vcv='construct' 
)

outoverall
print(outoverall,.approach_mgd = 'Klesel')
print(outoverall,.approach_mgd = 'Chin')
outoverall1=testMGD(.object = valorSalida,
                    .type_vcv='construct',
                    .R_permutation = 999,
                    .approach_p_adjust = 'bonferroni')
print(outoverall1,.approach_mgd = 'Chin')
outMGA=testMGD(.object = valorSalida,
               .parameters_to_compare = 'OrgIden~OrgPres',   ###Cambiar variables
               .approach_mgd = 'Chin')
outMGA