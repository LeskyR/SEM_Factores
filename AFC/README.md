```
Nombre: Análisis factorial confirmatorio

Publicado en: 'Modelos Estadísticos de Análisis de Factores que Afectan el Rendimiento Escolar'
Portal UNAH/MM:	https://mm.unah.edu.hn/descargar-tesis/

Descripción: 'En el análisis factorial confirmatorio (AFC), el investigador especifica el número de
	      factores y el patrón de relación entre el indicador y las cargas factoriales de antemano,
	      así como otros parámetros.  Además este análisis utiliza diagramas de ruta para representar 
	      factores y variables.'

Palabras clave: 'Análisis factorial confirmatorio, medidas de bondad de ajuste, coeficientes de confiabilidad.' 

Autora: Lesky Rivas

Cargar: 

	Datos: TerceroEP

	Resultado esperado: 'un diagrama que muestra cómo un conjunto de variables explicativas influye en
	la variable dependiente bajo consideración.'
```

```ruby
# "Análisis factorial confirmatorio"
 
#Los modelos de análisis factorial confirmatorio son un subconjunto de un enfoque más general para modelar variables latentes conocido como modelo de ecuación estructural o modelo de estructura de covarianza. Dichos modelos permiten tanto la respuesta como las variables latentes explicativas vinculadas por una serie de ecuaciones lineales.
  
#Instalación de paquetes y actualizaciones disponibles
  
  devtools::install_github("simsem/semTools/semTools")

install.packages('lavaan')
library(lavaan)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("parameters","apa","haven","ggplot2","ggpubr","gridExtra","apaTables", "reshape", "GPArotation", "mvtnorm", "psych", "psycho", "psychometric", "lavaan", "nFactors", "semPlot", "lavaan", "MVN", "semTools","polycor","ggcorrplot", "MVN", "car", "moments")
ipak(packages)

#cargando los datos
saveRDS(TerceroEP, file = "TerceroEP")
Data1<-readRDS(file="TerceroEP")

#guardando los datos
saveRDS(Data1, file = "Data1")

#verificando nombres y número de columnas
names(Data1)
ncol(Data1)

#cambiando nombre de las variables para mejor manejo de las mismas
names(Data1) = c("B1",	"B2", "B3","B4","A1","A2","A3","A4","A5","A6","A7","A8","A9",
                 "A10","A11","A12", "A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23",
                 "A24","A25","A26","A27","A28","A29","A30","A31","A32","A33","PL")

#verificando los nuevos nombres de las variables
names(Data1)

# subconjunto de datos 
bfi_s <- Data1[3:37] 
ncol(bfi_s)

#matriz de correlación policorica y gráfico de la mitad de abajo de la matriz
mat_cor <-cor(bfi_s, use="pairwise.complete.obs") 
ggcorrplot(mat_cor,type="lower",hc.order = T) 

#verificar si los datos siguen una distribución normal multivariada para ello se utilizó el test de Mardia
NM<-mvn(bfi_s ,  mvnTest = ("mardia"), desc = TRUE) 
NM$multivariateNormality


#Partiendo del resultado del análisis factorial, se inicia la modelación con análisis factorial confirmatorio


#Se crea un objeto "model3FC" donde se almacena la información con los tres factores resultantes en AFE y las varibles observables correspondientes.

modelo3FC <-  "
              entornoFYC  =~ A5 + A6 +A7 +A8 +A9 +A10 +A11 +A12 +A13 +A14 +A15
              gestion   =~ A17  +A19 +A20 +A21  +A18
              desempeno =~ A28  +A30 +A31 +A32 +A33
              "
# Almacenamos en el objeto "fit3FC" la información de la estimación

fit3FC <- cfa(model = modelo3FC,
              sample.cov = mat_cor,
              sample.nobs = 3647)
summary(fit3FC, fit.measures = TRUE, standardized = TRUE)

#muestra el gráfico
semPaths(fit3FC, intercepts = FALSE,edge.label.cex=0.8, optimizeLatRes = TRUE, 
         groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=1,sizeLat = 6,"std")

# Solicitar los 10 primeros IM con valores más altos para "reespecificar" el modelo
modindices(fit3FC, sort = TRUE, maximum.number = 10)        

#Procederemos a "reespecificar" creando un objeto con nombre "model_04"
modelo_04FC <-  "
              entornoFYC  =~ A5 + A6 +A7 +A8 +A9 +A10 +A11 +A12 +A13 +A14 +A15
              gestion   =~  A17  +A19 +A20  +A21
              desempeno =~ A28 +A29 +A30 +A31 +A32 +A33
              # Correlación de errores              
              A14 ~~ A15
              A31 ~~ A33
              A11 ~~ A12
              A5 ~~  A6
              A30 ~~ A32
              A7 ~~  A15
              A7 ~~ A12
              A8 ~~ A15
            "         

fit_04FC <- cfa(model = modelo_04FC,
                sample.cov = mat_cor,
                sample.nobs = 3647)
summary(fit_04FC, fit.measures = TRUE, standardized = TRUE)

semPaths(fit_04FC, intercepts = FALSE,edge.label.cex=0.8, optimizeLatRes = TRUE, 
         groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=1,sizeLat = 6,"std")

#Medidas de bondad de ajuste
fitmeasures(fit_04FC)

#Coeficientes de confiabilidad Alfa de Cronbach (Alpha)y Varianza Extraída Media (AVE)
semTools::reliability(fit_04FC)
```
![IMG1](https://github.com/LeskyR/SEM_Factores/blob/main/AFC/AFC11.jpg)
