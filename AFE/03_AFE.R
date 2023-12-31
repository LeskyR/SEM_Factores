
# "An�lisis factorial exploratorio"

#El an�lisis factorial exploratorio se emplea como una t�cnica exploratoria o descriptiva para determinar el n�mero adecuado de factores comunes y descubrir cu�les variables de medici�n son indicadores razonables de las diversas dimensiones latentes
  
  
#Instalaci�n de paquetes y actualizaciones disponibles
  
  devtools::install_github("simsem/semTools/semTools")

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

#verificando nombres y n�mero de columnas
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

#matriz de correlaci�n policorica y gr�fico de la mitad de abajo de la matriz
mat_cor <-cor(bfi_s, use="pairwise.complete.obs") 
ggcorrplot(mat_cor,type="lower",hc.order = T) 


#Antes de continuar con el an�lisis se debe confirmar que la muestra es v�lida para realizar el an�lisis factorial. Uno de los test que ayudan a validar los datos es el test de adecuaci�n muestral de Kaiser-Meyer-Olkin (KMO), y la prueba de Esfericidad de Bartlett. 

#prueba de Esfericidad de Bartlett
cortest.bartlett(mat_cor)->p_esf1
p_esf1$p

#Kaiser-Meyer-Olkin (KMO)
KMO(mat_cor)  

#Determinar el n�mero de factores
scree(mat_cor)
fa.parallel(mat_cor,n.obs=34,fa="fa",fm="minres")


#modelo de an�lisis factorial exploratorio, estimaci�n por m�xima verosimilitud
modelo3<-fa(mat_cor,
            nfactors = 3,
            rotate = "varimax",
            fm="mle") 

#muestra el diagrama            
fa.diagram(modelo3,sort=TRUE,
           cut=.3,
           simple=TRUE,
           digits=1)

#muestra la informaci�n de los factores
print(modelo3$loadings,cut=0)

#muestra la informaci�n adicional de los factores 
print(modelo3,cut=0,digits=3) 
