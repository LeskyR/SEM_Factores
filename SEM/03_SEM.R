
# "Ecuaciones Estructurales"

  
#El modelo de ecuaciones estructurales es una t�cnica estad�stica multivariante, utilizada para especificar fenomenos en funci�n de sus variables causales.
  
#Instalaci�n de paquetes y actualizaciones disponibles
  
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
bfi_sPL <- Data1[3:38] 
ncol(bfi_sPL)

#matriz de correlaci�n policorica y gr�fico de la mitad de abajo de la matriz
mat_corPL <-cor(bfi_sPL, use="pairwise.complete.obs") 
ggcorrplot(mat_corPL,type="lower",hc.order = T)

#Escalando variables
bfi_sPL11 <- apply(bfi_sPL,  2, scale)

#verificar si los datos siguen una distribuci�n normal multivariada 
NM<-mvn(bfi_sPL11,  mvnTest = ("mardia"))
NM$multivariateNormality

#Partiendo del resultado del an�lisis factorial, se inicia la modelaci�n con ecuaciones estruturales

modeloES2PL <- '
              #modelo de medida 
              entornoFYC =~ A5 + A6 +A7 +A8 +A9 +A10 +A11 +A12 +A13 +A14 +A15 
              gestion   =~ A17 +A18 +A19 +A20 +A21 
              desempeno =~ A28 +A30 +A31 +A32 
              
              #modelo estrutural
              PL ~ entornoFYC + gestion + desempeno
              
              # Correlaci�n de errores              
              A14 ~~ A15
              A11 ~~ A12
              A5 ~~  A6
              A30 ~~ A32
              A7 ~~  A15 
              A7 ~~  A12
              A8 ~~ A15
 
              '

fitES2PLdata <- sem(modeloES2PL, 
                    data =  bfi_sPL11)
summary(fitES2PLdata, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)

#muestra el gr�fico
semPaths(fitES2PLdata, intercepts = FALSE,edge.label.cex=0.8, optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=1,sizeLat = 6,"std")

#Medidas de bondad de ajuste
fitmeasures(fitES2PLdata)


#Una variable mediadora es una tercera variable que vincula una causa y un efecto.
#Mediaci�n Simple

modeloMsimple <- 
  '
              #modelo de medida 
              entornoFYC =~ A5 + A6 +A7 +A8 +A9 +A10 +A11 +A12 +A13 +A14 +A15 
              gestion   =~ A17 +A18 +A19 +A20 +A21 
              desempeno =~ A28 +A30 +A31 +A32 
              
              #modelo estrutural
              desempeno ~   entornoFYC 
              gestion ~   entornoFYC + desempeno
              PL ~  gestion 
              
              # Correlaci�n de errores              
              A14 ~~ A15
              A11 ~~ A12
              A5 ~~  A6
              A30 ~~ A32
              A7 ~~  A15
              A7 ~~ A12
              A8 ~~ A15
        '

fitMsimple <- sem(modeloMsimple, 
                  data =  bfi_sPL11) 
summary(fitMsimple, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)
semPaths(fitMsimple, intercepts = FALSE,edge.label.cex=0.8, optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=1,sizeLat = 6,"std")

#Medidas de bondad de ajuste
fitmeasures(modeloMsimple)

#uso de la prueba de Sobel para saber que tan significativa es la mediaci�n de desempe�o en este modelo

modeloMsimple <- 
  '
              #modelo de medida 
              entornoFYC =~ A5 + A6 +A7 +A8 +A9 +A10 +A11 +A12 +A13 +A14 +A15 
              gestion   =~ A17 +A18 +A19 +A20 +A21 
              desempeno =~ A28 +A30 +A31 +A32 
              
              #modelo estrutural
              desempeno ~  a1*entornoFYC 
              gestion ~   a2*entornoFYC + b*desempeno
              PL ~  c*gestion 
              I1:=a1*b
              Total:= I1 +a2
              
              # Correlaci�n de errores              
              A14 ~~ A15
              A11 ~~ A12
              A5 ~~  A6
              A30 ~~ A32
              A7 ~~  A15 
              A7 ~~ A12
              A8 ~~ A15
        '
fitMsimple <- sem(modeloMsimple, 
                  data =  bfi_sPL11) #usando la data 3 a 38 variables

summary(fitMsimple, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)

#metodo de  bootstrapping
set.seed(2019)

modeloMsimpleB <- 
  '
              #modelo de medida 
              entornoFYC =~ A5 + A6 +A7 +A8 +A9 +A10 +A11 +A12 +A13 +A14 +A15 
              gestion   =~ A17 +A18 +A19 +A20 +A21 
              desempeno =~ A28 +A30 +A31 +A32 
              
              #modelo estrutural
              desempeno ~  a1*entornoFYC 
              gestion ~   a2*entornoFYC + b*desempeno
              PL ~  c*gestion 
              I1:=a1*b
              Total:= I1 + a2
              
              # Correlaci�n de errores              
              A14 ~~ A15
              A11 ~~ A12
              A5 ~~  A6
              A30 ~~ A32
              A7 ~~  A15 
              A7 ~~ A12
              A8 ~~ A15
        '

fitMsimpleB <- sem(modeloMsimpleB, data =  bfi_sPL11, se="bootstrap", bootstrap=500) 
parameterEstimates(ffitMsimpleB, ci=TRUE, level=0.95, boot.ci.type="perc")


#Mediaci�n M�ltiple

modeloMmultiple <- 
  '
              #modelo de medida 
              entornoFYC =~ A5 + A6 +A7 +A8 +A9 +A10 +A11 +A12 +A13 +A14 +A15 
              gestion   =~ A17 +A18 +A19 +A20 +A21 
              desempeno =~ A28 +A30 +A31 +A32 
        
              #modelo estrutural
              desempeno ~  entornoFYC 
              gestion ~   entornoFYC 
              gestion ~   desempeno
              PL ~  gestion + entornoFYC 

             # Correlaci�n de errores              
              A14 ~~ A15
              A11 ~~ A12
              A5 ~~  A6
              A30 ~~ A32
              A7 ~~  A15 + A12
              A8 ~~ A15
        '

fitMmultiple <- sem(modeloMmultiple, 
                    data =  bfi_sPL11)

summary(fitMmultiple, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)
semPaths(fitMmultiple, intercepts = FALSE,edge.label.cex=0.8, optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=1,sizeLat = 6,"std")

#las medidas de bondad de ajuste y la prueba de Sobel y el metodo de  bootstrapping, se aplican igual que en la mediaci�n simple.

#validaci�n del modelo, se verific� si este estaba afectado por el m�todo com�n de la varianza

#prueba del factor �nico Harman
subconjunto<-subset(Data1, select=c(A5 , A6 ,A7 ,A8 ,A9 ,A10,A11 ,A12 ,A13 ,A14 ,A15, 
                                    A17 ,A18 ,A19 ,A20 ,A21, 
                                    A28 ,A30 ,A31 ,A32 ))
factorunico<-fa(subconjunto,
                nfactors = 1,
                rotate = "none",
                fm="mle")
factorunico

#htmt: evaluaci�n de la validez discriminante mediante la relaci�n heterorrasgo--monorrasgo
htmt(modeloMmultiple, data =  bfi_sPL)