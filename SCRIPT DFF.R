#Kevin Steven García - 1533173
#Alejandro Vargas - 1525953
#Alejandro Soto - 1532457
#Diseños factoriales fraccionados

##Datos diseño para ejemplo 1
FA=c(rep("0",8),rep("1",8))
FC=c(rep(c("0","0","1","1"),4))
FB=c(rep(c(rep("0",4),rep("1",4)),2))
Respuesta=c(5,4,7,9,20,14,10,6,4,11,2,7,4,6,14,16)
datos4=data.frame(FA,FB,FC,Respuesta)
str(datos4)

#grafico de efectos principales
x11()
Efectos <- data.frame(FA,FB,FC,Respuesta)
plot.design(Efectos, fun="mean", main=" Gráfica de efectos principales", ylab= "Respuesta", xlab="Factor")

#MODELO FACTORIAL
mod<-lm(Respuesta~FA+FB+FC+FA:FB+FA:FC+FB:FC+FA:FB:FC, data=datos4)
anova(mod)

#fraccionado
#A mano seria tomar las que muestran signo positivo en la interaccion de todas las velocidades
#antes de hacer el experimento
library("AlgDesign")
levels.design = c(2,2,2) 
f.design <- gen.factorial(levels.design) 

str(f.design)

#vector con los signos de la interaccion ABC
tr=c(f.design$X1*f.design$X2*f.design$X3)

#cuando se tienen replicas
fr=c()
for (i in 1:8) {
  fr=c(fr,rep(tr[i],2))
}

#extraccion de los datos matriz fraccionada
nueva=datos4
j=c()
for (i in 1:length(fr)) {
  if(fr[i]!=1){
    j=c(j,i)
  }
  nueva=datos4[-j,]
}
colnames(nueva)=c("A","B","C","O")



#MODELO FACTORIAL
mod1<-lm(O~A+B+C, data=nueva)
anova(mod1)

#Ejemplo2
#Diseño factorial completo con una sola replica.
#Ingresamos los datos
A<-c(rep(c("-1","1"),8))
B<-c(rep(c("-1","-1","1","1"),4))
C<-c(rep(c(rep("-1",4),rep("1",4)),2))
D<-c(rep("-1",8),rep("1",8))
filtracion<-c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)
datos2=data.frame(A,B,C,D,filtracion)
str(datos2)
head(datos2)

library(FrF2)
Tabla <- FrF2(nruns = 16, 
              nfactors = 4, 
              factor.names = list(A=c("-","+"), 
                                  B=c("-","+"), 
                                  C=c("-","+"),
                                  D=c("-","+")),
              replications = 1, randomize = F)
Tabla <- add.response(design = Tabla, response = filtracion)
Tabla
#Análisis método de Daniel
#Gráfico manual
Efectos0<-c(21.625,3.125,9.875,14.625,0.125,-18.125,16.625,2.375,-0.375,
            -1.125,1.875,4.125,-1.625,-2.625,1.375)
x11()
qqnorm(Efectos0)
qqline(Efectos0)

#Gráfico R
x11()
DanielPlot(Tabla)

mod2<-lm(filtracion~A+C+D+A:C+A:D+C:D+A:C:D, data=datos2)
anova(mod2)


#Gráfico efectos principales
MEPlot(Tabla, lwd = 2)
abline(h=0, col="red")

## Gráficas de Interacciones
IAPlot(Tabla, lwd = 2)

# Gráfica de interacción triple
filtracion1<-filtracion[1:8]
A1<-A[1:8]
B1<-B[1:8]
C1<-C[1:8]
filtracion2<-filtracion[9:16]
A2<-A[9:16]
B2<-B[9:16]
C2<-C[9:16]
x11()
par(mfrow=c(1,2))
cubePlot(obj = filtracion1, 
         eff1 = A1, 
         eff2 = B1, 
         eff3 = C1, 
         main = " Gráfica de interacción triple con factor D bajo")
cubePlot(obj = filtracion2, 
         eff1 = A2, 
         eff2 = B2, 
         eff3 = C2, 
         main = " Gráfica de interacción triple con factor D alto",round = -1)

#Fraccionado con una sola replica
#a mano seria tomar las que muestran signo positivo en la interaccion de todas las velocidades
#antes de hacer el experimento
library("AlgDesign")
levels.design1 = c(2,2,2,2) 
f.design1 <- gen.factorial(levels.design1) 
str(f.design1)

#vector con los signos de la interaccion ABC
tr1=c(f.design1$X1*f.design1$X2*f.design1$X3*f.design1$X4)

#extraccion de los datos matriz fraccionada
j=c()
for (i in 1:length(tr1)) {
  if(tr1[i]!=1){
    j=c(j,i)
  }
}
nueva1=datos2[-j,]
colnames(nueva1)=c("A","B","C","D","O")

Efectos<-c(19,1.5,14,16.5,-1,-18.5,19)
x11()
qqnorm(Efectos)
qqline(Efectos)
text(c(0.7,-0.3,-0.07,0.42,-0.68,-1.28,1.27),Efectos,c("A","B","C","D","AB","AC","AD"))

mod3<-lm(O~A*C, data=nueva1)
anova(mod3)
summary(mod3)