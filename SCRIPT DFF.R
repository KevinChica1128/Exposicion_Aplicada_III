#Kevin Steven García - 1533173
#Alejandro Vargas - 1525953
#Alejandro Soto - 1532457
#Diseños factoriales fraccionados

##Datos diseño para ejemplo
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
mod4<-lm(Respuesta~FA+FB+FC+FA:FB+FA:FC+FB:FC+FA:FB:FC, data=datos4)
anova(mod4)

#fraccionado
#a mano seria tomar las que muestran signo positivo en la interaccion de todas las velocidades
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
colnames(nueva)=c("H","J","K","O")



#MODELO FACTORIAL
mod6<-lm(O~H+J+K, data=nueva)
anova(mod6)

#grafico de efectos principales
x11()
Efectos <- data.frame(H,J,K,O)
plot.design(Efectos, fun="mean", main=" Gráfica de efectos principales", ylab= "Vida Util", xlab="Factor")


#Diseño factorial completo con una sola replica.
Respuesta1<-c(4,4,20,4,7,2,10,14)
#Se arma la tabla de diseño
library(FrF2)
Tabla <- FrF2(nruns = 8, 
              nfactors = 3, 
              factor.names = list(A=c("-","+"), 
                                  B=c("-","+"), 
                                  C=c("-","+")),
              replications = 1, randomize = F)
Tabla <- add.response(design = Tabla, response = Respuesta1)
Tabla
