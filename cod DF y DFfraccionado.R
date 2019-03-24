#Exposicion Diseño factorial fraccionado
#Kevin Garcia
#Alejandro Vargas
#Alejandro Soto

#Datos (Ejemplo mongomery)
Material=c(rep("1",12),rep("2",12),rep("3",12))
Temperatura=c(rep(c(rep("15",4),rep("70",4),rep("125",4)),3))
Vida=c(130,155,75,180,34,40,80,75,20,70,82,58,150,188,159,126,136,122,106,115,25,70,58,45,138,110,168,160,174,120,150,139,96,104,82,60)
datos=data.frame(Material,Temperatura,Vida)
str(datos)
head(datos)

X11()
boxplot(Vida~Material*Temperatura, ylab="Vida Util")
#grafico de efectos principales
x11()
Efectos <- data.frame(Material, Temperatura, Vida)
plot.design(Efectos, fun="mean", ylab= "Vida Util", xlab="Factor")

#Grafico de interacciones
x11()
interaction.plot(Material, Temperatura, Vida)

x11()
interaction.plot( Temperatura,Material, Vida)

#MODELO FACTORIAL
mod<-lm(Vida~Material+Temperatura+Material:Temperatura, data=datos)
anova(mod)

#Como existe interaccion hacemos:
#Pruebas de Comparaciones MÃºltiple (Pruebas Postanova)
library(multcompView)
library(lsmeans)
#Material/Temperatura
leastsquare1 = lsmeans(mod, ~Material|Temperatura,  adjust="tukey")
cld(leastsquare1, alpha=.05, Letters=letters)

#Temperatura/Material
leastsquare2 = lsmeans(mod, ~Temperatura|Material,  adjust="tukey")
cld(leastsquare2, alpha=.05, Letters=letters)

#DISEÑO FACTORIAL FRACCIONADO
install.packages("AlgDesign")
library("AlgDesign") 


levels.design = c(3,3) 
f.design <- gen.factorial(levels.design) 


fract.design <- optFederov(
  data=f.design, 
  nTrials=sum(levels.design), 
  approximate=TRUE) 

#me saca la fraccion del diseño
potDgn=optFederov(mod,data = datos,nTrials = 18)#el trial es el numero de datos que quiero
#en mi fraccion como es 1/2 entonces son 18 datos


#fraccion de diseño
potDgn$design
#MODELO FACTORIAL fraccionado
mod2<-lm(Vida~Material+Temperatura+Material:Temperatura, data=potDgn$design)
anova(mod2)

#Ejemplo 2
A<-c(rep(c("-","+"),8))
B<-c(rep(c("-","-","+","+"),4))
C<-c(rep(c(rep("-",4),rep("+",4)),2))
D<-c(rep("-",8),rep("+",8))
filtracion<-c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)
datos2=data.frame(A,B,C,D,filtracion)
str(datos2)
head(datos2)
mod3<-lm(filtracion~A+C+D+A:C+A:D+C:D+A:C:D, data=datos2)
anova(mod3)

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
DanielPlot(Tabla)

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
