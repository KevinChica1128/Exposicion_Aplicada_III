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

#Descriptivas
X11()
boxplot(Vida~Material*Temperatura, ylab="Vida Util")
#grafico de efectos principales
x11()
Efectos <- data.frame(Material, Temperatura, Vida)
plot.design(Efectos, fun="mean", main=" Gráfica de efectos principales", ylab= "Vida Util", xlab="Factor")

#Grafico de interacciones
x11()
interaction.plot(Material, Temperatura, Vida)
#en otro orden
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






