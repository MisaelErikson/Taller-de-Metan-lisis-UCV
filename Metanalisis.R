# Taller de Meta-analisis

library(tidyverse)

#cargando origen de datos
org="D:\\escri\\MetaAnalisis\\BaseDeDatos"

#ingresando a directorio de trabajo
setwd(org)
library(readxl)
baseMA=read.csv("correlacion.csv",sep=";")
baseMA

#calcular el tamaño de efecto en meta-análisis
#install.packages("metafor")
library(metafor)
help(escalc) # para conocer los distintos tipos de efectos. 
mid=escalc(measure='COR',ni=baseMA$Total,ri=baseMA$COR)

#Tamaño de efecto "yi" y variabilidad "vi" 
mid

#agregar a la base esas variables
baseMA$nuevaD=mid$yi
baseMA$nuevaVarD=mid$vi  

#uso de esas variables para un MA
help(rma)
resultMA=rma(yi=baseMA$nuevaD, vi=baseMA$nuevaVarD, 
             method="ML",
             weighted=TRUE, 
             level=95, 
             digits=4)

#presentacion de principales resultados
summary(resultMA)

# I^2>25% se considera una variabilidad a ser estudiada
# Cohen (1988) definió el tamaño del efecto como:
#pequeño d=0.2
#mediano d=0.5
#grande  d=0.8

# Intervalo de confinaza para los indices de heterogeneidad:
confint(resultMA)
## algunas revistas lo solicitan para evaluación
## pero es un dato importante.

#moderador Continuo
ModeradorN=rma(yi=baseMA$nuevaD, vi=baseMA$nuevaVarD, 
               mods =~ baseMA$Total, 
               method="ML",
               weighted=TRUE, 
               level=95, 
               digits=4)
summary(ModeradorN)

# Meta anova
# Moderador categoricos:
#Para ver la de cada una de las categoricas
ModeradorFormato2=rma(yi=baseMA$nuevaD, vi=baseMA$nuevaVarD, 
                      mods=~factor(baseMA$Instrumento), 
                      method="ML", 
                      weighted=TRUE, 
                      level=95, 
                      digits=4)
summary.rma(ModeradorFormato2)

#Test de Egger
# Ho: Sesgo de publicación
# H1: no hay sesgo de publicación
regtest(resultMA, model="lm")

#Funnel plot (grafico de enbudo)
# para determinar los sesgos de publicación
# tiene que tener los puntos dentro de la piramide
funnel(resultMA)

#Funnel plot mejorado 
# para ver si hay sesgo de publicacion con diferente niveles
# probabilidad
funnel(resultMA, level=c(90, 95, 99), 
       shade=c("white", "gray55", "gray75"), 
       refline=0, legend=TRUE)

#Funnel con trim and fill
# esto nos indica que hay punto blancos
# con contornos que deberian existir
taf=trimfill(resultMA)
funnel(taf, legend=TRUE)


#Rosenthal.
fsn(yi=baseMA$nuevaD, vi=baseMA$nuevaVarD,type = "Rosenthal")

fsn(yi=baseMA$nuevaD, vi=baseMA$nuevaVarD,type = "Rosenberg")

fsn(yi=baseMA$nuevaD, vi=baseMA$nuevaVarD,type = "Orwin")

#Analisis de sensitividad:

# como mejora el meta analisis, si eliminamos un estudio 
leave1out(resultMA)

#determina que estudio podemos eliminar 
influenciado=influence.rma.uni(resultMA)

#Muestra los objetos que estan influyendo 
influenciado$is.infl


#Grafica de valores de influencia
# para determinar que estudios estan influyendo en los resultados
#afectado mucho
influenciado=influence.rma.uni(resultMA)
plot(influenciado)


#--------------------------------------------------
#FOREST PLOT

#1. Forest basico
# el tamaño de los puntos indica que estudio tiene mas impacto
# el rombo inica el punto de coorte  y el ancho 
# es el intervalo de confianza

# si corta en cero los estudios no son significativos
forest(resultMA)

#2. Forest con autores y eje x
forest(resultMA,  
       slab = paste(baseMA$ï..Autor,
                    baseMA$year, sep = ", "),
       xlab="Efecto de correlación", 
       order=order(baseMA$ï..Autor))

#3. Forest con pesos y limites del eje x
forest(resultMA,  
       slab = paste(baseMA$ï..Autor,
                    baseMA$year, sep = ", "),
       xlab="Efecto de correlación", 
       order=order(baseMA$ï..Autor), 
       xlim = c(-2, 2),
       alim= c(-0.5, 1),
       showweights=TRUE)

#4. AÃ±adir cabeceras.

text(-1.5, 10, "Autor y año", cex=.8, font=2)
text(1.4, 10, "Pesos", cex=.8, font=2)
text(1.7, 10, "D [IC-95%]", cex=.8, font=2)

#margenes mas pequeños 
par(mar=c(1,4,3,2))

#graficos con decimales con punto
options(OutDec=',')
