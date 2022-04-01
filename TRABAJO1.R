# TRABAJO 1 DE DISEÑO DE EXPERIMENTOS

library(readxl)
library(tidyverse)
library(agricolae)
library(pgirmess)
library(carData)
library(car)


# EJERCICIO 3.4 LIBRO DE MONTGOMERY

temperatura = c(rep(100,5), rep(125, 4), rep(150, 5), rep(175,4))
densidad = c(21.8, 21.9, 21.7, 21.6, 21.7,
             21.7, 21.4, 21.5, 21.4,
             21.9, 21.8, 21.8, 21.6, 21.5,
             21.9, 21.7, 21.8, 21.4)

d_f <- data.frame(temp = factor(temperatura),
                  den = densidad)

#modelo para evaluar la significancia de los tratamientos
mod_temp = lm(den~temp, data = d_f)
summary.aov(mod_temp)

## RESPUESTA a)
### dado que el valorp de la temperatura es mayor que 0.05 se rechaza la hipotesis nula
### por lo tanto podemos concluir que la temperatura no afecta la densidad de los ladrillo


### RESPUESTA b)
duncan.test(mod_temp,"temp", main = "prueba de duncan", console = TRUE)

### la prueba de Duncan demuestra que no hay diferencia entre los tratamientos
### por lo tanto, habiendo concluido que la temperatura no afecta significativamente
### la densidad, no hay necesidad de realizar pruebas poshoc de diferencia de medias.

### RESPUESTA c)

shapiro.test(mod_temp$residuals)
mean(mod_temp$residuals)
bartlett.test(mod_temp$residuals~temp,data=d_f)

### Los residuales satisfacen los supuestos que el error sigue una distribucion normal
### con media cero y varianza sigma

### RESPUESTA d)

f_escalacion = sqrt(0.02571/5)

mtr1 = d_f %>% filter(temp == 100) %>%
  select(den)

mtr2 = d_f %>% filter(temp == 125) %>%
  select(den)

mtr3 = d_f %>% filter(temp == 150) %>%
  select(den)

mtr4 = d_f %>% filter(temp == 175) %>%
  select(den)


mtr = c(mean(mtr1$den), mean(mtr2$den), mean(mtr3$den), mean(mtr4$den))

fe = mtr * f_escalacion


plot(fe, mtr)

plot(fe)

plot(fe, c(0,0,0,0))

tg = rt(4, 3)

tg1 = f_escalacion * tg

plot(tg1)
# EJERCICIO 3.10 LIBRO MONTGOMERY

t_circuito = c(rep(1,5), rep(2,5), rep(3,5)) 
tiempo = c(9,12,10,8,15,
           20,21,23,17,30,
           6,5,8,16,7)

df_310 = data.frame(tp = factor(t_circuito),
                    tresp = tiempo)

modelo_310 = lm(tresp~tp, data = df_310, )
summary.aov(modelo_310)

# respuesta a
scheffe.test(modelo_310,"tp",alpha = 0.01,group=TRUE,console=TRUE,main="Tiempo de respuesta")

# respuesta b
TukeyHSD(aov(modelo_310),"tp", conf.level = 0.01)

# respuesta c

mtr1 = df_310 %>% filter(tp == 1) %>%
  select(tresp)

mtr2 = df_310 %>% filter(tp == 2) %>%
  select(tresp)

mtr3 = df_310 %>% filter(tp == 3) %>%
  select(tresp)

fe = sqrt(16.9 / 5)

mtr = c(mean(mtr1$tresp), mean(mtr2$tresp), mean(mtr3$tresp))

fe_mtr = fe * mtr

plot(fe_mtr, mtr)

# respuesta d

co = 2*(mean(mtr2$tresp)) - mean(mtr1$tresp) - mean(mtr3$tresp)

SSCo = (co^2)/(5*3)

SSE = 16.9

Fo = SSCo / SSE

valor_p <- pf(Fo, df1 = 1, df2 = 12, lower.tail = FALSE)

# no se rechaza la hipotesis nula por lo tanto no se puede afirmar que el supuesto inicial
# donde se creia que el tipo de circuito dos diferia de los otros dos sea verdadero.

#respuesta e

boxplot(tresp~tp,data = df_310,id=list(method="y"))

# de acuerdo a la grafica anterior, podemos concluir que el tipo de circuito 3 dismuye 
# sustancialmente el tiempo de respuesta respecto de los otros dos tipos de circuitos
# cabe resaltar, segun la prueba de TUKEY los tipos 1 y 3 no son diferentes, sin embargo,
# la grafica de caja y bigotes muestra una mejor respuesta en el tipo 3

# respuesta f

#Esperanza de los residuos = cero
mean(modelo_310$residuals)
#se logra evidenciar que la esperanza de los residuos tiende a cero

#prueba de homocedasticidad
bartlett.test(modelo_310$residuals~tp,data=df_310)
#cumple el supuestos que las varianzas son iguales

#prueba de normalidad
shapiro.test(modelo_310$residuals)
#se rechaza la hipotesis nula por lo tanto la distribucion de los residuos no cumple
# el supuesto de normalidad

plot(modelo_310$fitted.values,modelo_310$residuals,main="Residuales vs predicho")
qqnorm(modelo_310$residuals,pch=20)
qqline(modelo_310$residuals)

#evaluando la normalidad en el metodo grafico de quartiles, podemos notar que existen
#valores atipicos que jalan la distribucion fuera de la normalidad, por lo tanto, 
#podriamos decir, que la distribucion de los residuales podría tener una distribucion de tendencia
# normal con media cero y desviacion estandar sigma
 


#EJERCICIO 3.15 LIBRO DE MONTGOMERY

catalizador = c(rep(1,5), rep(2,4), rep(3,3), rep(4,4))

concentracion = c(58.2, 57.2, 58.4, 55.8, 54.9,
                 56.3, 54.5, 57.0, 55.3,
                 50.1, 54.2, 55.4,
                 52.9, 49.9, 50.0, 51.7)

df_315 = data.frame(cat = factor(catalizador),
                    con = concentracion)

# modelo 

mod_315 = lm(con~cat, data = df_315)
summary.aov(mod_315)

## respuesta a
scheffe.test(mod_315,"cat",group=TRUE,console=TRUE,main="Concentracion")
TukeyHSD(aov(mod_315),"cat")


## Dado que el valor p en el anova para el catalizador es menor que 0.05 con una significancia
## del 0.05 se rechaza la hipotesis nula y, además, basado en los resultados de las pruebas
## POSHOC Tukey y Scheffe, podemos concluir que los 4 catalizadores no 
## tienen el mismo efecto en la concentracion de la sustancia quimica

## respuesta b

## los residuales deben cumplir el supuesto que se distribuyen normalmente con media cero y varianza sigma

### media cero

mean(mod_315$residuals)
#podemos observar una clara tendencia hacia el cero

#normalidad
shapiro.test(mod_315$residuals)
# no se rechaza la hipotesis nula por lo tanto se cumple el supuesto de normalidad

#homocedasticidad
bartlett.test(mod_315$residuals~cat, data = df_315)
#no se rechaza la hipotesis nula por lo tanto se cumple el supuesto de homocedasticidad

## respuesta c

# valor del MSE

MSE_315 = 2.88
n1 = 5
N = length(concentracion)
a = length(unique(catalizador))


t = qt((0.01/2),(N-a), lower.tail = FALSE)

IC = t*(sqrt((MSE_315*2)/n1))

me = df_315 %>% filter(cat == 1) %>%
  summarise(mean(con))

mtr1 = me$`mean(con)`

print('Para la media del tratamiento 1, con un nivel de confianza del 99% se tiene el siguiente Intervalo de Confianza')
print(paste('El intervalo inferior es ', mtr1 - IC, 'y el superior es ', mtr1 + IC))


# EJERCICIO 3.17 LIBRO DE MONTGOMERY


metodo = c(rep(1, 5), rep(2, 5), rep(3, 5))
conteo = c(31, 10, 21, 4, 1,
           62, 40, 24, 30, 35,
           53, 27, 120, 97, 68)

df_317 = data.frame(met = factor(metodo),
                    cont = conteo)

## respuesta a

### modelo 

mod317 = lm(cont~met, data = df_317)
summary.aov(mod317)
### evaluada la significancia de los metodos se evalua la igualdad entre ellos

scheffe.test(mod317,"met",group=TRUE,console=TRUE,main="Conteo")

### basado en los resultados anteriores podemos concluir que existe al menos un metodo
### que es diferente a los demás, en este caso, los metodos 1 y 3 son diferentes entre sí.

## respuesta b

plot(mod317$fitted.values,mod317$residuals,main="Residuales vs predicho")
qqnorm(mod317$residuals,pch=20)
qqline(mod317$residuals)

### existen varios valores fuera de la linea diagonal en el grafico de cuartiles, es posible que
### no se cumpla el supuesto de normalidad

shapiro.test(mod317$residuals)

### no obstante, al realizar la prueba de normalidad de shapiro, encontramos que sí se cumple el supuesto
### de normalidad

## respuesta c

### reliazada la prueba de normalidad se realizan las pruebas de los demás supuestos de los residuales

### Esperanza cero

mean(mod317$residuals)
# existe una clara tendencia hacia el cero

#homocedasticidad
bartlett.test(mod317$residuals~met, data = df_317)
# a pesar del valor bajo del valor p, se cumple el supuesto de homocedasticidad

### anteriormente se realizó prueba de Scheffe para validar la diferencia entre las medias de los tratamientos
### se realizan en este punto test de Dunca y Tukey solo para comprobar los resultados anteriores

#### tukey

TukeyHSD(aov(mod317),"met")

#### Duncan

duncan.test(mod317,"met", main = "prueba de duncan", console = TRUE)


### con base en estos resultados podemos concluir que el modelo del experimento es significativo para los tratmaientos
### y, ademas, que los mismos difieren entre sí, al menos, en los efectos producidos por los metodos 1 y 3 en el conteo
### de las particulas en las obleas.
# Ejercicio 4-5 Libro de Montgomery


## El ejercicio es un diseño de bloques completos


tratamiento = (rep(seq(1,5), 6))
bloques = c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5))
efecto = cbind(c(0.78, 0.85, 0.93, 1.14, 0.97),
         c(0.80, 0.85, 0.92, 0.97, 0.86),
         c(0.81, 0.92, 0.95, 0.98, 0.78),
         c(0.75, 0.86, 0.89, 0.88, 0.76),
         c(0.77, 0.81, 0.89, 0.86, 0.76),
         c(0.78, 0.83, 0.83, 0.83, 0.75))

df <- data.frame(boquilla = factor(tratamiento), 
                    vel = factor(bloques), 
                    dif = c(0.78, 0.85, 0.93, 1.14, 0.97,
                           0.80, 0.85, 0.92, 0.97, 0.86,
                           0.81, 0.92, 0.95, 0.98, 0.78,
                           0.75, 0.86, 0.89, 0.88, 0.76,
                           0.77, 0.81, 0.89, 0.86, 0.76,
                           0.78, 0.83, 0.83, 0.83, 0.75)
                  )

#las columnas son los bloques y las filas los tratamientos

### Ejercicio hecho a Pedal


a = length(unique(tratamiento))
b = length(unique(bloques))

media_bloque1 = mean(efecto[,1])
media_bloque2 = mean(efecto[,2])
media_bloque3 = mean(efecto[,3])
media_bloque4 = mean(efecto[,4])
media_bloque5 = mean(efecto[,5])
media_bloque6 = mean(efecto[,6])

media_tr1 = mean(efecto[1,])
media_tr2 = mean(efecto[2,])
media_tr3 = mean(efecto[3,])
media_tr4 = mean(efecto[4,])
media_tr5 = mean(efecto[5,])

media_global = mean(efecto)

#suma de las medias de los tratamientos
sum_tr = c(((media_tr1 - media_global)^2), ((media_tr2 - media_global)^2),
          ((media_tr3 - media_global)^2), ((media_tr4 - media_global)^2),
          ((media_tr5 - media_global)^2)) # suma de los tratamientos

#sumatoria de los cuadrados de los tratamientos
SSTr = b *(sum(sum_tr))
gl_tr = a -1

# suma de las medias de los bloques
sum_b = c(((media_bloque1 - media_global)^2), ((media_bloque2 - media_global)^2),
          ((media_bloque3 - media_global)^2), ((media_bloque4 - media_global)^2),
          ((media_bloque5 - media_global)^2)) 

#sumatoria de los cuadrados de los tratamientos
SSB = a * (sum(sum_b))
gl_b = b - 1

# error 
error1 = c(((efecto[1,1] - media_tr1 - media_bloque1 + media_global)^2),
           ((efecto[1,2] - media_tr1 - media_bloque2 + media_global)^2),
           ((efecto[1,3] - media_tr1 - media_bloque3 + media_global)^2),
           ((efecto[1,4] - media_tr1 - media_bloque4 + media_global)^2),
           ((efecto[1,5] - media_tr1 - media_bloque5 + media_global)^2),
           ((efecto[1,6] - media_tr1 - media_bloque6 + media_global)^2))

error2 = c(((efecto[2,1] - media_tr2 - media_bloque1 + media_global)^2),
           ((efecto[2,2] - media_tr2 - media_bloque2 + media_global)^2),
           ((efecto[2,3] - media_tr2 - media_bloque3 + media_global)^2),
           ((efecto[2,4] - media_tr2 - media_bloque4 + media_global)^2),
           ((efecto[2,5] - media_tr2 - media_bloque5 + media_global)^2),
           ((efecto[2,6] - media_tr2 - media_bloque6 + media_global)^2))

error3 = c(((efecto[3,1] - media_tr3 - media_bloque1 + media_global)^2),
           ((efecto[3,2] - media_tr3 - media_bloque2 + media_global)^2),
           ((efecto[3,3] - media_tr3 - media_bloque3 + media_global)^2),
           ((efecto[3,4] - media_tr3 - media_bloque4 + media_global)^2),
           ((efecto[3,5] - media_tr3 - media_bloque5 + media_global)^2),
           ((efecto[3,6] - media_tr3 - media_bloque6 + media_global)^2))

error4 = c(((efecto[4,1] - media_tr4 - media_bloque1 + media_global)^2),
            ((efecto[4,2] - media_tr4 - media_bloque2 + media_global)^2),
            ((efecto[4,3] - media_tr4 - media_bloque3 + media_global)^2),
            ((efecto[4,4] - media_tr4 - media_bloque4 + media_global)^2),
            ((efecto[4,5] - media_tr4 - media_bloque5 + media_global)^2),
            ((efecto[4,6] - media_tr4 - media_bloque6 + media_global)^2))

error5 = c(((efecto[5,1] - media_tr5 - media_bloque1 + media_global)^2),
            ((efecto[5,2] - media_tr5 - media_bloque2 + media_global)^2),
            ((efecto[5,3] - media_tr5 - media_bloque3 + media_global)^2),
            ((efecto[5,4] - media_tr5 - media_bloque4 + media_global)^2),
            ((efecto[5,5] - media_tr5 - media_bloque5 + media_global)^2),
            ((efecto[5,6] - media_tr5 - media_bloque6 + media_global)^2))

# sumatoria de los cuadros del error
SSE = sum(sum(error1), sum(error2), sum(error3), sum(error4), sum(error5)) 
gl_e = (a-1)*(b-1)

# Sumatoria de los cuadrados totales

SST = SSTr + SSB + SSE
gl_t = (a*b) - 1

# Media de los cuadrados de los tratamientos
MSTr = SSTr / gl_tr

# media de los cuadrados de los bloques
MSB = SSB / gl_b

# media de los cuadrados del error
MSE = SSE / gl_e

Fo_tr = MSTr / MSE
Fo_b = MSB / MSE


# H0: Las medias de los tratamientos son iguales
# H1: Al menos el efecto en uno de los tratamientos es diferente
valor_p_tr <- pf(Fo_tr, df1 = gl_tr, df2 = gl_e, lower.tail = FALSE)

# con un nivel de significancia del 0.05 se rechaza la hipotesis nula

# H0: Las medias de los bloques son iguales
# H1: Al menos el efecto en uno de los bloques es diferente
valor_p_b <- pf(Fo_b, df1 = gl_b, df2 = gl_e, lower.tail = FALSE)

# con un nivel de significancia del 0.05 se rechaza la hipotesis nula

# diagrama de dispersion

plot(tratamiento, efecto)
boxplot(df$dif~df$boquilla)

## respuesta 1 

#Dado que la matriz ANOVA nos muestra que el valorp para los tratamientos, es decir,
# para las boquillas es menos que 0.05, se rechaza la hipotesis nula por lo tanto podemos afirmar,
#que el diseño de la boquilla afecta la velocidad.
# Esto se confirma en los graficos de dispersion y caja y bigotes donde se evidencia claramente
# que los diseños de boquilla 3 y 4 aumentan las diferencias potenciales mientras que los diseños
# 1 y 5 las reducen sustancialmente.


# respuesta 2

To1 = media_tr1 - media_global
To2 = media_tr2 - media_global
To3 = media_tr3 - media_global
To4 = media_tr4 - media_global
To5 = media_tr5 - media_global

B1 = media_bloque1 - media_global
B2 = media_bloque2 - media_global
B3 = media_bloque3 - media_global
B4 = media_bloque4 - media_global
B5 = media_bloque5 - media_global
B6 = media_bloque6 - media_global

y_gorro11 = To1 + B1 + media_global
y_gorro21 = To2 + B1 + media_global
y_gorro31 = To3 + B1 + media_global
y_gorro41 = To4 + B1 + media_global
y_gorro51 = To5 + B1 + media_global

y_gorro12 = To1 + B2 + media_global
y_gorro22 = To2 + B2 + media_global
y_gorro32 = To3 + B2 + media_global
y_gorro42 = To4 + B2 + media_global
y_gorro52 = To5 + B2 + media_global

y_gorro13 = To1 + B3 + media_global
y_gorro23 = To2 + B3 + media_global
y_gorro33 = To3 + B3 + media_global
y_gorro43 = To4 + B3 + media_global
y_gorro53 = To5 + B3 + media_global

y_gorro14 = To1 + B4 + media_global
y_gorro24 = To2 + B4 + media_global
y_gorro34 = To3 + B4 + media_global
y_gorro44 = To4 + B4 + media_global
y_gorro54 = To5 + B4 + media_global

y_gorro15 = To1 + B5 + media_global
y_gorro25 = To2 + B5 + media_global
y_gorro35 = To3 + B5 + media_global
y_gorro45 = To4 + B5 + media_global
y_gorro55 = To5 + B5 + media_global

y_gorro16 = To1 + B6 + media_global
y_gorro26 = To2 + B6 + media_global
y_gorro36 = To3 + B6 + media_global
y_gorro46 = To4 + B6 + media_global
y_gorro56 = To5 + B6 + media_global

y_gorro <- c(y_gorro11, y_gorro21, y_gorro31, y_gorro41, y_gorro51,
             y_gorro12, y_gorro22, y_gorro32, y_gorro42, y_gorro52,
             y_gorro13, y_gorro23, y_gorro33, y_gorro43, y_gorro53,
             y_gorro14, y_gorro24, y_gorro34, y_gorro44, y_gorro54,
             y_gorro15, y_gorro25, y_gorro35, y_gorro45, y_gorro55,
             y_gorro16, y_gorro26, y_gorro36, y_gorro46, y_gorro56)



residuales1 = df$dif - y_gorro 

resid = data.frame(y = df$dif,
                   y_est = y_gorro,
                   residuales = residuales1)

shapiro.test(residuales1)
leveneTest(residuales1~fctor(tratamiento))

## forma modelo regresion
modelo = lm(dif~vel+boquilla, data = df)

summary.aov(modelo)


residuales = residuals(modelo)
bartlett.test(modelo$residuals~boquilla,data=df)

shapiro.test(modelo$residuals)
plot(modelo$fitted.values,modelo$residuals,main="Residuales vs predicho")
qqnorm(modelo$residuals,pch=20)
qqline(modelo$residuals)

# respuesta 3

boxplot(dif~vel,data = df,id=list(method="y"))

tratamientos = seq(1,5)

media_boquillas = c(media_tr1, media_tr2, media_tr3, media_tr4, media_tr5)

plot(tratamientos, media_boquillas, main = 'Medias de los tratamientos')
smoothingSpline = smooth.spline(tratamientos, media_boquillas, spar=0.1)
lines(smoothingSpline, col='black', lwd=3)

### grafica t student escalonada
factor_escalacion = sqrt(MSE/b)
dis_t = rt(a, gl_tr)
plot(dis_t, factor_escalacion, main = 'Distribucion t con 4 grados de libertad')
smoothingSpline = smooth.spline(tratamientos, dis_t, spar=0.1)
lines(smoothingSpline, col='black', lwd=3)

duncan.test(modelo,"boquilla", main = "prueba de duncan", console = TRUE)

#### Se evidencia que las medias de los factores de los tratamientos tienen una distribución
### con tendencia a la distribucion t-student comprobando que los tratamientos 1 y 4
### presentan diferencias significativas respecto de los demás tratamientos.
### esto también se logra evidenciar en la prueba de Duncan.

# EJERCICIO 4-13 

distancia = rep(seq(4,10,2), 5)
sujeto = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4))
tiempo = c(10, 7, 5, 6,
           6, 6, 3, 4,
           6, 6, 3, 4,
           6, 1, 2, 2,
           6, 6, 5, 3)

df2 <- data.frame(tr = factor(distancia),
                  blq = factor(sujeto),
                  enfoque = tiempo)


#construccion del modelo
mod_enfoque = lm(enfoque~tr+blq, data = df2)

#anova
summary.aov(mod_enfoque)
boxplot(enfoque~blq, data = df2, id=list(method="y"))

scheffe.test(mod_enfoque,"tr",group=TRUE,console=TRUE,main="Tiempo de enfoque")
TukeyHSD(aov(mod_enfoque),"tr")
duncan.test(mod_enfoque,"tr", main = "prueba de duncan", console = TRUE)

resid=residuals(mod_enfoque)
bartlett.test(mod_enfoque$residuals~tr,data=df2)

shapiro.test(mod_enfoque$residuals)
plot(mod_enfoque$fitted.values,mod_enfoque$residuals,main="Residuales vs predicho")
qqnorm(mod_enfoque$residuals,pch=20)
qqline(mod_enfoque$residuals)


# EJERCICIO 4-15


orden = rep(seq(1,4),4)
operador = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4))
metodo = c(3,2,1,4,
           4,3,2,1,
           1,4,3,2,
           2,1,4,3)
ensamble = c(10,7,5,10,
             14,18,10,10,
             7,11,11,12,
             8,8,9,14)

df3 <- data.frame(tra = factor(orden),
                  ope = factor(operador),
                  met = factor(metodo),
                  tiempo = ensamble)

mod_ensa = lm(tiempo~tra+ope+met, data = df3)

summary.aov(mod_ensa)

#el tratamiento no es significativo se hace prueba con bloques completos

df3

# prueba de bloques completos tomando como el metodo como tramiento 

mod_ensa1 = lm(tiempo~met+ope, data = df3)
summary.aov(mod_ensa1)


scheffe.test(mod_ensa1,"met",group=TRUE,console=TRUE,main="Tiempo de ensamble")
TukeyHSD(aov(mod_ensa1),"met")
duncan.test(mod_ensa1,"met", main = "prueba de duncan", console = TRUE)

resid=residuals(mod_ensa1)
bartlett.test(mod_ensa1$residuals~met,data=df3)

shapiro.test(mod_ensa1$residuals)
plot(mod_ensa1$fitted.values,mod_ensa1$residuals,main="Residuales vs predicho")
qqnorm(mod_ensa1$residuals,pch=20)
qqline(mod_ensa1$residuals)









