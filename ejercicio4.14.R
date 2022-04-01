#EJERCICIO 4.14 MONGOTMERY

library(readxl)
library(agricolae)
install.packages("pgirmess")
library(pgirmess)

df <- read_excel("EjemploCL.xlsx", 
                 sheet = "Hoja2")

dia = factor(df$Dia)
lote = factor(df$Lote)
metodo = factor(df$Ingrediente)
efecto = df$Efecto

mod = lm(efecto~dia+lote+metodo,data=df)
summary(mod)
summary(aov(mod))

boxplot(efecto~metodo,data = df)

# como solo el tratamiento es significativo, se hace diseño unifactorial





