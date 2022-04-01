#Diseño de cuadrado latino
library(agricolae)

DCL1=data.frame(Dia=factor(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))),
               Lote=factor(rep(c(1,2,3,4),4)),
               Metodo=factor(c(1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1)),
               Resistencia=c(303,299,290,290,280,321,313,282,275,315,319,300,304,293,295,305))

dia=factor(DCL1$Dia)
lote=factor(DCL1$Lote)
metodo=factor(DCL1$Metodo)
resistencia=DCL1$Resistencia
              
modcl1=lm(resistencia~dia+lote+metodo,data=DCL1)
summary(modcl1)
summary(aov(modcl1))
boxplot(resistencia~metodo,data = DCL1)

LSD.test(modcl1,"metodo",group=TRUE,console=TRUE,main="Metodo")
scheffe.test(modcl1,"metodo",group=TRUE,console=TRUE,main="Metodo")
TukeyHSD(aov(modcl1),which = "metodo",ordered = TRUE)

??LSD.test

plot(TukeyHSD(aov(modcl1),which = "metodo",ordered = TRUE))

bartlett.test(modcl1$residuals~metodo,data=DCL1)

shapiro.test(modcl1$residuals)
qqnorm(modcl1$residuals, pch=20)
qqline(modcl1$residuals)
