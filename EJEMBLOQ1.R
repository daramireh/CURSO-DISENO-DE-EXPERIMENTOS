library(readxl)
BLQ1 <- read_excel("problema.xlsx")
View(BLQ1)

fnitrog=factor(BLQ1$Fnitrog)
Bloq=factor(BLQ1$bloq)
modbloq=lm(prodced~Bloq+fnitrog,data = BLQ1)

summary.aov(modbloq)
boxplot(prodced~bloq,data = BLQ1,id=list(method="y"))

library(agricolae)
scheffe.test(modbloq,"fnitrog",group=TRUE,console=TRUE,main="Fuente nitr?geno")

TukeyHSD(aov(modbloq),"fnitrog")
resid=residuals(modbloq)
bartlett.test(modbloq$residuals~fnitrog,data=BLQ1)

shapiro.test(modbloq$residuals)
plot(modbloq$fitted.values,modbloq$residuals,main="Residuales vs predicho")
qqnorm(modbloq$residuals,pch=20)
qqline(modbloq$residuals)

