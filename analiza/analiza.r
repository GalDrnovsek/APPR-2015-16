# 4. faza: Analiza podatkov

library(ggplot2)
library(MASS)
library(mgcv)
library(maptools)

#prvi del
a <- ggplot(vse_skupaj1, aes(x=`GDP pc`, y=`GMs per million`)) + geom_point()
print(a)

a + geom_smooth(method = "lm")

lin1 <- lm(data = vse_skupaj1, vse_skupaj1$`GDP pc` ~ vse_skupaj1$`GMs per million`)
lin1
predict(lin1, data.frame(vse_skupaj1$`GMs per million`))

kv1 <- lm(data = vse_skupaj1, vse_skupaj1$`GDP pc` ~ vse_skupaj1$`GMs per million` + I(vse_skupaj1$`GMs per million`^2))
kv1
a + geom_smooth(method = "lm", formula = y ~ x + I(x^2))

b <- lowess(vse_skupaj1$`GDP pc`, vse_skupaj1$`GMs per million`)
a + geom_line(data=as.data.frame(b), aes(x=x, y=y), color="green")

mls1 <- loess(data = vse_skupaj1, vse_skupaj1$`GDP pc` ~ vse_skupaj1$`GMs per million`)
a + geom_smooth(method = "loess")

premica1 <- a + geom_smooth(method = "gam", formula = y ~ s(x))
premica1

#drugi del
c <- ggplot(vse_skupaj1, aes(x=`% of GDP`, y=`GMs per million`)) + geom_point()
print(c)

c + geom_smooth(method = "lm")

lin2 <- lm(data = vse_skupaj1, vse_skupaj1$`% of GDP` ~ vse_skupaj1$`GMs per million`)
lin2
predict(lin2, data.frame(vse_skupaj1$`GMs per million`))

kv2 <- lm(data = vse_skupaj1, vse_skupaj1$`% of GDP` ~ vse_skupaj1$`GMs per million` + I(vse_skupaj1$`GMs per million`^2))
kv2
c + geom_smooth(method = "lm", formula = y ~ x + I(x^2))

d <- lowess(vse_skupaj1$`% of GDP`, vse_skupaj1$`GMs per million`)
c + geom_line(data=as.data.frame(d), aes(x=x, y=y), color="green")

mls2 <- loess(data = vse_skupaj1, vse_skupaj1$`% of GDP` ~ vse_skupaj1$`GMs per million`)
krivulja1 <- c + geom_smooth(method = "loess")
krivulja1

c + geom_smooth(method = "gam", formula = y ~ s(x))

#tretji del
e <- ggplot(vse_skupaj2, aes(x=`GDP pc`, y=`Active GMs`)) + geom_point()
print(e)

e + geom_smooth(method = "lm")

lin3 <- lm(data = vse_skupaj2, vse_skupaj2$`GDP pc` ~ vse_skupaj2$`Active GMs`)
lin3
predict(lin3, data.frame(vse_skupaj2$`Active GMs`))

kv3 <- lm(data = vse_skupaj2, vse_skupaj2$`GDP pc` ~ vse_skupaj2$`Active GMs` + I(vse_skupaj2$`Active GMs`^2))
kv3
e + geom_smooth(method = "lm", formula = y ~ x + I(x^2))

f <- lowess(vse_skupaj2$`GDP pc`, vse_skupaj2$`Active GMs`)
e + geom_line(data=as.data.frame(f), aes(x=x, y=y), color="green")

mls3 <- loess(data = vse_skupaj2, vse_skupaj2$`GDP pc` ~ vse_skupaj2$`Active GMs`)
e + geom_smooth(method = "loess")

premica2 <- e + geom_smooth(method = "gam", formula = y ~ s(x))
premica2

#četrti del
i <- ggplot(vse_skupaj2, aes(x=`% of GDP`, y=`Active GMs`)) + geom_point()
print(i)

i + geom_smooth(method = "lm")

lin4 <- lm(data = vse_skupaj2, vse_skupaj2$`% of GDP` ~ vse_skupaj2$`Active GMs`)
lin4
predict(lin4, data.frame(vse_skupaj2$`Active GMs`))

kv4 <- lm(data = vse_skupaj2, vse_skupaj2$`% of GDP` ~ vse_skupaj2$`Active GMs` + I(vse_skupaj2$`Active GMs`^2))
kv4
i + geom_smooth(method = "lm", formula = y ~ x + I(x^2))

j <- lowess(vse_skupaj2$`% of GDP`, vse_skupaj2$`Active GMs`)
i + geom_line(data=as.data.frame(j), aes(x=x, y=y), color="green")

mls4 <- loess(data = vse_skupaj2, vse_skupaj2$`% of GDP` ~ vse_skupaj2$`Active GMs`)
krivulja2 <- i + geom_smooth(method = "loess")
krivulja2

i + geom_smooth(method = "gam", formula = y ~ s(x))
