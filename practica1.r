library(ggplot2)
data(diamonds)
View(diamonds)
summary(diamonds)



summary(diamonds$carat)
summary(diamonds$cut)
summary(diamonds$color)
summary(diamonds$clarity)
summary(diamonds$depth)
summary(diamonds$table)
summary(diamonds$price)


sd(diamonds$carat)
sd(diamonds$depth)
sd(diamonds$table)
sd(diamonds$price)
max(diamonds$price)
min(diamonds$price)

#graficas categoricas
ggplot(diamonds, aes(x = cut, fill = cut)) + geom_bar()
x11()
ggplot(diamonds, aes(x = color, fill = color)) + geom_bar()
x11()
ggplot(diamonds, aes(x = clarity, fill = clarity)) + geom_bar()
x11()
ggplot(diamonds, aes(x = color, fill =  cut)) + geom_bar()
ggplot(diamonds, aes(x = clarity, fill = cut)) + geom_bar()

#numericos
ggplot(diamonds, aes(x = carat)) + geom_density()
x11()
ggplot(diamonds, aes(x = price)) + geom_density()

ggplot(diamonds, aes(depth, fill = cut, colour = cut)) + geom_density(alpha = 0.1)



