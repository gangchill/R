library(car)
library(memisc)
UN <- sort(UN,by=~gdp)
lm2 <- lm(log(infant.mortality)~log(gdp),
          data=UN,na.action=na.exclude)

impred <- exp(predict(lm2))
plot(infant.mortality~gdp,
     data=UN)
lines(impred~gdp,data=UN,col="red")

Chiro.scaled <- scale(Chirot)

# Some numeric data
x <- rep(1:5,4)
# An unordered factor (nominal variable)
f <- factor(x,levels=1:5,
            labels=c("A", "B", "C", "D", "E"))
# Show contrast matrix
contrasts(f)

library(MASS)
contrasts(f) <- contr.sdif(levels(f))

contrasts(f)

options(contrasts=c("contr.sum","contr.sdif"))
f <- factor(x,levels=1:5,
            labels=c("A", "B", "C", "D", "E"))
o <- as.ordered(f)
contrasts(f)

contrasts(o)

contrasts(o) <- contr.poly(levels(o))
contrasts(o)
