set.seed(2)


x <- rep(1:5,each=20)
y <- 0.3 + 0.5*x + rnorm(n=length(x),sd=.3)
plot(x,y)
abline(a=.3,b=.5,col="red")


x <- rnorm(n=100,mean=3)
y <- 0.3 + 0.5*x + rnorm(n=length(x),sd=.3)
plot(x,y)
abline(a=.3,b=.5,col="red")


# Let's create some artificial data
set.seed(2)

x <- rep(1:5,each=20)
y <- 0.3 + 0.5*x + rnorm(n=length(x),sd=.3)
DataFrame1 <- data.frame(x,y)

x <- rnorm(n=100,mean=3)
y <- 0.3 + 0.5*x + rnorm(n=length(x),sd=.3)
DataFrame2 <- data.frame(x,y)

# 'Unclutter' the workspace
rm(x,y)


#We get the data from a data frame
lm1 <- lm(y~x,data=DataFrame1)

# The "lm" object
print(lm1)


# The coefficients
coef(lm1)


# A model summary
summary(lm1)

library(car)

# Years of education as independent variable
lm.prestige_education <- lm(prestige~education,
                            data=Prestige)
summary(lm.prestige_education)


plot(prestige~education,data=Prestige)
abline(lm.prestige_education,col="red")


# Income as independent variable
lm.prestige_income <- lm(prestige~income,
                            data=Prestige)
summary(lm.prestige_income)


plot(prestige~income,data=Prestige)
abline(lm.prestige_income,col="red")
# Not a very good fit, apparently.


# Logarithm of income as independent variable
lm.prestige_logincome <- lm(prestige~log(income),
                            data=Prestige)
summary(lm.prestige_logincome)


plot(prestige~log(income),data=Prestige)
abline(lm.prestige_logincome,col="red")


lm.prestige_biv <- lm(prestige~education+log(income),
                            data=Prestige)
summary(lm.prestige_biv)


# We really want to compare the relevance of the variables,
# thus we ask for standardised coefficients
# 'scale' does the trick
lm.prestige_biv.std <- lm(scale(prestige)~scale(education)+scale(log(income))-1,
                            data=Prestige)
summary(lm.prestige_biv.std)


