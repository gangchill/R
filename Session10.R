sink("Session08.Rlog",split=TRUE)
x <- rnorm(50)
f <- factor(rep(1:2,each=25),labels=c("a","b"))
a1 <- 1.4
a2 <-  .2
b1 <- -.2
b2 <-  .3
y <- ifelse(f=="a",
            a1+b1*x,
            a2+b2*x) + .1*rnorm(50)

lm(y ~ f+x)

lm(y ~ f*x)

lm(y ~ f/x)

lm(y ~ f/x - 1)

lm(y ~ f + f:x - 1)

load("norrisWork.RData")

# Main effects only
summary(lm(POLstand~GDPcap_UN+Religion,
      data=norrisWork))

# Interaction of GDP/cap. and religion
summary(lm(POLstand~GDPcap_UN*Religion,
           data=norrisWork))

# Slopes of GDP/cap. nested within religion
summary(lm(POLstand~Religion/GDPcap_UN-1,
           data=norrisWork))

load("Prestige.RData")

summary(lm(prestige~income+type,data=Prestige))

summary(lm(prestige~income*type,data=Prestige))

summary(lm(prestige~type/income-1,data=Prestige))

sink()