library(car)

lm.prestige_iii <- lm(prestige~education+log(income)+type,
                      data=Prestige)
summary(lm.prestige_iii)

library(car)

lm.prestige_ii <- lm(prestige~education+log(income),
                     data=Prestige,
                     subset=is.finite(type))
# Filter missing values,
# otherwise an error occurs

# Modify 'lm.prestige_ii' by adding regressors for
# the factor 'type'
lm.prestige_iii <- update(lm.prestige_ii,
                          .~.+type)

# This is equivalent with
# lm.prestige_iii <- lm(prestige~education+log(income)+type,
#                            data=Prestige)

# F-test comparing two models, thereby testing the
# statistical significance of the variable 'type'
anova(lm.prestige_ii,lm.prestige_iii)

lm.prestige_iii <- lm(prestige~education+log(income)+type,
                      data=Prestige)

# Sequential F-test of all terms in the model
anova(lm.prestige_iii)

lm.prestige_iii <- lm(prestige~education+log(income)+type,
                      data=Prestige)
coef(lm.prestige_iii)

# Wald test
linearHypothesis(lm.prestige_iii,
                 c(
                   "education = log(income)",
                   "typeprof  = typewc + 1"
                 ))

# Alternative formulation
linearHypothesis(lm.prestige_iii,
                 hypothesis.matrix=matrix(c(
                   0, 1, -1, 0,  0,
                   0, 0,  0, 1, -1
                 ),nrow=2,byrow=TRUE),
                 rhs=c(0,1)
)
