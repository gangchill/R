library(car)
Chile2 <- within(Chile,{
   # Ignore all `non-valid' levels of the factor
   vote2 <- factor(vote,
               levels=c("N","Y"))
   # A binary variable
   vote.bnry <- as.integer(vote2=="Y")
   })
Chile2 <- subset(Chile2,is.finite(vote2))

# Binary dependent variable
# Using standardised versions of age and income as
# regressors: scale(age) and scale(income)
Chile.glm.b <- glm(
       vote.bnry~sex+scale(age)+education+scale(income),
                         data=Chile2,family=binomial)
summary(Chile.glm.b)

# Response is a two-level factor
Chile.glm.f <- glm(
           vote2~sex+scale(age)+education+scale(income),
                         data=Chile2,family=binomial)
summary(Chile.glm.f)

library(car)
# The data frame contains experimental conditions
# and numbers of correctly recalled words (out of 40)
Friendly[c(1:3,11:13,21:23),]

# Based on the conditions, one can assume that
# the counts follow a binomial distribution with
# denominator 40
Friendly.glm <- glm(cbind(correct,40-correct)~condition,
                    data=Friendly,
                    family=binomial)
summary(Friendly.glm)


library(car)
Chile2 <- within(Chile,{
  # Ignore all `non-valid' levels of the factor
  vote2 <- factor(vote,
                  levels=c("N","Y"))
  # A binary variable
  vote.bnry <- as.integer(vote2=="Y")
})
Chile2 <- subset(Chile2,is.finite(vote2))


# The null model
Chile.glm.0 <- glm(
  vote.bnry~sex+scale(age)+scale(income),
  family=binomial,
  data=Chile2,
  subset=is.finite(education))
# The model of the alternative hypothesis
Chile.glm.1 <- glm(
  vote.bnry~sex+scale(age)+scale(income)+education,
  family=binomial,
  data=Chile2)

# A likelihood ratio test of the effect of education
anova(
  Chile.glm.0,
  Chile.glm.1,
  test="Chisq"
  )

Friendly.glm <- glm(cbind(correct,40-correct)~condition,
                    data=Friendly,
                    family=binomial)
# The null model
Friendly.glm0 <- glm(cbind(correct,40-correct)~1,
                    data=Friendly,
                    family=binomial)

anova(
  Friendly.glm0,
  Friendly.glm,
  test="Chisq"
)