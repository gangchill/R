library(Matching)

data(GerberGreenImai)
# GerberGreenImai: a data set for examining the effect of
# get-out-to-vote appeals on turnout - it is fetched
# from the package with 'data'.

# A matrix of covariates is created
Covariates <- with(GerberGreenImai,
                   model.matrix(~ PERSONS + VOTE96.1 + NEW + MAJORPTY + AGE +
                                  WARD + PERSONS:VOTE96.1 + PERSONS:NEW + AGE2-1))

# Call of 'Match' with exact matching
# by default ATT is estimated
GGI.ATT.exact<- with(GerberGreenImai,
                     Match(Y=VOTED98,
                           Tr=PHN.C1,
                           X=Covariates,
                           exact=TRUE))
summary(GGI.ATT.exact)

# ATC is estimated
GGI.ATC.exact <- with(GerberGreenImai,
                      Match(Y=VOTED98,
                            Tr=PHN.C1,
                            X=Covariates,
                            estimand="ATC",
                            exact=TRUE))
summary(GGI.ATC.exact)

# ATE is estimated
GGI.ATE.exact <- with(GerberGreenImai,
                       Match(Y=VOTED98,
                             Tr=PHN.C1,
                             X=Covariates,
                             estimand="ATE",
                             exact=TRUE))
summary(GGI.ATE.exact)

# Check of the balance before and after matching
MatchBalance(PHN.C1~ PERSONS + VOTE96.1 + NEW + MAJORPTY + AGE +
               WARD + PERSONS:VOTE96.1 + PERSONS:NEW + AGE2,
             data=GerberGreenImai,
             match.out=GGI.ATE.exact)

with(GerberGreenImai,
     with(GGI.ATE.exact,
          qqplot(AGE[index.control],
                 AGE[index.treated])
     ))

# Call of 'Match' with nearest-neighbour matching based on
# Mahalanobis-distance
# ATE is estimated here
GGI.ATE.MD<- with(GerberGreenImai,
                  Match(Y=VOTED98,
                        Tr=PHN.C1,
                        X=Covariates,
                        estimand="ATE",
                        Weight=2))
summary(GGI.ATE.MD)

# Checking the balance before and after matchings
MatchBalance(PHN.C1~ PERSONS + VOTE96.1 + NEW + MAJORPTY + AGE +
               WARD + PERSONS:VOTE96.1 + PERSONS:NEW + AGE2,
             data=GerberGreenImai,
             match.out=GGI.ATE.MD)

# Once again nearest-neighbour matching with Mahalanobis distance,
# this time with a caliper
GGI.ATE.MD.C.25<- with(GerberGreenImai,
                       Match(Y=VOTED98,
                             Tr=PHN.C1,
                             X=Covariates,
                             estimand="ATE",
                             caliper=.25,
                             Weight=2))
summary(GGI.ATE.MD.C.25)

MatchBalance(PHN.C1~ PERSONS + VOTE96.1 + NEW + MAJORPTY + AGE +
               WARD + PERSONS:VOTE96.1 + PERSONS:NEW + AGE2,
             data=GerberGreenImai,
             match.out=GGI.ATE.MD.C.25)

##
# To obtain propensity scores we estimate 
# a logistic regression model

PS.model <- glm(PHN.C1 ~ PERSONS + VOTE96.1 + NEW + MAJORPTY + AGE +
                                WARD + PERSONS:VOTE96.1 + PERSONS:NEW + AGE2, 
                              family=binomial(logit), data=GerberGreenImai)

PScore.p <- fitted(propensity.score.model)

GGI.ATE.PSP<- with(GerberGreenImai,
                  Match(Y=VOTED98,
                        Tr=PHN.C1,
                        X=PScore.p,
                        estimand="ATE"))
summary(GGI.ATE.PSP)

MatchBalance(PHN.C1~ PERSONS + VOTE96.1 + NEW + MAJORPTY + AGE +
               WARD + PERSONS:VOTE96.1 + PERSONS:NEW + AGE2,
             data=GerberGreenImai,
             match.out=GGI.ATE.PSP)


# Propensity-score matching does not help to achieve balance
# Therefore we try again with a caliper

GGI.ATE.PSP.C.01<- with(GerberGreenImai,
                   Match(Y=VOTED98,
                         Tr=PHN.C1,
                         X=PScore.p,
                         caliper=.01,
                         estimand="ATE"))
summary(GGI.ATE.PSP.C.01)

MatchBalance(PHN.C1~ PERSONS + VOTE96.1 + NEW + MAJORPTY + AGE +
               WARD + PERSONS:VOTE96.1 + PERSONS:NEW + AGE2,
             data=GerberGreenImai,
             match.out=GGI.ATE.PSP.C.01)

## Visual checks for the balance between treatment and control groups

with(GerberGreenImai,
        qqplot(AGE[PHN.C1==0],
               AGE[PHN.C1==1],
               xlab="Age in Control Group",
               ylab="Age among the Treated",
               main="Q-Q-Plot of Age before Matching"),
     )
abline(a=0,b=1,col="red")

with(GerberGreenImai,
     with(GGI.ATE.exact,
          qqplot(AGE[index.control],
                 AGE[index.treated],
                 xlab="Age in Control Group",
                 ylab="Age among the Treated",
                 main="Q-Q-Plot of Age after Exact Matching"))
     )
abline(a=0,b=1,col="red")

with(GerberGreenImai,
     with(GGI.ATE.PSP.C.01,
          qqplot(AGE[index.control],
                 AGE[index.treated],
                 xlab="Age in Control Group",
                 ylab="Age among the Treated",
                 main="Q-Q-Plot of Age after Propensity-Score Matching with Caliper"))
        )
abline(a=0,b=1,col="red")
