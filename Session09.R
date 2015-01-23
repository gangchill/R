# Some numeric data
x <- rep(1:5,4)
# An unordered factor (nominal variable)
f <- factor(x,levels=1:5,
            labels=c("A", "B", "C", "D", "E"))
# Show contrast matrix
contrasts(f)

# 5 dummy regressors with the second category as baseline
contrasts(f) <- contr.treatment(levels(f),base=2)
contrasts(f)

# 5 deviation contrast regressors with the last category
# as baseline
contrasts(f) <- contr.sum(levels(f))
contrasts(f)

# To set a baseline category here, you need the package 'memisc'
library(memisc)
contrasts(f) <- contr.sum(levels(f),base=2)
f

# Finally an example with `C'
C(f,contr.treatment,base=5)

## Real-data example: Occupational Prestige
library(car)
with(Prestige,levels(type))

# First, give the 'type' variable some better
# factor-level ordering and some better labels

Prestige <- within(Prestige,{
       type <- factor(type,
                      levels=c("bc","wc","prof"),
                      labels=c("Blue collar",
                               "White collar",
                               "Professional")
                      )
       })

# Examine the occupation type
# and the prestige by type
attach(Prestige)

# Contrasts
contrasts(type)
# Mean prestige per level of 'type'
tapply(prestige, type, mean)

detach(Prestige)

# Regression with type as indep. var
summary(lm(prestige~type, data=Prestige))

# Without intercept - compare this with the 
# group averages
summary(lm(prestige~type-1, data=Prestige))

# With intercept
summary(lm(prestige~type, data=Prestige))

# Changing the baseline category
summary(lm(prestige~type, data=Prestige,
           contrasts=list(type=contr.treatment(3,base=2))))

# Deviation coding
summary(lm(prestige~type, data=Prestige,
           contrasts=list(type=contr.sum)))

# Changing the baseline category with C()
summary(lm(prestige~C(type,treatment,base=2), data=Prestige))
        
# More complicated, but nicer output
summary(lm(prestige~type, 
           data=within(Prestige,{
             contrasts(type) <- contr.treatment(levels(type),
                                                base=2)
           })
       ))
