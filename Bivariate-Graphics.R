library(car)
# 'UN' is a data set with country GDP and infant mortality
attach(UN) # Make the variables in the data frame available
plot(gdp,infant.mortality)

# 'Cooler' syntax, same effect
detach(UN) # Undo 'attach(UN)'
plot(infant.mortality~gdp,data=UN)
# 'data=UN' tells R to fetch the variables from data frame 'UN'

# Now on a logarithmic scale
plot(infant.mortality~gdp,data=UN,log="xy")

# A scatterplot matrix for a data frame
pairs(Florida[1:4])

# '~'-constructs can be used to select variables
pairs(~GORE+BUSH+BUCHANAN+NADER,data=Florida)

# Several ways to create a boxplot
# The data are about occupational prestige, average, income, and
# type of occupation
with(Prestige,plot(type,income))
with(Prestige,boxplot(type,income))
boxplot(income~type,data=Prestige)
plot(income~type,data=Prestige)

plot(income~type,data=Prestige)

#  A horizontal boxplot
boxplot(income~type,data=Prestige,horizontal=TRUE)

load("Chile2.RData")
plot(vote~age,data=Chile2)

plot(vote~region,data=Chile2)

load("BerkeleyPercTab.RData")
barplot(BerkeleyPercTab2,
        main="Admissions to UC Berkeley Grad School 1973",
        sub="Dark area represents percentage admitted")

barplot(BerkeleyPercTab,beside=TRUE,legend.text=TRUE,
        ylab="Percent admitted")

mosaicplot(~ Class + Sex +  Survived, data = Titanic,
           color = TRUE,
           main = "Survival on the Titanic")
