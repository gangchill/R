x <- rep(1:3,5)
x

f <- factor(x,levels=c(1,2,3),
      labels=c("A", "B", "C"))
f

o <- ordered(x,levels=c(1,2,3),
      labels=c("A", "B", "C"))
o

FDR <- list(firstname = c("Franklin","Delano"),
          lastname = "Roosevelt"
          )
FDR

FDR$firstname

FDR$lastname

FDR[2]

FDR[[2]]

FDR[["lastname"]]

length(FDR)

# This gives the first 15 `rows' of the data set on
# U.S. State Judge ratings
USJudgeRatings[1:15,]

# This gives a different look on the data set on
# U.S. State Judge ratings
str(USJudgeRatings)

x <- rnorm(n=50,mean=1,sd=2)
a <- factor(rep(1:5,10),labels=c("A","B","C","D","E"))
b <- factor(rep(1:2,c(25,25)),labels=c("+","-"))
TDF <- data.frame(x,a,b)
TDF[1:8,]

str(TDF)

# Accessing two variables using '$'
mean(USJudgeRatings$CONT) + mean(USJudgeRatings$INTG)

# Accessing two variables using 'attach'
attach(USJudgeRatings)
mean(CONT) + mean(INTG)

detach(USJudgeRatings)

# Accessing two variables using 'with'
with(USJudgeRatings,{
  mean(CONT) + mean(INTG)
})


library(foreign)
lijphart <- read.dta("lijphart.dta")
attach(lijphart)
table(FEDERALI)

100 * prop.table(table(FEDERALI))

mean(FEDERALI)
median(FEDERALI)

c(mean = mean(FEDERALI), median = median(FEDERALI),
    stddev = sd(FEDERALI),
    min = min(FEDERALI),
    max = max(FEDERALI))

cor(MINWIN, EFFNUMPA)

cor(MINWIN, EFFNUMPA, method = "kendall")

cor(MINWIN, EFFNUMPA, method = "spearman")

cov(MINWIN, EFFNUMPA)

detach(lijphart)

lijphart1 <- lijphart[-1]

lijphart1[1:5, 1:2] <- NA
lijphart1[1:10, ]

cor(lijphart1)
cor(lijphart1, use = "complete")
cor(lijphart1, use = "pairwise")
cov(lijphart1, use = "pairwise")

load("Berkeley.RData")
attach(Berkeley)

table(Admit,Gender)

table(Admit,Gender,Dept)

BerkeleyTable <- table(Admit,Gender,Dept)

100*prop.table(table(Admit,Gender),2)

margin.table(BerkeleyTable,2:3)

100*prop.table(BerkeleyTable,2:3)

detach(Berkeley)

BerkeleyTable <- xtabs(~Admit+Gender+Dept,
                               data=Berkeley)
BerkeleyTable

as.data.frame(BerkeleyTable)

xtabs(~Admit+Gender,data=as.data.frame(BerkeleyTable))

xtabs(Freq~Admit+Gender,
            data=as.data.frame(BerkeleyTable))

xtabs(Freq~Admit+Gender,data=BerkeleyTable)

ftable(BerkeleyTable,col.vars=c("Gender","Admit"))

ftable(100*prop.table(BerkeleyTable,2:3),
      col.vars=c("Gender","Admit"))

lijphart <- read.csv("lijphart.csv")
library(memisc)

genTable(mean(MINWIN)~cut(EFFNUMPA,5),data=lijphart)

genTable(mean(MINWIN)~cut(EFFNUMPA,5)+cut(EXDOM,5),
                                     data=lijphart)

genTable(mean(MINWIN)~cut(EFFNUMPA,5)+cut(EXDOM,3),
                                     data=lijphart)

genTable(c(mean(MINWIN),sd(MINWIN))~cut(EFFNUMPA,5),
                                     data=lijphart)

load("Berkeley.RData")

genTable(percent(Admit)~Gender,data=Berkeley)

t(genTable(percent(Admit)~Gender,data=Berkeley))

