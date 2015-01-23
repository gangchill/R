### Changing vektors and lists #####################################################

x <- rnorm(n=8)
y <- 5*x + 1

names(y) <- letters[1:8]
length(y) <- 10

names(y)
length(y)

x <- rnorm(n=20)
x[1:10]

# Setting some elements to zero
x[1:5] <- 0
x[1:10]

# Assigning names
names(x) <- letters[1:20]
# Changing names 
names(x)[1:4] <- LETTERS[1:4]
x[1:10]

# Making the object shorter
length(x) <- 8
x

# Making the object longer (and filling in NAs)
length(x) <- 12
x[6:12]

# Filling the missing values
# Note that is.na() returns a logical vector
x[is.na(x)] <- rnorm(n=sum(is.na(x)))
x

# ``Censoring'' the random numbers below zero
x[x<0] <- 0
x

# See what happens if we assing beyond the length
length(x)<- 2
x[6]
x[6] <- 0
x

lili <- list(a=1:6,b=c("Agatha","Bertha"))
lili

lili[[2]] <- letters[8:19]
lili

# Another list as a list element
lili[[3]] <- list(x=rnorm(n=3),y=rchisq(n=4,df=3))
lili

# Changing an element range of the list
lili[3:4] <- list(x=rnorm(n=3),y=rchisq(n=4,df=3))
lili

# Deleting elements from the list
lili[2:3] <- NULL
lili

names(lili) <- c("a","b")
lili$b <- lili$b - mean(lili$b)
lili

### Modifying variables in data frames ############################################

attach(iris)
Sepal.Length <- Sepal.Length - mean(Sepal.Length)
mean(Sepal.Length)

detach(iris)
mean(Sepal.Length)

mean(iris$Sepal.Length)

rm(Sepal.Length)

x <- rnorm(n=50,mean=1,sd=2)
a <- factor(rep(1:5,10),labels=c("A","B","C","D","E"))
b <- factor(rep(1:2,c(25,25)),labels=c("+","-"))
TDF <- data.frame(x,a,b)

rm(x,a,b)

with(TDF,mean(x))

TDF <- within(TDF,{
  z <- (x - mean(x))/sd(x)
  x.cens <- x
  x.cens[x<0] <- 0
})

# No variable z (unless there was one before)
try(mean(z))

with(TDF,mean(z))

with(TDF,table(x.cens>0))

### Recoding #################################################

df <- data.frame(a = 1:7, f = factor(letters[1:7]))
df

# Recoding using built-in facilities
# Recoding `a` into three categories 
# 1,2,3 -> 1
# 4,5   -> 2
# 6,7   -> 3

df <- within(df,{
  
  # Variant 1
  a3.1 <- a
  a3.1[a %in% c(1,2,3)] <- 1
  a3.1[a %in% c(4,5)]   <- 2
  a3.1[a %in% c(6,7)]   <- 3
  
  # Variant 2
  a3.2 <- ( 1*(a %in% c(1,2,3))
           +2*(a %in% c(4,5))
           +3*(a %in% c(6,7))
          )
  
  # Variant 3
  a3.3 <- ifelse(a %in% c(1,2,3),1,
                 ifelse(a %in% c(4,5),2,
                        ifelse(a %in% c(6,7),3,NA)))
})
df

# Recoding `f` into three categories 
# a,b,c -> a
# d,e   -> b
# f,g   -> c

df <- within(df,{
  
  # Variant 1
  f3.1 <- f
  f3.1[f %in% c("a","b","c")] <- "a"
  f3.1[f %in% c("d","e")]   <- "b"
  f3.1[f %in% c("f","g")]   <- "c"
  
  # Variant 2
  f3.2 <- factor( 1*(f %in% c("a","b","c"))
            +2*(f %in% c("d","e"))
            +3*(f %in% c("f","g")),
                  labels=c("a","b","c"))
  
  # Variant 3
  f3.3 <- ifelse(f %in% c("a","b","c"),"a",
                 ifelse(f %in% c("d","e"),"b",
                        ifelse(f %in% c("f","g"),"c",NA)))
})
df

# Recoding with `memisc`
library(memisc)
df <- df[1:2]
df <- within(df,{
             a3 <- recode(a,
                          c(1,2,3)->1,
                          c(4,5)  ->2,
                          c(6,7)  ->3)
             f3 <- recode(f,
                          c("a","b","c")->"a",
                          c("d","e")    ->"b",
                          c("f","g")    ->"c")
})
df

# Various options of `recode`
df <- data.frame(a = 1:7, f = factor(letters[1:7]))
within(df,
    r <- recode(a,  1  -> 5,
                    2:3-> 2,
                    7  -> 6))

within(df,
    r <- recode(a,  1  -> 5,
                    2:3-> 2,
                    7  -> 6,
                    otherwise="copy"))

within(df,
    r <- recode(a,  1  -> 5,
                    2:3-> 2,
                    7  -> 6,
                    otherwise=100))

within(df,
    r <- recode(a,  1   -> "a",
                    2:3 -> "b",
                    7   -> "c"))

within(df,
    r <- recode(f,letters[1:3] -> "a",
                  letters[4:5] -> "b",
                  "f"          -> "c",
                  otherwise="z"))

# Another little helper from `memisc`

df <- data.frame(x=rnorm(n=300),
                 y=rnorm(n=300,mean=1,sd=3))

# `cases` creates a factor with automatically
# generated labels
df <- within(df,{
  x4 <- cases(x < -1, 
              x >= -1 & x < 0,
              x >= 0 & x < 1,
              x >= 1)
  y4 <- cases(y < -1, 
              y >= -1 & y < 0,
              y >= 0 & y < 1,
              y >= 1)
})
with(df,table(x4,y4))

# `cases` creates a factor with user-provided
# labels
df <- within(df,{
  x4 <- cases(a = x < -1, 
              b = x >= -1 & x < 0,
              c = x >= 0 & x < 1,
              d = x >= 1)
  y4 <- cases(a = y < -1, 
              b = y >= -1 & y < 0,
              c = y >= 0 & y < 1,
              d = y >= 1)
})
with(df,table(x4,y4))
str(df)

# `cases` creates a numeric vector
df <- within(df,{
  x4 <- cases(1 <- x < -1, 
              2 <- x >= -1 & x < 0,
              3 <- x >= 0 & x < 1,
              4 <- x >= 1)
  y4 <- cases(1 <- y < -1, 
              2 <- y >= -1 & y < 0,
              3 <- y >= 0 & y < 1,
              4 <- y >= 1)
})
with(df,table(x4,y4))
str(df)