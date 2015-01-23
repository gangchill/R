# Roll a die one time
result <- sample(1:6,size=1)
cat("The die reads ",result,".\n",sep="")
if(result == 6) cat("Yes! I win.\n") else
      cat("No! I lose.\n")

# Roll a die exactly ten times
for(i in 1:10){
      cat("Roll No.",i,"\t")
      result <- sample(1:6,size=1)
      cat("Got a",result,"\t")
      if(result == 6) cat("Yes! I win.\n") else
         cat("No! I lose.\n")
}

# Roll a die until you win
result <- 0
while(result != 6) {
      cat("Rolling the die ...")
      result <- sample(1:6,size=1)
      cat("Got a",result,"\n")
      if(result == 6) cat("I win --- eventually.\n")
}

# Roll two dice until you get two sixes
repeat{
      cat("Rolling two dice ... ")
      result <- sample(1:6,size=2,replace=TRUE)
      cat("The dice show",result[1],"and",
         result[2],"\n")
      if(all(result == c(6,6))) {
         cat("Yes! I win, I have two sixes!\n")
         break
      }
}

# Another example that we encountered already
pval <- function(x) round(coef(summary(x))[,4],3)


# A more complex example using loops and brances
wait.for.sixes <- function(times=1,silent=FALSE){
      if(times < 1) stop("`times' should be positive")
      have.sixes <- 0
      trials <- 0
      repeat{
        die.roll <- sample(1:6,size=1)
        trials <- trials + 1
        if(die.roll == 6) have.sixes <- have.sixes + 1
        if(have.sixes >= times) {
            if(!silent)
              cat("Had",times,"sixes in",trials,"trials.\n")
            return(trials)
        }
      }
   }

wait.for.sixes()

wait.for.sixes(1)

wait.for.sixes(10)

