
x <- 3*(1:100)
x
# First element of x
x[1]
# First ten elements of x
x[1:10]
# All elements except the last 90
x[-(11:100)]

c(a=1,b=2)
x <- c(a=1,b=2)
names(x)
names(x) <- c("c","d")
names(x)

c(2,3,4)+1

1+c(2,3,4)

c(1,1,1,2,2,2) * c(1,2,3)

c(1,1,1,2,2,2,3) * c(1,2,3)

x <- -3:3
x

# is equal
x == 0

# is greater than
x > 0

# is smaller than
x < 0

# is greater than or equal
x >= 0

# is smaller than or equal
x <= 0

# is nonequal to
x != 0

x <- -3:3
x

# is not equal to
!(x == 0)

# is greater than -1 and smaller than 1
x > -1 & x < 1

# is greater than 1 and smaller than -1
x > 1 & x < -1

# is larger than 1 or smaller than -1
x > 1 | x < -1

# is larger than -1 or smaller than 1
x > -1 | x < 1

cos(pi)

cos

print(pi) #equivalent to just typing 'pi'

print

# Operators are functions, too (note the "backticks")
`+`

# Another way to define the square root:
sqrt <- function(x) x^(.5)

sqrt(4)

# To get the definition:
sqrt

# sqrt was already defined:
rm(sqrt)

sqrt(4)

sqrt