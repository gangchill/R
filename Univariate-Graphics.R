set.seed(10)
# Roll a die 200 times:
x <- sample(1:6,size=200,replace=TRUE)
barplot(table(x))

# Plotting the distribution of factor levels
f <- factor(x)
plot(f)

# Get 100 values from a standard normal distribution:
x <- rnorm(n=100)
# Standard histogram
hist(x)

# A histogram with relative frequencies
hist(x,probability=TRUE)

# A histogram with fewer breaks
hist(x,breaks=5)

# A histogram with empirical data
# The data are average amounts of precipitation (rainfall) in 
# inches for each of 70 United States (and Puerto Rico) cities
hist(precip)

# Plot of the elements in 'x' in order of their occurrence
plot(x)

# Plot of the elements in 'x' in order of their size
plot(sort(x))

# Get 100 values from a standard normal distribution:
x <- rnorm(n=100)
# Normal quantile plot
qqnorm(x)
qqline(x)

# Normal quantile plot of rainfall in 70 US cities
qqnorm(precip)
qqline(precip)

# The data are (approximately) quarterly approval ratings for 
# US Presidents from the first quarter of 1945 to the last
# quarter of 1974
plot(presidents)

# A line plot
plot(as.numeric(presidents),type="l")
