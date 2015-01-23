# A comment line is started by `#'.
# Everything after `#' is ignored by R.

# Clear the workspace
rm(list=ls())

a <- 50
a
 
# This function call lists all user defined objects
ls()

Beatles <- c("John","Paul","George","Ringo")
`Sgt. Peppers Lonely Heart's Club Band` <- Beatles
`Sgt. Peppers Lonely Heart's Club Band`


a <- 1
b <- 2
# Save objects `a' and `b' in file `example.RData'
save(a,b,file="example.RData")
# Delete `a' and `b'
rm(a,b)
ls()


load("example.RData")
# Are the objects back?
print(a)


# A new possibility how to save objects since
# R 2.13.0
saveRDS(a,"a.RData")
saveRDS(b,"b.RData")
rm(a,b)
ls()

# A new possibility to restore objects since
# R 2.13.0
a <- readRDS("a.RData")
b <- readRDS("b.RData")
print(a)