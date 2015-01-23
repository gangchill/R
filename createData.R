#generate csv files with data points containing actual clusters

#10 clusters in 5 dimensional space, last number is standard deviation
centers=t(sapply(1:10,function(c) c(c,runif(5, min=-200,max=500), runif(1,10,100))))
colnames(centers)=c("center",paste("x",1:5,sep=""),"sd")
#use a skewed distribution to centers
centerProb=runif(1:nrow(centers))

#each iteration of this creates a million samples
#compression with tar and gnuzip leads to about 50% rate
#7 iterations + one "noise" round used
#ssize=runif(1, min=500,max=500000)
ssize=1e6
sampleCenters=sample(1:nrow(centers),ssize, replace=T, prob=centerProb)
samples=cbind(sapply(2:(ncol(centers)-1),function(i)             
             rnorm(length(sampleCenters),mean=centers[sampleCenters,i]
                   , sd=centers[sampleCenters,"sd"])))
write.table(samples, append=T, file="/tmp/samples.csv", col.names=F, row.names=F, sep="\t")


