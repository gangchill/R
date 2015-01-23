# k-means example in one dimension
#n=400;
#k=4
#epsilon=0.01

#samples
#points=cbind(runif(n),runif(n))
#for testing
#points[which(points>0.4&points<0.6)]=points[which(points>0.4&points<0.6)]-0.2
#sizes=rnorm(n, mean=5, sd=3)
#sizes[which(sizes<0)]=0.1

#plot(points, cex=sizes/2, pch=20); points(centers, cex=2, pch=20,col=rgb(1,0,0))

#samples=points; weights=sizes; dist=euclid; k=4; epsilon=2

euclid=function(a,b){ return( sqrt(sum((a-b)^2)));}



kmeans=function(samples=points, weights=sizes, dist=euclid, k=4,epsilon=0.1){
#init centers
  n=nrow(samples)
  centers=samples[sample(1:n,k),]
#identify nearest center for this example
  findCenter=function(sample, centers){
    return(which.min(apply(centers,1, dist, sample)));
  }
  maxIteration=1000;
  change=2*epsilon;
  iteration=0;
  while(change>epsilon&& iteration<maxIteration){
  # find nearest center for each sample
    c=apply(samples,1, findCenter, centers);
    oldCenters=centers;
  # find new centers

    centers=t(sapply(1:k, function(x) {
      ci=which(c==x);
      if(length(ci)>1){
        return(
          apply(samples[ci,],2,function(x) return(sum(x*weights[ci])))/sum(weights[ci])
          );
      }else if(length(ci)==1){
        apply(t(as.matrix(samples[ci,])),2,function(x) return(sum(x*weights[ci])))/sum(weights[ci])
      }else{
        return(samples[sample(1:n,1),]);
      }
    }));

    change=sum(abs(oldCenters-centers));
    iteration=iteration+1;
  }

  clusters=apply(samples,1, findCenter, centers);
  return(list(centers=centers, samples=samples, weights=weights
              , clusters=clusters
              , clusterWeights=tapply(weights,clusters,sum)
              ));
}

#cl=kmeans()
#plot(cl$samples, cex=sqrt(cl$weights), pch=20); points(cl$centers, cex=sqrt(cl$clusterWeights),  pch=20,col=rgb(1,0,0,0.3))

clusterFileContent=function(file=gzfile("samples.tar.gz"),k=6, sep="\t",maxRead=10000){
  #add weighted clusters to level
  #return modified level
  addToLevel=function(clustering, stackLevel=NULL){
    if(is.null(stackLevel)){
      cat("create\n");
      return(list(samples=cl$centers, weights=cl$clusterWeights))
    }else{
      cat("add\n");
      return(list(samples=rbind(stackLevel$samples,cl$centers),
                  weights=c(stackLevel$weights, cl$clusterWeights)))
    }
  }
  
#"initialize" stack
  stack=c()

  #debug: clustered levels
  clevels=c();
  #open file for reading
  open(file)
  #gz files return filename as first line
  if(grep("gzfile", class(file))==1)  filename=readLines(file,1)

#max number of elements on one level and lines to read per call
  n=100; blocks=10;
  #count elements already read
  elementsRead=0;
  #false, when end of file reached
  hasMoreLines=T;
  while(hasMoreLines&&elementsRead<maxRead){
    #0th stack level
    samples=NULL; 
    weights=NULL;
    while(length(weights)<n){
      lines=sapply(readLines(file,n=blocks),strsplit,sep,fixed=T)
      hasMoreLines=(length(lines)==blocks);
      elementsRead=elementsRead+length(lines);
      if(is.null(samples)){samples=matrix(0,0,length(lines[[1]]))}
      for(line in lines){
        samples=rbind(samples,as.numeric(line));
      }
      weights=c(weights,rep(1,length(lines)))
    }
    #cluster 0th level
#    cat("clustering level",0,"\n");
    clevels=c(clevels,0);
    cl=kmeans(samples=samples, weights=weights, k=4*k)

    #add to first weighted stack level
    stack[[1]]=addToLevel(cl,stack[[1]])
    i=1;
    while(i<=length(stack) && length(stack[[i]]$weights)>=n){
      cat("clustering level",i,"\n");

      cl=kmeans(samples=stack[[i]]$samples, stack[[i]]$weights,k=4*k);
      stack[[i]]=list(samples=matrix(0,0,ncol(stack[[i]]$samples)), weights=c())
      clevels=c(clevels,i);
      level=NULL;
      if(i<length(stack)) level=stack[[i+1]];
      stack[[i+1]]=addToLevel(cl,level);
      i=i+1;
      for(l in 1:length(stack)){ cat(l,"=",length(stack[[l]]$weights),"; ")}
      cat("\n")
    }
  }
 close(file)
  collected=length(weights);
  i=1;
  while(i<length(stack)){
    while(collected<n && i<=length(stack)){
      samples=rbind(samples,stack[[i]]$samples);
      weights=c(weights,stack[[i]]$weights);
      stack[[i]]=list(samples=matrix(0,0,ncol(stack[[i]]$samples)), weights=c())
      collected=length(weights);
      cat(collected,"samples, level",i,"\n");
      i=i+1;
    }
    cat("clustering",collected,"samples up to level",i,"\n")
    cl=kmeans(samples=samples, weights=weights,k=4*k)
    samples=cl$centers;
    weights=cl$clusterWeights;
    collected=length(weights);

  }
  cat("final clustering of", length(weights),"samples\n");
  cl=kmeans(samples=samples,weights=weights,k=k)
  return(list(stack=stack,clevels=clevels, clustering=cl))
}
#debug
#pairs(samples,col=cl$clusters,pch=20,)

setwd("~/mydocs/bds/exercise/5/")
s=clusterFileContent(maxRead=4e4,k=10)
pairs(s$clustering$centers,pch=20,col=gray(0.5,0.5), cex=sqrt(s$clustering$weights)/500)
