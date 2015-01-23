library(lattice)
## Tonga Trench Earthquakes

Depth <- equal.count(quakes$depth, number=8, overlap=.1)
Earthquakes <- xyplot(lat ~ long | Depth, data = quakes)
print(Earthquakes)

states <- data.frame(state.x77,
            state.name = dimnames(state.x77)[[1]],
            state.region = state.region)
Murder <- xyplot(Murder ~ Population | state.region,
    data = states,
    groups = state.name,
    aspect=1,
    panel = function(x, y, subscripts, groups)
        ltext(x = x, y = y,
              label = groups[subscripts],
              cex=.7)
    )
print(Murder)


library(lattice)
load("perc-churchat-denom-year.RData")
# File contains data frame perc.churchat.denom.year


xyplot(percentage~year|denom*region,
       data=perc.churchat.denom.year,
       groups=churchat,
       type="b",
       auto.key=TRUE)


xyplot(percentage~year|denom*churchat,
       data=perc.churchat.denom.year,
       groups=region,
       type="b",
       auto.key=TRUE)



xyplot(percentage~year|churchat*denom,
       data=perc.churchat.denom.year,
       groups=region,
       type="b",
       auto.key=TRUE)




load(file="PoP-Percdiffs.RData")
library(lattice)
xyplot(
   percdiff.raw ~Year | source,
   data=selfempl.percdiffs
 )


xyplot(
    percdiff.raw +
    percdiff.model + percdiff.model.lower +
    percdiff.model.upper
      ~Year | source,
    data=selfempl.percdiffs
 )


xyplot(
    percdiff.raw +
    percdiff.model + percdiff.model.lower +
    percdiff.model.upper
      ~Year | source,
    data=selfempl.percdiffs,
    type="b", lwd=1.5, lty=c(0,1,2,2), ###
    cex=c(.3,0,0,0)                    ###
)



xyplot(
    percdiff.raw +
    percdiff.model + percdiff.model.lower +
    percdiff.model.upper
      ~Year | source,
    data=selfempl.percdiffs,
    type="b", lwd=1.5, lty=c(0,1,2,2),
    cex=c(.3,0,0,0),
    ylab="Percentage difference", ylim=c(-5,85)  ###
 )


xyplot(
    percdiff.raw +
    percdiff.model + percdiff.model.lower +
    percdiff.model.upper
      ~Year | source,
    data=selfempl.percdiffs,
    type="b", lwd=1.5, lty=c(0,1,2,2),
    cex=c(.3,0,0,0),
    ylab="Percentage difference", ylim=c(-5,85),
    as.table=TRUE,    layout=c(4,2)              ###
 )



xyplot(
    percdiff.raw +
    percdiff.model + percdiff.model.lower +
    percdiff.model.upper
      ~Year | source,
    data=selfempl.percdiffs,
    type="b", lwd=1.5, lty=c(0,1,2,2),
    cex=c(.3,0,0,0),
    ylab="Percentage difference", ylim=c(-5,85),
    as.table=TRUE,    layout=c(4,2),
    aspect=1                                   ###
 )


xyplot(
    percdiff.raw +
    percdiff.model + percdiff.model.lower +
    percdiff.model.upper
      ~Year | source,
    data=selfempl.percdiffs,
    type="b", lwd=1.5, lty=c(0,1,2,2),
    cex=c(.3,0,0,0),
    ylab="Percentage difference", ylim=c(-5,85),
    as.table=TRUE,    layout=c(4,2),
    aspect=1,
    par.settings=list(                                 ###
      superpose.symbol=list(                           ###
        col=c("gray60","black","black","black")  ),    ###
      superpose.line=list(col=c("gray50","black","black","black"))
    )                                                  ###
  )


xyplot(
    percdiff.raw +
    percdiff.model + percdiff.model.lower +
    percdiff.model.upper
      ~Year | source,
    data=selfempl.percdiffs,
    type="b", lwd=1.5, lty=c(0,1,2,2),
    cex=c(.3,0,0,0),
    ylab="Percentage difference", ylim=c(-5,85),
    as.table=TRUE,    layout=c(4,2),
    aspect=1,
    par.settings=list(
      superpose.symbol=list(
        col=c("gray60","black","black","black")  ),
      superpose.line=list(col=c("gray50","black","black","black"))
    ),
        panel=function(x,y,...){                     ###
                  panel.grid(h=-1,v=0)               ###
                  panel.superpose(x,y,...)           ###
              }                                      ###
 )

######################

## Earthquakes by their coordinates
qua <- xyplot(lat ~ long | cut(depth, 2), quakes,
              aspect = "iso", pch = ".", cex = 2)
qua
## add layer showing distance from Auckland
newdat <- with(quakes, expand.grid(
  gridlat = seq(min(lat), max(lat), length = 60),
  gridlon = seq(min(long), max(long), length = 60)))
newdat <- within(newdat, dist <- sqrt((gridlat - -36.87)^2 +
                                      (gridlon - 174.75)^2))
qua + layer_(panel.contourplot(x = gridlon, y = gridlat, z = dist,
                               contour = TRUE, subscripts = TRUE),
                               data = newdat)


######################
 
load(file="PoP-Percdiffs.RData")
library(lattice)
library(latticeExtra)

(p <- xyplot(
  percdiff.raw  ~Year | source,
  data=selfempl.percdiffs,
  cex=c(.3),
  ylab="Percentage difference", ylim=c(-5,85),
  as.table=TRUE,
  layout=c(4,2),
  aspect=1,
  col="gray60"
  ))

  
(p <- p + as.layer(xyplot(
  percdiff.model~Year | source,
  type="l",col="black",lwd=1.5,
  data=selfempl.percdiffs)
))


(p <- p + as.layer(xyplot(
  percdiff.model.upper+percdiff.model.lower~Year | source,
  type="l",col="black",lwd=1.5,lty=2,
  data=selfempl.percdiffs)
))


(p <- p + layer(panel.grid(h=-1,v=0)))


update(p,par.settings=theEconomist.theme())


asTheEconomist(p,type="p")
