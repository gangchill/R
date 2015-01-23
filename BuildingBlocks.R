# Create some data to plot
x <- seq(from=0,to=2*pi,length=101)
y <- sin(x) + rnorm(n=101,sd=.5)

plot.new()
plot.window(xlim=range(x),ylim=range(y))
# Add axes
# x axis
axis(1, at = c(0, pi/2, pi, 3/2*pi, 2*pi),
     labels = expression(0, pi/2, pi, 3/2*pi, 2*pi))
axis(2) # y axis
box() # Put a frame around the plot

# Add lines
lines(x,sin(x),lwd=2)

text(x=3/2*pi,y=1.5,
       bquote(
         paste(sin(3/4*pi)== 1/sqrt(2))
         == .(sin(3/4*pi))),
       pos=3)

arrows(x0=3/2*pi,x1=3/4*pi,y0=1.5,y1=sin(3/4*pi),lwd=2)

segments(x0=x,x1=x,y0=sin(x),y1=y)

points(x,y,pch=21,bg="green")
