# The following lines create a pdf file that contains
# graphics output 6 inches wide and 6 inches high.

pdf(file="hist1.pdf",width=6,height=6)

# Now the histogram is created.
hist(rnbinom(n=1000,size=1,prob=1/6))

# The file is closed and plotting is finished.
dev.off()

# The following lines create a png file that contains
# graphics output 480 pixels wide and 480 pixels high,
# and 12 point text size.

png(file="hist1.png",width=480,height=480,pointsize=12)

# Now the histogram is created.
hist(rnbinom(n=1000,size=1,prob=1/6))

# The file is closed and plotting is finished.
dev.off()




