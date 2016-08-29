add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


# library(shape)

par(ps = 14, cex = 1.5, cex.lab = 2)
svg("schematic_deployments.svg",
    width = 6, height = 4, family = "serif")
pdf("schematic_deployments.pdf",
    width = 6, height = 4, family = "serif")
png("schematic_deployments.png",
    width = 6, height = 4, res = 600, family = "serif",
    units = "in")
# ?png
# ?cairo_ps
# Plot base map
par(mfrow = c(1,1))
par(mar=c(4,8, 1, 1) + 0.1)  


plot(c(1:6,1:2)~c(0:7),
     type = "n",
     xlab = "Days",
     ylab = "",
     # xaxt = "n",
     yaxt = "n",
     cex.lab = 1.3
     )
grid(ny = NA)
# ?Arrows
# ?grid()
arrows( .5,5,3.5,5,
         lwd = 3, col = add.alpha("black", 0.8))
arrows( 3.5,4,6.5,4,
       lwd = 3, col = add.alpha("black", 0.8))
arrows( .5,4,3.5,4,
       lwd = 3, lty = 2,
       col = add.alpha("black", 0.8))
arrows( 3.2,4,3.5,4,
        lwd = 3,
        col = add.alpha("black", 0.8))
arrows( 3.5,5,6.5,5,
       lwd = 3, lty = 2,
       col = add.alpha("black", 0.8))
arrows( 6.2,5,6.5,5,
        lwd = 3, 
        col = add.alpha("black", 0.8))
segments(-1,5,0.5,5,
         lwd = 3,
         lty = 3,
         col = add.alpha("dark grey", 0.6))
segments(6.5,5,8,5,
         lwd = 3,
         lty = 3,
         col = add.alpha("dark grey", 0.6))
segments(-1,4,0.5,4,
         lwd = 3,
         lty = 3,
         col = add.alpha("dark grey", 0.6))
segments(6.5,4,8,4,
         lwd = 3,
         lty = 3,
         col = add.alpha("dark grey", 0.6))

# Controls
arrows( 0.5,2.5,3.5,2.5,
       lwd = 3, lty = 2,
       col = add.alpha("dark grey", 1))
arrows( 3.2,2.5,3.5,2.5,
        lwd = 3, 
        col = add.alpha("dark grey", 1))
segments(-1,2.5,0.5,2.5,
         lwd = 3,
         lty = 3,
         col = add.alpha("dark grey", 0.6))
segments(3.5,2.5,8,2.5,
         lwd = 3,
         lty = 3,
         col = add.alpha("dark grey", 0.6))

segments(-1,1.5,8,1.5,
         lwd = 3,
         lty = 3,
         col = add.alpha("dark grey", 0.6))


point.loc <- c(1:7)-0.5
point.loc <- point.loc + (sample(c(20:80),7)/100 - 0.5)
points(point.loc, rep(1.5, 7),
       pch = 21,
       bg = "grey",
       col = "black",
       lwd = 2)
  

# Capture events
segments(0.5,2,0.5,5.5,
         lwd = 2,
         lty = 3,
         col = add.alpha("black", 0.6))
segments(3.5,2,3.5,5.5,
         lwd = 2,
         lty = 3,
         col = add.alpha("black", 0.6))
segments(6.5,3.5,6.5,5.5,
         lwd = 2,
         lty = 3,
         col = add.alpha("black", 0.6))

# Text
mtext("Experimental", side = 2,
      las = 1, at = 5.5, line = 6, adj = 0.2,
      cex = 1.3)
mtext("GPS+TDR 1st", side = 2,
      las = 1, at = 5, line = 5, adj = 0.2,
      cex = 1.2)
mtext("GPS+TDR 2nd", side = 2,
      las = 1, at = 4, line = 5, adj = 0.2,
      cex = 1.2)

mtext("Controls", side = 2,
      las = 1, at = 3, line = 6, adj = 0.2,
      cex = 1.3)
mtext("Full", side = 2,
      las = 1, at = 2.5, line = 5, adj = 0.2,
      cex = 1.2)
mtext("Background", side = 2,
      las = 1, at = 1.5, line = 5, adj = 0.2,
      cex = 1.2)


# On plot
text(0.5,5.7,"Capt. 1",
      cex = 1.2)
text(3.5,5.7,"Capt. 2",
     cex = 1.2)
text(6.5,5.7,"Capt. 3",
     cex = 1.2)

text(2,5.2,"GPS+TDR",
     cex = 1.2)
text(5,4.2,"GPS+TDR",
     cex = 1.2)

text(5,5.2,"TDR",
     cex = 1.2)
text(2,4.2,"TDR",
     cex = 1.2)

text(2,2.7,"None",
     cex = 1.2,
     col = "dark grey")

dev.off()
# ?sample
