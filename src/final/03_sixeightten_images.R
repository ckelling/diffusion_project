library(ggplot2)

load("C:/Users/ckell/OneDrive/Penn State/Research/RCode/7_31_Diffdata/Diff1data.Rdata")
load("C:/Users/ckell/OneDrive/Penn State/Research/RCode/7_31_Diffdata/Diff2data.Rdata")

#Totdat <- cbind(Totdat, cc)
#Totdatb <- cbind(Totdatb, cc)

plot(Totdat$Triangles, Totdat$cc)


# Trying to restructure the data a bit, to create the averages for 
# tiles that have the same diameter and number of triangles

#aggregating the data for the SIR Model
agg_datT <- aggregate(Totdat$AvgT,by=list(Diameter=Totdat$Diameter,Triangles=Totdat$Triangles),data=Totdat,FUN=mean)
names(agg_datT) <- c("Diameter", "Triangles", "AvgT")
agg_datN <- aggregate(Totdat$AvgNum,by=list(Diameter=Totdat$Diameter,Triangles=Totdat$Triangles),data=Totdat,FUN=mean)
names(agg_datN) <- c("Diameter", "Triangles", "AvgNum")

#aggregating the data for the SI Model
agg_datT2 <- aggregate(Totdatb$AvgT,by=list(Diameter=Totdatb$Diameter,Triangles=Totdatb$Triangles),data=Totdatb,FUN=mean)
names(agg_datT2) <- c("Diameter", "Triangles", "AvgT")
agg_datN2 <- aggregate(Totdatb$AvgNum,by=list(Diameter=Totdatb$Diameter,Triangles=Totdatb$Triangles),data=Totdatb,FUN=mean)
names(agg_datN2) <- c("Diameter", "Triangles", "AvgNum")

#creating the tile plots to see Diameter vs the Number of Triangles

#plots for the SIR Model
ggplot(agg_datT, aes(Diameter, Triangles)) +
  geom_raster(aes(fill = AvgT), interpolate = FALSE)+ labs(title = "SIR Model- Avg Time of Dispersion")+
  geom_text(aes(label=round(AvgT,3)))
ggplot(agg_datN, aes(Diameter, Triangles)) +
  geom_raster(aes(fill = AvgNum), interpolate = FALSE)+ labs(title = "SIR Model- Avg # Infected")+
  geom_text(aes(label=round(AvgNum,3)))

#plots for the SI Model
ggplot(agg_datT2, aes(Diameter, Triangles)) +
  geom_raster(aes(fill = AvgT), interpolate = FALSE)+ labs(title = "SI Model- Avg Time of Dispersion")+
  geom_text(aes(label=round(AvgT,3)))
ggplot(agg_datN2, aes(Diameter, Triangles)) +
  geom_raster(aes(fill = AvgNum), interpolate = FALSE)+ labs(title = "SI Model- Avg # Infected")+
  geom_text(aes(label=round(AvgNum,3)))


#joint plot for the SIR Model
#Build the data
t <- Totdat$AvgNum
x1 <- Totdat$Triangles
x2 <- Totdat$AvgT

#Set up the plot area for two "crammed" plots
par(pty="m", plt=c(0, 1, 0, 1), omd=c(0.2,0.8,0.2,0.8),xpd=TRUE)
par(mfrow = c(1, 2))

#Plot x1 and x2 together
plot(x2, t, type="p", ylim = 1.5 * range(x1, x2), xlab="Triangles", ylab="AvgNum", main="", col=Totdat$Nodes+2, las=1)
mtext(side=1, "Average Time", line=2, cex=0.8)
mtext(side=1, "SIR Model", line=-14.5, cex=1.5)
plot(x1, t, type="p", ylim = 1.5 * range(x1, x2), yaxt="n", xlab="AvgT", ylab="", main="", col=Totdat$Nodes+2, las=1)
mtext(side=2, "Average Number infected", line=11.8, cex=0.8)
mtext(side=1, "Triangles", line=2, cex=0.8)
legend("bottomright", legend=c("10 nodes", "8 nodes","6 nodes"),pch=c(1,1,1), bg="white", lwd=c(1,1, 1), col=c("blue", "red","grey"), cex=0.9)

#joint plot for the SI Model
#Build the data
t <- Totdatb$AvgNum
x1 <- Totdatb$Triangles
x2 <- Totdatb$AvgT

#Set up the plot area for two "crammed" plots
par(pty="m", plt=c(0, 1, 0, 1), omd=c(0.2,0.8,0.2,0.8),xpd=TRUE)
par(mfrow = c(1, 2))

#Plot x1 and x2 together
plot(x2, t, type="p", ylim = 1.5 * range(x1, x2), xlab="Triangles", ylab="AvgNum", main="", col=Totdat$Nodes+2, las=1)
mtext(side=1, "Average Time", line=2, cex=0.8)
mtext(side=1, "SI Model", line=-14.5, cex=1.5)
plot(x1, t, type="p", ylim = 1.5 * range(x1, x2), yaxt="n", xlab="AvgT", ylab="", main="", col=Totdat$Nodes+2, las=1)
mtext(side=2, "Average Number infected", line=12, cex=0.8)
mtext(side=1, "Triangles", line=2, cex=0.8)
legend("bottomright", legend=c("10 nodes", "8 nodes","6 nodes"),pch=c(1,1,1), bg="white", lwd=c(1,1, 1), col=c("blue", "red","grey"), cex=0.9)



####
# Creating this plot for CLUSTERING COEFFICIENT instead of TRIANGLES
####


# Trying to restructure the data a bit, to create the averages for 
# tiles that have the same diameter and number of triangles

#aggregating the data for the SIR Model
agg_datT <- aggregate(Totdat$AvgT,by=list(Diameter=Totdat$Diameter,cc=Totdat$cc),data=Totdat,FUN=mean)
names(agg_datT) <- c("Diameter", "ClusterCoeff", "AvgT")
agg_datN <- aggregate(Totdat$AvgNum,by=list(Diameter=Totdat$Diameter,cc=Totdat$cc),data=Totdat,FUN=mean)
names(agg_datN) <- c("Diameter", "ClusterCoeff", "AvgNum")

#aggregating the data for the SI Model
agg_datT2 <- aggregate(Totdatb$AvgT,by=list(Diameter=Totdatb$Diameter,cc=Totdatb$cc),data=Totdatb,FUN=mean)
names(agg_datT2) <- c("Diameter", "ClusterCoeff", "AvgT")
agg_datN2 <- aggregate(Totdatb$AvgNum,by=list(Diameter=Totdatb$Diameter,cc=Totdatb$cc),data=Totdatb,FUN=mean)
names(agg_datN2) <- c("Diameter", "ClusterCoeff", "AvgNum")

#creating the tile plots to see Diameter vs the Number of Triangles

#plots for the SIR Model
ggplot(agg_datT, aes(Diameter, ClusterCoeff)) +
  geom_tile(aes(fill = AvgT, height=0.05))+ labs(title = "SIR Model- Avg Time of Dispersion")+
  geom_text(aes(label=round(AvgT,3), size=3))#+coord_fixed(ratio=10)

ggplot(agg_datN, aes(Diameter, ClusterCoeff)) +
  geom_tile(aes(fill = AvgNum, height=0.05))+ labs(title = "SIR Model- Avg # Infected")+
  geom_text(aes(label=round(AvgNum,3), size=3))#+coord_fixed(ratio=10)


#plots for the SI Model
ggplot(agg_datT2, aes(Diameter, ClusterCoeff)) +
  geom_tile(aes(fill = AvgT, height=0.05))+ labs(title = "SI Model- Avg Time of Dispersion")+
  geom_text(aes(label=round(AvgT,3)),size=4.5)#+coord_fixed(ratio=10)

ggplot(agg_datN2, aes(Diameter, ClusterCoeff)) +
  geom_tile(aes(fill = AvgNum, height=0.05))+ labs(title = "SI Model- Avg # Infected")+
  geom_text(aes(label=round(AvgNum,3), size=3))#+coord_fixed(ratio=10)

#joint plot for the SIR Model
#Build the data
t <- Totdat$AvgNum
x1 <- Totdat$cc
x2 <- Totdat$AvgT

#Set up the plot area for two "crammed" plots
par(pty="m", plt=c(0, 1, 0, 1), omd=c(0.2,0.8,0.2,0.8),xpd=TRUE)
par(mfrow = c(1, 2))

#Plot x1 and x2 together
plot(x2, t, type="p", ylim = 1.5 * range(x1, x2), xlab="ClusterCoeff", ylab="AvgNum", main="", col=Totdat$Nodes+2, las=1)
mtext(side=1, "Average Time", line=2, cex=0.8)
mtext(side=1, "SIR Model", line=-14.5, cex=1.5)
plot(x1, t, type="p", ylim = 1.5 * range(x1, x2), yaxt="n", xlab="AvgT", ylab="", main="", col=Totdat$Nodes+2, las=1)
mtext(side=2, "Average Number infected", line=11.8, cex=0.8)
mtext(side=1, "ClusterCoeff", line=2, cex=0.8)
legend("bottomright", legend=c("10 nodes", "8 nodes","6 nodes"),pch=c(1,1,1), bg="white", lwd=c(1,1, 1), col=c("blue", "red","grey"), cex=0.9)

#joint plot for the SI Model
#Build the data
t <- Totdatb$AvgNum
x1 <- Totdatb$cc
x2 <- Totdatb$AvgT

#Set up the plot area for two "crammed" plots
par(pty="m", plt=c(0, 1, 0, 1), omd=c(0.2,0.8,0.2,0.8),xpd=TRUE)
par(mfrow = c(1, 2))

#Plot x1 and x2 together
plot(x2, t, type="p", ylim = 1.5 * range(x1, x2), xlab="ClusterCoeff", ylab="AvgNum", main="", col=Totdat$Nodes+2, las=1)
mtext(side=1, "Average Time", line=2, cex=0.8)
mtext(side=1, "SI Model", line=-14.5, cex=1.5)
plot(x1, t, type="p", ylim = 1.5 * range(x1, x2), yaxt="n", xlab="AvgT", ylab="", main="", col=Totdat$Nodes+2, las=1)
mtext(side=2, "Average Number infected", line=12, cex=0.8)
mtext(side=1, "ClusterCoeff", line=2, cex=0.8)
legend("bottomright", legend=c("10 nodes", "8 nodes","6 nodes"),pch=c(1,1,1), bg="white", lwd=c(1,1, 1), col=c("blue", "red","grey"), cex=0.9)

#citation("ggplot2")

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density))

cars <- ggplot(mtcars, aes(mpg, factor(cyl)))
cars + stat_bin2d(aes(fill = ..count..), binwidth = c(3,1))
