#######
### This file is plot the full dataset in both tile and dual plot format.
#######

library(ggplot2)
library(plotly)

#loading the full datasets
load("C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/final/full_diff1_dat.Rdata")
load("C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/final/full_diff2_dat.Rdata")


mean(diff1_dat$AvgNum)
mean(diff2_dat$AvgNum)
max(diff1_dat$AvgNum)
####
# Creating this plot for CLUSTERING COEFFICIENT 
####


# Trying to restructure the data a bit, to create the averages for 
# tiles that have the same diameter and number of triangles

#aggregating the data for the SIR Model
agg_datT <- aggregate(diff1_dat$AvgT,by=list(Diameter=diff1_dat$Diameter,CC=diff1_dat$CC),data=diff1_dat,FUN=mean)
names(agg_datT) <- c("Diameter", "ClusterCoeff", "AvgT")
agg_datN <- aggregate(diff1_dat$AvgNum,by=list(Diameter=diff1_dat$Diameter,CC=diff1_dat$CC),data=diff1_dat,FUN=mean)
names(agg_datN) <- c("Diameter", "ClusterCoeff", "AvgNum")

#aggregating the data for the SI Model
agg_datT2 <- aggregate(diff2_dat$AvgT,by=list(Diameter=diff2_dat$Diameter,CC=diff2_dat$CC),data=diff2_dat,FUN=mean)
names(agg_datT2) <- c("Diameter", "ClusterCoeff", "AvgT")
agg_datN2 <- aggregate(diff2_dat$AvgNum,by=list(Diameter=diff2_dat$Diameter,CC=diff2_dat$CC),data=diff2_dat,FUN=mean)
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
t <- diff1_dat$AvgNum
x1 <- diff1_dat$CC
x2 <- diff1_dat$AvgT

#Set up the plot area for two "crammed" plots
par(pty="m", plt=c(0, 1, 0, 1), omd=c(0.2,0.8,0.2,0.8),xpd=TRUE)
par(mfrow = c(1, 2))

#Plot x1 and x2 together
plot(x2, t, type="p", ylim = 1.5 * range(x1, x2), xlab="ClusterCoeff", ylab="AvgNum", main="", col=diff1_dat$Nodes+2, las=1)
mtext(side=1, "Average Time", line=2, cex=0.8)
mtext(side=1, "SIR Model", line=-12.5, cex=1.5)
plot(x1, t, type="p", ylim = 1.5 * range(x1, x2), yaxt="n", xlab="AvgT", ylab="", main="", col=diff1_dat$Nodes+2, las=1)
mtext(side=2, "Average Number infected", line=11.8, cex=0.8)
mtext(side=1, "ClusterCoeff", line=2, cex=0.8)
legend("bottomright", legend=c("12 nodes","10 nodes", "8 nodes","6 nodes"),pch=c(1,1,1,1), bg="white", lwd=c(1,1, 1), col=c("magenta","blue", "red","grey"), cex=0.9)

#joint plot for the SI Model
#Build the data
t <- diff2_dat$AvgNum
x1 <- diff2_dat$CC
x2 <- diff2_dat$AvgT

#Set up the plot area for two "crammed" plots
par(pty="m", plt=c(0, 1, 0, 1), omd=c(0.2,0.8,0.2,0.8),xpd=TRUE)
par(mfrow = c(1, 2))

#Plot x1 and x2 together
plot(x2, t, type="p", ylim = 1.5 * range(x1, x2), xlab="ClusterCoeff", ylab="AvgNum", main="", col=diff1_dat$Nodes+2, las=1)
mtext(side=1, "Average Time", line=2, cex=0.8)
mtext(side=1, "SI Model", line=-12.5, cex=1.5)
plot(x1, t, type="p", ylim = 1.5 * range(x1, x2), yaxt="n", xlab="AvgT", ylab="", main="", col=diff1_dat$Nodes+2, las=1)
mtext(side=2, "Average Number infected", line=12, cex=0.8)
mtext(side=1, "ClusterCoeff", line=2, cex=0.8)
legend("bottomright", legend=c("12 nodes","10 nodes", "8 nodes","6 nodes"),pch=c(1,1,1,1), bg="white", lwd=c(1,1, 1), col=c("magenta","blue", "red","grey"), cex=0.9)


