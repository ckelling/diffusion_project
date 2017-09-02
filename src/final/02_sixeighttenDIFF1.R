##############################################################
###
### Simulation in 10 node graphs
###
##############################################################
library(igraph)
library(plotly)
source("C:/Users/ckell/OneDrive/Penn State/Research/RCode/DiffusionFunction1.R")

#Creating Graphs
#4 tri, 5 Diam, 1 cut
G1=graph.formula(1-2,1-3,1-4,2-3,2-5,3-4,4-5,5-6,6-7,6-9,7-8,7-10,8-9,8-10,9-10)
#4 tri, 4 Diam, 2 Cut
G2=graph.formula(1-2,1-6,1-10,2-3,2-4,3-5,3-4,4-5,5-6,6-7,7-8,7-9,8-10,8-9,9-10)
#3 tri, 4 Diam, 2 Cut
G3=graph.formula(1-2,1-10,1-8,2-3,2-4,3-4,3-5,4-5,5-6,6-10,6-7,7-9,7-8,8-9,9-10)
#2 tri, 4 diam, 3 cut
G4=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,2-5,3-7,4-6,7-10,1-9)
#4 tri, 3 diam, 2 cut
G5=graph.formula(1-2,1-9,1-10,2-3,2-4,3-4,4-5,3-5,5-6,6-7,6-8,7-8,7-10,8-9,9-10)
#3 tri, 3 diam, 3 cut
G6=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-9,2-4,3-8,5-7,6-10)
#2 tri, 3 diam, 3 cut
G7=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,2-10,3-9,4-8,5-7,1-6)
#2 tri, 3 diam, 3 cut
G8=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-6,2-4,3-8,5-10,7-9)
#2 tri, 3 diam, 3 cut
G9=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,2-10,3-8,4-6,5-9,1-7)
#2 tri, 3 diam, 2 cut
G10=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,2-4,3-5,6-9,7-10,1-8)
#1 tri, 3 diam, 3 cut
G11=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-5,2-4,3-8,6-9,7-10)
#1 tri, 3 diam, 3 cut
G12=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-4,2-9,3-8,5-7,6-10)
#1 tri, 3 diam, 3 cut
G13=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-7,2-4,3-8,5-9,6-10)
#0 tri, 3 diam, 3 cut
G14=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-4,2-5,3-8,5-9,6-10)
#0 tri, 3 diam, 3 cut
G15=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-5,10-6,2-9,3-8,4-7)
#0 tri, 3 diam, 3 cut
G16=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-5,2-7,3-8,4-9,10-6)
#0 tri, 3 diam, 3 cut
G17=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-6,2-7,3-8,4-9,5-10)
#0 tri, 3 diam, 3 cut
G18=graph.formula(1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-1,1-7,2-6,3-8,4-10,5-9)
#0 tri, 2 diam, 3 cut
G19=graph.formula(1-2,2-3,3-4,4-5,5-1,1-6,2-7,3-8,4-9,5-10,6-8,6-9,7-10,7-9,8-10)

netlist=list(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,G13,G14,G15,G16,G17,G18,G19)
cc <- rep(NA, 26)
for(i in 1:19){
  cc[i] <- transitivity(netlist[[i]])
}


# par(mfrow=c(2,3))
# plot(G1, main= "Diameter 5, 4 Triangle")
# plot(G2, main= "Diameter 4, 4 Triangle")
# plot(G3, main= "Diameter 4, 3 Triangle")
# plot(G4, main= "Diameter 4, 2 Triangle")
# plot(G5, main= "Diameter 3, 4 Triangle")
# plot(G6, main= "Diameter 3, 3 Triangle")
# plot(G7, main= "Diameter 3, 2 Triangle")
# plot(G8, main= "Diameter 3, 2 Triangle")
# plot(G9, main= "Diameter 3, 2 Triangle")
# plot(G10, main= "Diameter 3, 2 Triangle")
# plot(G11, main= "Diameter 3, 1 Triangle")
# plot(G12, main= "Diameter 3, 1 Triangle")
# plot(G13, main= "Diameter 3, 1 Triangle")
# plot(G14, main= "Diameter 3, 0 Triangle")
# plot(G15, main= "Diameter 3, 0 Triangle")
# plot(G16, main= "Diameter 3, 0 Triangle")
# plot(G17, main= "Diameter 3, 0 Triangle")
# plot(G18, main= "Diameter 3, 0 Triangle")
# plot(G19, main= "Diameter 2, 0 Triangle")


netlist=list(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,G13,G14,G15,G16,G17,G18,G19)
n=10000 #number of simulations
p=0.5 #probability of infection
AverageTime=rep(0,19)
AverageNumInf=rep(0,19)
par(mfrow=c(2,2))
for(i in 1:19){
  G=netlist[[i]]
  # Run the simulation
  sim=rep(0,n)
  Vinitial=rep(0,n)
  numinf1=rep(0,n)
  print(paste(i, "********************************************************"))
  for(j in 1:n){
    Vinitial[j] = sample(1:10,1)
    sim[j]=length(diffusion1(G,Vinitial[j],p))
    numinf1[j]=numinf
    if(j %% 100 == 0){
      print(j)
    }
  }
  AverageTime[i]=mean(sim)
  #hist(sim, main=c("Time of Diffusion for Graph",i))
  AverageNumInf[i]=mean(numinf1)
  #hist(numinf1, main = c("Number of People Infected for Graph",i))
}

# par(mfrow=c(2,1))
# for(i in 1:19){
#   plot(netlist[[i]])
# }

######################
## Simulating for 6-8 nodes
######################

#8 node, 3 Diam, 4 Tri
Gr1=graph.formula(1-2,1-3,1-4,2-3,2-4,3-5,4-6,5-8,5-7,6-7,6-8,7-8)
#8 node, 3 Diam, 2 Tri
Gr2=graph.formula(1-2,1-5,1-8,2-3,2-8,3-4,3-7,4-5,5-6,4-6,6-7,7-8)
#8 node, 3 Diam, 0 Tri, 2 cut
Gr3=graph.formula(1-2,1-7,1-3,2-8,2-4,3-4,3-5,4-6,5-6,5-7,6-8,7-8)
#8 node, 2 Diam, 1 Tri
Gr4=graph.formula(1-2,1-5,1-8,2-3,2-8,3-4,3-6,4-5,4-7,5-6,6-7,7-8)
#8 node, 2 Diam, 0 Tri
Gr5=graph.formula(1-2,1-5,1-8,2-3,2-6,3-4,3-7,4-5,4-8,5-6,6-7,7-8)
#6 node, 2 Diam, 2 Tri
Gr6=graph.formula(1-2,1-4,1-6,2-6,2-3,3-5,3-4,4-5,5-6)
#6 node, 2 Diam, 0 Tri
Gr7=graph.formula(1-2,1-4,1-6,2-5,2-3,3-6,3-4,4-5,5-6)

netlistb=list(Gr1,Gr2,Gr3,Gr4,Gr5,Gr6,Gr7)
for(i in 20:26){
  cc[i] <- transitivity(netlistb[[i-19]])
}

View(as.data.frame(cc))

#par(mfrow=c(2,3))
#plot(G6, main= "Diameter 2, 2 Triangle")
#plot(G7, main= "Diameter 2, 0 Triangle")


AverageTime68=rep(0,7)
AverageNumInf68=rep(0,7)
par(mfrow=c(2,2))
for(i in 1:5){
  G=netlistb[[i]]
  # Run the simulation
  sim=rep(0,n)
  Vinitial=rep(0,n)
  numinf1=rep(0,n)
  for(j in 1:n){
    Vinitial[j] = sample(1:8,1)
    sim[j]=length(diffusion1(G,Vinitial[j],p))
    numinf1[j]=numinf
  }
  AverageTime68[i]=mean(sim)
  #hist(sim, main=c("Time of Diffusion for Graph",i))
  AverageNumInf68[i]=mean(numinf1)
  print(i)
  #hist(numinf1, main = c("Number of People Infected for Graph",i))
}
for(i in 6:7){
  G=netlistb[[i]]
  # Run the simulation
  sim=rep(0,n)
  Vinitial=rep(0,n)
  numinf1=rep(0,n)
  for(j in 1:n){
    Vinitial[j] = sample(1:6,1)
    sim[j]=length(diffusion1(G,Vinitial[j],p))
    numinf1[j]=numinf
  }
  AverageTime68[i]=mean(sim)
  #hist(sim, main=c("Time of Diffusion for Graph",i))
  AverageNumInf68[i]=mean(numinf1)
  print(i)
  #hist(numinf1, main = c("Number of People Infected for Graph",i))
}


dat=cbind(c(5,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2),
          c(4,4,3,2,4,3,2,2,2,2,1,1,1,0,0,0,0,0,0),
          c(1,2,2,3,2,3,3,3,3,2,3,3,3,3,3,3,3,3,3))
dat=cbind(dat,AverageTime,AverageNumInf)


dat2=cbind(c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,8,8,8,8,8,6,6),
           c(5,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,3,3,3,2,2,2,2),
           c(4,4,3,2,4,3,2,2,2,2,1,1,1,0,0,0,0,0,0,4,2,0,1,0,2,0),
           c(1,2,2,3,2,3,3,3,3,2,3,3,3,3,3,3,3,3,3,3,3,2,3,3,3,3),
           cc)

AvgT=rep(0,26)
AvgT[0:19]=AverageTime
AvgT[20:26]=AverageTime68
AvgNum=rep(0,26)
AvgNum[1:19]=AverageNumInf
AvgNum[20:26]=AverageNumInf68

Totdat=cbind(dat2,AvgT,AvgNum)
#Totdat=Totdat[1:19,]
#Totdat=Totdat[20:26,]
colnames(Totdat)=c("Nodes","Diameter","Triangles","CutSet","AvgT","AvgNum")
Totdat=as.data.frame(Totdat)
#attach(Totdat)

# require(plotly)
# plot_ly(z = ~AvgT, x=~Nodes,y=~Triangles, type = "contour")
# plot_ly(z = ~AvgT, x=~Diameter,y=~Triangles, type = "contour")
# plot_ly(z= ~AvgNum, x=~Diameter, y=~Triangles, type="contour")
# 
# plot(x=Triangles, y=AvgT,col=Nodes+2,main="SIR Model")
# plot(x=AvgT,y=AvgNum,col=Nodes+2,main="SIR Model")
# plot(x=Diameter,y=AvgNum,col=Nodes+2,main="SIR Model")
# plot(x=Nodes,y=AvgT,main="SIR Model")

save(Totdat, file = "C:/Users/ckell/OneDrive/Penn State/Research/RCode/7_31_Diffdata/Diff1data.Rdata")
