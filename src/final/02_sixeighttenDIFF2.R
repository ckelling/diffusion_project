####
## Simulation Diffusion 2
####

source("C:/Users/ckell/OneDrive/Penn State/Research/RCode/DiffusionFunction2.R")
library(plotly)
library(igraph)

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
n=10000 #number of simulations
p=0.5 #probability of infection
AverageTimeb=rep(0,19)
AverageNumInfb=rep(0,19)
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
    sim[j]=length(diffusion2(G,Vinitial[j],p))
    numinf1[j]=numinf
    if(j %% 100 == 0){
      print(j)
    }
  }
  AverageTimeb[i]=mean(sim)
  #hist(sim, main=c("Time of Diffusion for Graph",i))
  AverageNumInfb[i]=mean(numinf1)
  #hist(numinf1, main = c("Number of People Infected for Graph",i))
}

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
AverageTime68b=rep(0,7)
AverageNumInf68b=rep(0,7)
par(mfrow=c(2,2))
for(i in 1:5){
  G=netlistb[[i]]
  # Run the simulation
  sim=rep(0,n)
  Vinitial=rep(0,n)
  numinf1=rep(0,n)
  print(paste(i, "********************************************************"))
  for(j in 1:n){
    Vinitial[j] = sample(1:8,1)
    sim[j]=length(diffusion2(G,Vinitial[j],p))
    numinf1[j]=numinf
    if(j %% 100 == 0){
      print(j)
    }
  }
  AverageTime68b[i]=mean(sim)
  #hist(sim, main=c("Time of Diffusion for Graph",i))
  AverageNumInf68b[i]=mean(numinf1)
  #hist(numinf1, main = c("Number of People Infected for Graph",i))
}
for(i in 6:7){
  G=netlistb[[i]]
  # Run the simulation
  sim=rep(0,n)
  Vinitial=rep(0,n)
  numinf1=rep(0,n)
  print(paste(i, "********************************************************"))
  for(j in 1:n){
    Vinitial[j] = sample(1:6,1)
    sim[j]=length(diffusion2(G,Vinitial[j],p))
    numinf1[j]=numinf
    if(j %% 100 == 0){
      print(j)
    }
  }
  AverageTime68b[i]=mean(sim)
  #hist(sim, main=c("Time of Diffusion for Graph",i))
  AverageNumInf68b[i]=mean(numinf1)
  #hist(numinf1, main = c("Number of People Infected for Graph",i))
}


datb=cbind(c(5,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2),
          c(4,4,3,2,4,3,2,2,2,2,1,1,1,0,0,0,0,0,0),
          c(1,2,2,3,2,3,3,3,3,2,3,3,3,3,3,3,3,3,3))
datb=cbind(datb,AverageTimeb,AverageNumInfb)


dat2b=cbind(c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,8,8,8,8,8,6,6),
           c(5,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,3,3,3,2,2,2,2),
           c(4,4,3,2,4,3,2,2,2,2,1,1,1,0,0,0,0,0,0,4,2,0,1,0,2,0),
           c(1,2,2,3,2,3,3,3,3,2,3,3,3,3,3,3,3,3,3,3,3,2,3,3,3,3),
           cc)

AvgTb=rep(0,26)
AvgTb[0:19]=AverageTimeb
AvgTb[20:26]=AverageTime68b
AvgNumb=rep(0,26)
AvgNumb[1:19]=AverageNumInfb
AvgNumb[20:26]=AverageNumInf68b

Totdatb=cbind(dat2b,AvgTb,AvgNumb)
#Totdatb=Totdatb[1:19,]
#Totdatb=Totdatb[20:26,]
colnames(Totdatb)=c("Nodes","Diameter","Triangles","CutSet","AvgT","AvgNum")
Totdatb=as.data.frame(Totdatb)
#attach(Totdatb)

# 
# plot_ly(z = ~AvgT, x=~Nodes,y=~Triangles, type = "contour")
# plot_ly(z = ~AvgT, x=~Diameter,y=~Triangles, type = "contour")
# plot_ly(z= ~AvgNum, x=~Diameter, y=~Triangles, type="contour")
# 
# 
# plot(x=Triangles, y=AvgT,col=Nodes+2,main="SI Model")
# #plot(x=AvgT,y=AvgNum,col=Nodes+2,main="SI Model")
# plot(x=Diameter,y=AvgT,col=Nodes+2,main="SI Model")
# plot(x=Nodes,y=AvgT,main="SI Model")

save(Totdatb, file = "C:/Users/ckell/OneDrive/Penn State/Research/RCode/7_31_Diffdata/Diff2data.Rdata")
