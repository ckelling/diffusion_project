library(igraph)
load("C:/Users/ckell/OneDrive/Penn State/Research/OverLeafCode_and_Images/twelve_graphs.Rdata")
source("C:/Users/ckell/OneDrive/Penn State/Research/OverLeafCode_and_Images/Code/01_DIFF1.R")

#calculate clustering coefficient
cc<- rep(NA, 85)
for(i in 1:85){
  cc[i] <- transitivity(graphs[[i]])
}

#calculate diameter
diam <- rep(NA, 85)
for(i in 1:85){
  diam[i] <- diameter(graphs[[i]])
}

nodes <- rep(12, 85)

n=10000 #number of simulations
p=0.5 #probability of infection
AverageTime=rep(0,85)
AverageNumInf=rep(0,85)
for(i in 1:85){
  G=graphs[[i]]
  # Run the simulation
  sim=rep(0,n)
  Vinitial=rep(0,n)
  numinf1=rep(0,n)
  print(paste(i, "********************************************************"))
  for(j in 1:n){
    Vinitial[j] = sample(1:12,1)
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

dat_12_diff1 <- cbind(nodes, diam, cc, AverageTime, AverageNumInf)
colnames(dat_12_diff1)=c("Nodes","Diameter","CC","AvgT","AvgNum")
save(dat_12_diff1, file= "C:/Users/ckell/OneDrive/Penn State/Research/OverLeafCode_and_Images/Code/twelve_diff1.Rdata")
