library(reshape2)
#Test to read data
load("C:/Users/ckell/OneDrive/Penn State/Research/OverLeafCode_and_Images/twelve.Rdata")

#creating a blank list for the data
graphs <- list()
for(i in 1:85){
  nam <- paste("Gr", i, sep="")
  assign(nam, read.table(text = twelve_node, nrows=12, skip = grep(paste("Graph", i, sep = " "), readLines(textConnection(twelve_node)))))
  
  #manipulating the data from an adjacency list to an edge list
  place_holder <- get((paste("Gr", i, sep="")))[,-2]
  place_holder <- melt(place_holder, id=c("V1")) 
  place_holder <- place_holder[,-2]
  place_holder <- t(apply(place_holder, 1, sort))
  place_holder <- place_holder[!duplicated(place_holder),]
  
  #creation of graphs and storing in a list for simulation
  graphs[[i]] <- graph.data.frame(place_holder, directed=FALSE)
}

#saving to use in the simulation
save(graphs, file = "C:/Users/ckell/OneDrive/Penn State/Research/OverLeafCode_and_Images/twelve_graphs.Rdata")
