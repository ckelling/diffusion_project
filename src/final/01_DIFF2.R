###
# Diffusion Function 2
###
diffusion2 <- function(G, Vinitial, p){
  #precomupte all outgoing graph adjacencies
  G$AdjList = get.adjlist(G, mode="out")
  q=50
  #initialize various graph attributes
  V(G)$color = "blue"
  E(G)$color = "black"
  V(G)[Vinitial]$color <- "yellow"
  
  #list to store the incrememntal graphs (for plotting later)
  Glist <- list(G)
  count <- 1
  numinf<<-1
  
  #spread the infection
  active <- Vinitial
  while(length(active)>0){
    new_infected <- NULL
    E(G)$color = "black"
    for(v in active){
      #spread through the daily contacts of vertex v
      daily_contacts <- G$AdjList[[v]]
      E(G)[v %->% daily_contacts]$color <- "red"
      for( v1 in daily_contacts){
        new_color <- sample(c("red","blue"),1
                            ,prob=c(p,1-p))
        if(V(G)[v1]$color == "blue" & new_color=="red"){
          V(G)[v1]$color <- "red"
          new_infected <- c(new_infected, v1)
          numinf<<-numinf+1
        }
      }
    }
    
    #the next active set
    active <- c(new_infected,active)
    
    #add graph to list
    count <- count + 1
    Glist[[count]] <- G
    if(length(active)==vcount(G)){
      break
    }
  }
  return(Glist)
}
