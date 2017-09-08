#######
### This file is meant to merge all of the data that was collected through
### multiple simulations and make it into a form that can be plotted. 
#######

#loading 12 node data
load("C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/original/twelve_node_data/twelve_diff1.Rdata")
load("C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/original/twelve_node_data/twelve_diff2a.Rdata")
load("C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/original/twelve_node_data/twelve_diff2b.Rdata")
load("C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/original/twelve_node_data/twelve_diff2c.Rdata")
load("C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/original/twelve_node_data/twelve_diff2d.Rdata")

#loading 6-10 node data
load("C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/final/6810_Diff1data.Rdata")
load("C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/final/6810_Diff2data.Rdata")

#merging all of the different iterations that we simulated through
dat_12_diff2 <- rbind(dat_12_diff2a[1:24,], dat_12_diff2b[25:39,], dat_12_diff2c[40:74,], dat_12_diff2d[75:85,])

#making it so that we can merge for diff1
diff1_6810 <- Totdat[,c(1,2,5:7)]
colnames(diff1_6810)[5] <- "CC"

#making it so that we can merge for diff 2
diff2_6810 <- Totdatb[,c(1,2,5:7)]
colnames(diff2_6810)[5] <- "CC"

#combining for all nodes
diff1_dat <- rbind(diff1_6810, dat_12_diff1)
diff2_dat <- rbind(diff2_6810, dat_12_diff2)

save(diff1_dat, file = "C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/final/full_diff1_dat.Rdata")
save(diff2_dat, file = "C:/Users/ckell/OneDrive/Penn State/Research/ashton_diff_project_git/data/final/full_diff2_dat.Rdata")