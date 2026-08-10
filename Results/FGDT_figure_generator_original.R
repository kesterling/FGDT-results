


# This code reproduces the figures in the paper:
#     Using LLM Digital Twin Simulation to Evaluate the Emergent Properties of Human Group Interaction: 
#     With an Application to Focus Group Design






library(foreign)
library(ggridges)
#library(ggpubr)
#library(viridis)
#library(tidyverse)

# set.seed(5591)


###### VERY IMPORTANT: 

# To run this file, either use your GUI or uncomment/edit this next line to change your working directory that has the "Results" folder:
# setwd("C:/Users/kevine/Dropbox/TeCD Lab/Prytaneum/Focus Group Methods/read/LLM simulated responses/Experiments/Scripts/Kevin's Copy/Results")

# The correct path will have "/Results" at the end. If you do not do this, you will simply encounter an error message.
# Once you have set the correct path, then you can highlight the full text of this file and press the "Run" button.
# That will generate the figures in the order they appear in the manuscript.

# Email Kevin Esterling (kevin.esterling@ucr.edu) if you have any questions about running this code.

######################


project_dir<- getwd()


## Nuclear Power, four options


# N=600: 8323757419
# N=60: 3708584978
# N=6: 5045433629

dirs<-c("trial_id_8323757419","trial_id_3708584978","trial_id_5045433629")


# use the following code if four standpoint options (see below if you have 3 standpoint options)

# trial id 8323757419 is 600 participant, 4 option on nuclear power
setwd(paste0(project_dir,"/",dirs[1]))
l1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in large N response 1 matrix
l2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in large N response 2 matrix
l3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in large N response 3 matrix
l4<-as.matrix(read.csv("similarityMatrix_standpoint3.csv", header = FALSE)) # read in large N response 4 matrix
# trial id 3708584978 is 60 participant
setwd(paste0(project_dir,"/",dirs[2]))
m1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in medium N response 1 matrix
m2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in medium N response 2 matrix
m3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in medium N response 3 matrix
m4<-as.matrix(read.csv("similarityMatrix_standpoint3.csv", header = FALSE)) # read in medium N response 4 matrix
# trial id 5045433629 is 6 participant
setwd(paste0(project_dir,"/",dirs[3]))
s1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in small N response 1 matrix
s2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in small N response 2 matrix
s3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in small N response 3 matrix
s4<-as.matrix(read.csv("similarityMatrix_standpoint3.csv", header = FALSE)) # read in small N response 4 matrix

l1vec<-l1[lower.tri(l1)]
l2vec<-l2[lower.tri(l2)]
l3vec<-l3[lower.tri(l3)]
l4vec<-l4[lower.tri(l4)]
m1vec<-m1[lower.tri(m1)]
m2vec<-m2[lower.tri(m2)]
m3vec<-m3[lower.tri(m3)]
m4vec<-m4[lower.tri(m4)]
s1vec<-s1[lower.tri(s1)]
s2vec<-s2[lower.tri(s2)]
s3vec<-s3[lower.tri(s3)]
s4vec<-s4[lower.tri(s4)]

l1vec_valid<-l1vec
l2vec_valid<-l2vec
l3vec_valid<-l3vec
l4vec_valid<-l4vec
m1vec_valid<-m1vec
m2vec_valid<-m2vec
m3vec_valid<-m3vec
m4vec_valid<-m4vec
s1vec_valid<-s1vec
s2vec_valid<-s2vec
s3vec_valid<-s3vec
s4vec_valid<-s4vec

l1vec_valid[l1vec_valid<0.1]<-0
l2vec_valid[l2vec_valid<0.1]<-0
l3vec_valid[l3vec_valid<0.1]<-0
l4vec_valid[l4vec_valid<0.1]<-0
m1vec_valid[m1vec_valid<0.1]<-0
m2vec_valid[m2vec_valid<0.1]<-0
m3vec_valid[m3vec_valid<0.1]<-0
m4vec_valid[m4vec_valid<0.1]<-0
s1vec_valid[s1vec_valid<0.1]<-0
s2vec_valid[s2vec_valid<0.1]<-0
s3vec_valid[s3vec_valid<0.1]<-0
s4vec_valid[s4vec_valid<0.1]<-0

l1vec_valid[l1vec_valid==1]<-0
l2vec_valid[l2vec_valid==1]<-0
l3vec_valid[l3vec_valid==1]<-0
l4vec_valid[l4vec_valid==1]<-0
m1vec_valid[m1vec_valid==1]<-0
m2vec_valid[m2vec_valid==1]<-0
m3vec_valid[m3vec_valid==1]<-0
m4vec_valid[m4vec_valid==1]<-0
s1vec_valid[s1vec_valid==1]<-0
s2vec_valid[s2vec_valid==1]<-0
s3vec_valid[s3vec_valid==1]<-0
s4vec_valid[s4vec_valid==1]<-0

setwd(project_dir)
sink("results_original.txt")
print("Nuclear power 4, Random")
print("Empty")
# These statements return the rate of empty text cells
# this is the formula: k=(1+sqrt(1+8n))/2 where n is the number of 1s and k>1
if (min(s1vec_valid)>0) {0 
} else if (min(s1vec_valid)==0 & length(s1vec[s1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s1vec[s1vec==1])))/2)/length(diag(s1))}
if (min(s2vec_valid)>0) {0 
} else if (min(s2vec_valid)==0 & length(s2vec[s2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s2vec[s2vec==1])))/2)/length(diag(s2))}
if (min(s3vec_valid)>0) {0 
} else if (min(s3vec_valid)==0 & length(s3vec[s3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s3vec[s3vec==1])))/2)/length(diag(s3))}
if (min(s4vec_valid)>0) {0 
} else if (min(s4vec_valid)==0 & length(s4vec[s4vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s4vec[s4vec==1])))/2)/length(diag(s4))}
if (min(m1vec_valid)>0) {0 
} else if (min(m1vec_valid)==0 & length(m1vec[m1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m1vec[m1vec==1])))/2)/length(diag(m1))}
if (min(m2vec_valid)>0) {0 
} else if (min(m2vec_valid)==0 & length(m2vec[m2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m2vec[m2vec==1])))/2)/length(diag(m2))}
if (min(m3vec_valid)>0) {0 
} else if (min(m3vec_valid)==0 & length(m3vec[m3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m3vec[m3vec==1])))/2)/length(diag(m3))}
if (min(m4vec_valid)>0) {0 
} else if (min(m4vec_valid)==0 & length(m4vec[m4vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m4vec[m4vec==1])))/2)/length(diag(m4))}
if (min(l1vec_valid)>0) {0 
} else if (min(l1vec_valid)==0 & length(l1vec[l1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l1vec[l1vec==1])))/2)/length(diag(l1))}
if (min(l2vec_valid)>0) {0 
} else if (min(l2vec_valid)==0 & length(l2vec[l2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l2vec[l2vec==1])))/2)/length(diag(l2))}
if (min(l3vec_valid)>0) {0 
} else if (min(l3vec_valid)==0 & length(l3vec[l3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l3vec[l3vec==1])))/2)/length(diag(l3))}
if (min(l4vec_valid)>0) {0 
} else if (min(l4vec_valid)==0 & length(l4vec[l4vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l4vec[l4vec==1])))/2)/length(diag(l4))}

# These statements give the mean, SD and N for the non-zero cells:
print("Mean")
mean(s1vec_valid[s1vec_valid>0])
mean(s2vec_valid[s2vec_valid>0])
mean(s3vec_valid[s3vec_valid>0])
mean(s4vec_valid[s4vec_valid>0])
mean(m1vec_valid[m1vec_valid>0])
mean(m2vec_valid[m2vec_valid>0])
mean(m3vec_valid[m3vec_valid>0])
mean(m4vec_valid[m4vec_valid>0])
mean(l1vec_valid[l1vec_valid>0])
mean(l2vec_valid[l2vec_valid>0])
mean(l3vec_valid[l3vec_valid>0])
mean(l4vec_valid[l4vec_valid>0])

print("SD")
sd(s1vec_valid[s1vec_valid>0])
sd(s2vec_valid[s2vec_valid>0])
sd(s3vec_valid[s3vec_valid>0])
sd(s4vec_valid[s4vec_valid>0])
sd(m1vec_valid[m1vec_valid>0])
sd(m2vec_valid[m2vec_valid>0])
sd(m3vec_valid[m3vec_valid>0])
sd(m4vec_valid[m4vec_valid>0])
sd(l1vec_valid[l1vec_valid>0])
sd(l2vec_valid[l2vec_valid>0])
sd(l3vec_valid[l3vec_valid>0])
sd(l4vec_valid[l4vec_valid>0])

print("N")
length(s1vec_valid[s1vec_valid>0])
length(s2vec_valid[s2vec_valid>0])
length(s3vec_valid[s3vec_valid>0])
length(s4vec_valid[s4vec_valid>0])
length(m1vec_valid[m1vec_valid>0])
length(m2vec_valid[m2vec_valid>0])
length(m3vec_valid[m3vec_valid>0])
length(m4vec_valid[m4vec_valid>0])
length(l1vec_valid[l1vec_valid>0])
length(l2vec_valid[l2vec_valid>0])
length(l3vec_valid[l3vec_valid>0])
length(l4vec_valid[l4vec_valid>0])
sink()



## figures

source("similarity_nuclear4.R")
source("mds_nuclear4.R")



##########################################################################################

## Nuclear Power, three options (random)


# N=600: 2307512764 
# N=60: 8971956292
# N=6: 5231569904

dirs<-c("trial_id_2307512764","trial_id_8971956292","trial_id_5231569904")


# ******** If only three standpoint options, use this:

# trial id 2307512764
setwd(paste0(project_dir,"/",dirs[1]))
l1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in large N response 1 matrix
l2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in large N response 2 matrix
l3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in large N response 3 matrix
# trial id 8971956292
setwd(paste0(project_dir,"/",dirs[2]))
m1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in medium N response 1 matrix
m2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in medium N response 2 matrix
m3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in medium N response 3 matrix
# trial id 5231569904
setwd(paste0(project_dir,"/",dirs[3]))
s1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in small N response 1 matrix
s2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in small N response 2 matrix
s3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in small N response 3 matrix

l1vec<-l1[lower.tri(l1)]
l2vec<-l2[lower.tri(l2)]
l3vec<-l3[lower.tri(l3)]
m1vec<-m1[lower.tri(m1)]
m2vec<-m2[lower.tri(m2)]
m3vec<-m3[lower.tri(m3)]
s1vec<-s1[lower.tri(s1)]
s2vec<-s2[lower.tri(s2)]
s3vec<-s3[lower.tri(s3)]

l1vec_valid<-l1vec
l2vec_valid<-l2vec
l3vec_valid<-l3vec
m1vec_valid<-m1vec
m2vec_valid<-m2vec
m3vec_valid<-m3vec
s1vec_valid<-s1vec
s2vec_valid<-s2vec
s3vec_valid<-s3vec

l1vec_valid[l1vec_valid<0.1]<-0
l2vec_valid[l2vec_valid<0.1]<-0
l3vec_valid[l3vec_valid<0.1]<-0
m1vec_valid[m1vec_valid<0.1]<-0
m2vec_valid[m2vec_valid<0.1]<-0
m3vec_valid[m3vec_valid<0.1]<-0
s1vec_valid[s1vec_valid<0.1]<-0
s2vec_valid[s2vec_valid<0.1]<-0
s3vec_valid[s3vec_valid<0.1]<-0

l1vec_valid[l1vec_valid==1]<-0
l2vec_valid[l2vec_valid==1]<-0
l3vec_valid[l3vec_valid==1]<-0
m1vec_valid[m1vec_valid==1]<-0
m2vec_valid[m2vec_valid==1]<-0
m3vec_valid[m3vec_valid==1]<-0
s1vec_valid[s1vec_valid==1]<-0
s2vec_valid[s2vec_valid==1]<-0
s3vec_valid[s3vec_valid==1]<-0

setwd(project_dir)
sink("results_original.txt", append=TRUE)
print("Nuclear Power 3, Random")
# These statements return the rate of empty text cells
print("Empty")
if (min(s1vec_valid)>0) {0 
} else if (min(s1vec_valid)==0 & length(s1vec[s1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s1vec[s1vec==1])))/2)/length(diag(s1))}
if (min(s2vec_valid)>0) {0 
} else if (min(s2vec_valid)==0 & length(s2vec[s2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s2vec[s2vec==1])))/2)/length(diag(s2))}
if (min(s3vec_valid)>0) {0 
} else if (min(s3vec_valid)==0 & length(s3vec[s3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s3vec[s3vec==1])))/2)/length(diag(s3))}
if (min(m1vec_valid)>0) {0 
} else if (min(m1vec_valid)==0 & length(m1vec[m1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m1vec[m1vec==1])))/2)/length(diag(m1))}
if (min(m2vec_valid)>0) {0 
} else if (min(m2vec_valid)==0 & length(m2vec[m2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m2vec[m2vec==1])))/2)/length(diag(m2))}
if (min(m3vec_valid)>0) {0 
} else if (min(m3vec_valid)==0 & length(m3vec[m3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m3vec[m3vec==1])))/2)/length(diag(m3))}
if (min(l1vec_valid)>0) {0 
} else if (min(l1vec_valid)==0 & length(l1vec[l1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l1vec[l1vec==1])))/2)/length(diag(l1))}
if (min(l2vec_valid)>0) {0 
} else if (min(l2vec_valid)==0 & length(l2vec[l2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l2vec[l2vec==1])))/2)/length(diag(l2))}
if (min(l3vec_valid)>0) {0 
} else if (min(l3vec_valid)==0 & length(l3vec[l3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l3vec[l3vec==1])))/2)/length(diag(l3))}

# These statements give the mean, SD and N for the non-zero cells:
print("Mean")
mean(s1vec_valid[s1vec_valid>0])
mean(s2vec_valid[s2vec_valid>0])
mean(s3vec_valid[s3vec_valid>0])
mean(m1vec_valid[m1vec_valid>0])
mean(m2vec_valid[m2vec_valid>0])
mean(m3vec_valid[m3vec_valid>0])
mean(l1vec_valid[l1vec_valid>0])
mean(l2vec_valid[l2vec_valid>0])
mean(l3vec_valid[l3vec_valid>0])

print("SD")
sd(s1vec_valid[s1vec_valid>0])
sd(s2vec_valid[s2vec_valid>0])
sd(s3vec_valid[s3vec_valid>0])
sd(m1vec_valid[m1vec_valid>0])
sd(m2vec_valid[m2vec_valid>0])
sd(m3vec_valid[m3vec_valid>0])
sd(l1vec_valid[l1vec_valid>0])
sd(l2vec_valid[l2vec_valid>0])
sd(l3vec_valid[l3vec_valid>0])

print("N")
length(s1vec_valid[s1vec_valid>0])
length(s2vec_valid[s2vec_valid>0])
length(s3vec_valid[s3vec_valid>0])
length(m1vec_valid[m1vec_valid>0])
length(m2vec_valid[m2vec_valid>0])
length(m3vec_valid[m3vec_valid>0])
length(l1vec_valid[l1vec_valid>0])
length(l2vec_valid[l2vec_valid>0])
length(l3vec_valid[l3vec_valid>0])
sink()




## figures

source("similarity_nuclear3.R")
source("mds_nuclear3.R")


##########################################################################################

## Nuclear Power, three options, INDIVIDUAL design


# N=600: 4384390194 <-- these trials are ONLY in the replication!!
# N=60: 368357090 <-- these trials are ONLY in the replication!!
# N=6: 5425534150 <-- these trials are ONLY in the replication!!

dirs<-c("trial_id_4384390194","trial_id_368357090","trial_id_5425534150")


# ******** If only three standpoint options, use this:

# trial id none
setwd(paste0(project_dir,"/",dirs[1]))
l1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in large N response 1 matrix
l2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in large N response 2 matrix
l3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in large N response 3 matrix
# trial id none
setwd(paste0(project_dir,"/",dirs[2]))
m1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in medium N response 1 matrix
m2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in medium N response 2 matrix
m3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in medium N response 3 matrix
# trial id none
setwd(paste0(project_dir,"/",dirs[3]))
s1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in small N response 1 matrix
s2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in small N response 2 matrix
s3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in small N response 3 matrix

l1vec<-l1[lower.tri(l1)]
l2vec<-l2[lower.tri(l2)]
l3vec<-l3[lower.tri(l3)]
m1vec<-m1[lower.tri(m1)]
m2vec<-m2[lower.tri(m2)]
m3vec<-m3[lower.tri(m3)]
s1vec<-s1[lower.tri(s1)]
s2vec<-s2[lower.tri(s2)]
s3vec<-s3[lower.tri(s3)]

l1vec_valid<-l1vec
l2vec_valid<-l2vec
l3vec_valid<-l3vec
m1vec_valid<-m1vec
m2vec_valid<-m2vec
m3vec_valid<-m3vec
s1vec_valid<-s1vec
s2vec_valid<-s2vec
s3vec_valid<-s3vec

l1vec_valid[l1vec_valid<0.1]<-0
l2vec_valid[l2vec_valid<0.1]<-0
l3vec_valid[l3vec_valid<0.1]<-0
m1vec_valid[m1vec_valid<0.1]<-0
m2vec_valid[m2vec_valid<0.1]<-0
m3vec_valid[m3vec_valid<0.1]<-0
s1vec_valid[s1vec_valid<0.1]<-0
s2vec_valid[s2vec_valid<0.1]<-0
s3vec_valid[s3vec_valid<0.1]<-0

l1vec_valid[l1vec_valid==1]<-0
l2vec_valid[l2vec_valid==1]<-0
l3vec_valid[l3vec_valid==1]<-0
m1vec_valid[m1vec_valid==1]<-0
m2vec_valid[m2vec_valid==1]<-0
m3vec_valid[m3vec_valid==1]<-0
s1vec_valid[s1vec_valid==1]<-0
s2vec_valid[s2vec_valid==1]<-0
s3vec_valid[s3vec_valid==1]<-0

setwd(project_dir)
sink("results_original.txt", append=TRUE)
print("Nuclear Power 3, Individual")
# These statements return the rate of empty text cells
print("Empty")
if (min(s1vec_valid)>0) {0 
} else if (min(s1vec_valid)==0 & length(s1vec[s1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s1vec[s1vec==1])))/2)/length(diag(s1))}
if (min(s2vec_valid)>0) {0 
} else if (min(s2vec_valid)==0 & length(s2vec[s2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s2vec[s2vec==1])))/2)/length(diag(s2))}
if (min(s3vec_valid)>0) {0 
} else if (min(s3vec_valid)==0 & length(s3vec[s3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s3vec[s3vec==1])))/2)/length(diag(s3))}
if (min(m1vec_valid)>0) {0 
} else if (min(m1vec_valid)==0 & length(m1vec[m1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m1vec[m1vec==1])))/2)/length(diag(m1))}
if (min(m2vec_valid)>0) {0 
} else if (min(m2vec_valid)==0 & length(m2vec[m2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m2vec[m2vec==1])))/2)/length(diag(m2))}
if (min(m3vec_valid)>0) {0 
} else if (min(m3vec_valid)==0 & length(m3vec[m3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m3vec[m3vec==1])))/2)/length(diag(m3))}
if (min(l1vec_valid)>0) {0 
} else if (min(l1vec_valid)==0 & length(l1vec[l1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l1vec[l1vec==1])))/2)/length(diag(l1))}
if (min(l2vec_valid)>0) {0 
} else if (min(l2vec_valid)==0 & length(l2vec[l2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l2vec[l2vec==1])))/2)/length(diag(l2))}
if (min(l3vec_valid)>0) {0 
} else if (min(l3vec_valid)==0 & length(l3vec[l3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l3vec[l3vec==1])))/2)/length(diag(l3))}

# These statements give the mean, SD and N for the non-zero cells:
print("Mean")
mean(s1vec_valid[s1vec_valid>0])
mean(s2vec_valid[s2vec_valid>0])
mean(s3vec_valid[s3vec_valid>0])
mean(m1vec_valid[m1vec_valid>0])
mean(m2vec_valid[m2vec_valid>0])
mean(m3vec_valid[m3vec_valid>0])
mean(l1vec_valid[l1vec_valid>0])
mean(l2vec_valid[l2vec_valid>0])
mean(l3vec_valid[l3vec_valid>0])

print("SD")
sd(s1vec_valid[s1vec_valid>0])
sd(s2vec_valid[s2vec_valid>0])
sd(s3vec_valid[s3vec_valid>0])
sd(m1vec_valid[m1vec_valid>0])
sd(m2vec_valid[m2vec_valid>0])
sd(m3vec_valid[m3vec_valid>0])
sd(l1vec_valid[l1vec_valid>0])
sd(l2vec_valid[l2vec_valid>0])
sd(l3vec_valid[l3vec_valid>0])

print("N")
length(s1vec_valid[s1vec_valid>0])
length(s2vec_valid[s2vec_valid>0])
length(s3vec_valid[s3vec_valid>0])
length(m1vec_valid[m1vec_valid>0])
length(m2vec_valid[m2vec_valid>0])
length(m3vec_valid[m3vec_valid>0])
length(l1vec_valid[l1vec_valid>0])
length(l2vec_valid[l2vec_valid>0])
length(l3vec_valid[l3vec_valid>0])
sink()



## figure

source("similarity_nuclear3_ind.R")
source("mds_nuclear3_ind.R")



##########################################################################################


## Gun Control, including stratification design

# 60 simple randomization: 3634605881
# 6 stratified design: 5447221142
# 6 simple randomization: 7087039549

dirs<-c("trial_id_3634605881","trial_id_5447221142","trial_id_7087039549")


# ******** If only three standpoint options, use this:

# trial id 3634605881
setwd(paste0(project_dir,"/",dirs[1]))
l1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in large N response 1 matrix
l2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in large N response 2 matrix
l3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in large N response 3 matrix
# trial id 5447221142
setwd(paste0(project_dir,"/",dirs[2]))
m1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in medium N response 1 matrix
m2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in medium N response 2 matrix
m3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in medium N response 3 matrix
# trial id 7087039549
setwd(paste0(project_dir,"/",dirs[3]))
s1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in small N response 1 matrix
s2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in small N response 2 matrix
s3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in small N response 3 matrix

l1vec<-l1[lower.tri(l1)]
l2vec<-l2[lower.tri(l2)]
l3vec<-l3[lower.tri(l3)]
m1vec<-m1[lower.tri(m1)]
m2vec<-m2[lower.tri(m2)]
m3vec<-m3[lower.tri(m3)]
s1vec<-s1[lower.tri(s1)]
s2vec<-s2[lower.tri(s2)]
s3vec<-s3[lower.tri(s3)]

l1vec_valid<-l1vec
l2vec_valid<-l2vec
l3vec_valid<-l3vec
m1vec_valid<-m1vec
m2vec_valid<-m2vec
m3vec_valid<-m3vec
s1vec_valid<-s1vec
s2vec_valid<-s2vec
s3vec_valid<-s3vec

l1vec_valid[l1vec_valid<0.1]<-0
l2vec_valid[l2vec_valid<0.1]<-0
l3vec_valid[l3vec_valid<0.1]<-0
m1vec_valid[m1vec_valid<0.1]<-0
m2vec_valid[m2vec_valid<0.1]<-0
m3vec_valid[m3vec_valid<0.1]<-0
s1vec_valid[s1vec_valid<0.1]<-0
s2vec_valid[s2vec_valid<0.1]<-0
s3vec_valid[s3vec_valid<0.1]<-0

l1vec_valid[l1vec_valid==1]<-0
l2vec_valid[l2vec_valid==1]<-0
l3vec_valid[l3vec_valid==1]<-0
m1vec_valid[m1vec_valid==1]<-0
m2vec_valid[m2vec_valid==1]<-0
m3vec_valid[m3vec_valid==1]<-0
s1vec_valid[s1vec_valid==1]<-0
s2vec_valid[s2vec_valid==1]<-0
s3vec_valid[s3vec_valid==1]<-0

setwd(project_dir)
sink("results_original.txt", append=TRUE)
print("Gun Control, Stratified")
# These statements return the rate of empty text cells
print("Empty")
if (min(s1vec_valid)>0) {0 
} else if (min(s1vec_valid)==0 & length(s1vec[s1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s1vec[s1vec==1])))/2)/length(diag(s1))}
if (min(s2vec_valid)>0) {0 
} else if (min(s2vec_valid)==0 & length(s2vec[s2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s2vec[s2vec==1])))/2)/length(diag(s2))}
if (min(s3vec_valid)>0) {0 
} else if (min(s3vec_valid)==0 & length(s3vec[s3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(s3vec[s3vec==1])))/2)/length(diag(s3))}
if (min(m1vec_valid)>0) {0 
} else if (min(m1vec_valid)==0 & length(m1vec[m1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m1vec[m1vec==1])))/2)/length(diag(m1))}
if (min(m2vec_valid)>0) {0 
} else if (min(m2vec_valid)==0 & length(m2vec[m2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m2vec[m2vec==1])))/2)/length(diag(m2))}
if (min(m3vec_valid)>0) {0 
} else if (min(m3vec_valid)==0 & length(m3vec[m3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(m3vec[m3vec==1])))/2)/length(diag(m3))}
if (min(l1vec_valid)>0) {0 
} else if (min(l1vec_valid)==0 & length(l1vec[l1vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l1vec[l1vec==1])))/2)/length(diag(l1))}
if (min(l2vec_valid)>0) {0 
} else if (min(l2vec_valid)==0 & length(l2vec[l2vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l2vec[l2vec==1])))/2)/length(diag(l2))}
if (min(l3vec_valid)>0) {0 
} else if (min(l3vec_valid)==0 & length(l3vec[l3vec==1])==0) {1/length(diag(s1))
} else {((1 + sqrt(1 + 8*length(l3vec[l3vec==1])))/2)/length(diag(l3))}

# These statements give the mean, SD and N for the non-zero cells:
print("Mean")
mean(s1vec_valid[s1vec_valid>0])
mean(s2vec_valid[s2vec_valid>0])
mean(s3vec_valid[s3vec_valid>0])
mean(m1vec_valid[m1vec_valid>0])
mean(m2vec_valid[m2vec_valid>0])
mean(m3vec_valid[m3vec_valid>0])
mean(l1vec_valid[l1vec_valid>0])
mean(l2vec_valid[l2vec_valid>0])
mean(l3vec_valid[l3vec_valid>0])

print("SD")
sd(s1vec_valid[s1vec_valid>0])
sd(s2vec_valid[s2vec_valid>0])
sd(s3vec_valid[s3vec_valid>0])
sd(m1vec_valid[m1vec_valid>0])
sd(m2vec_valid[m2vec_valid>0])
sd(m3vec_valid[m3vec_valid>0])
sd(l1vec_valid[l1vec_valid>0])
sd(l2vec_valid[l2vec_valid>0])
sd(l3vec_valid[l3vec_valid>0])

print("N")
length(s1vec_valid[s1vec_valid>0])
length(s2vec_valid[s2vec_valid>0])
length(s3vec_valid[s3vec_valid>0])
length(m1vec_valid[m1vec_valid>0])
length(m2vec_valid[m2vec_valid>0])
length(m3vec_valid[m3vec_valid>0])
length(l1vec_valid[l1vec_valid>0])
length(l2vec_valid[l2vec_valid>0])
length(l3vec_valid[l3vec_valid>0])
sink()


## figure

source("similarity_guncontrol_strat.R")
source("mds_guncontrol_strat.R")


##########################################################################################

## Figure 4: Bias Audit, human data House of Representatives

# Before running this: once you run summarizationSimilarities_scmc.py, open the 
# summarizationSimilarities_SCMC.csv file to manually create the Similaritymax3
# (the highest similarity score among the three-item summary version) and the Similaritymax5 
# (the highest similarity score among the five-item summary version) columns. To do that, you 
# use the =max() function and use columns J,K,L for the max3 and columns L,M,N,O,P for the max5.
# Also, manually create a column favor that =0 if In Support == False 
# and =1 if In Support == TRUE.



setwd(project_dir)

data <- read.csv("summarizationSimilarities_SCMC.csv")

data$Education[data$Education=="Post-Grad"]<-"4-Year"

data <-
  data %>%
  mutate(white = ifelse(Ethnicity == "White", yes = 1, no = 0),
        male = ifelse(Gender == "Man", yes = 1, no = 0),
        college = ifelse(Education == ("4-Year"), yes = 1, no = 0),
        repub = ifelse(Party == "R", yes = 1, no = 0),
        favor = ifelse(In.Support == "TRUE", yes = 1, no = 0)
           )

## Main figure

figdata1a<-cbind(data$Similaritymax5[data$white==0],data$favor[data$white==0])
figdata1b<-cbind(data$Similaritymax5[data$white==1],data$favor[data$white==1])
figdata2a<-cbind(data$Similaritymax5[data$male==0],data$favor[data$male==0])
figdata2b<-cbind(data$Similaritymax5[data$male==1],data$favor[data$male==1])
figdata3a<-cbind(data$Similaritymax5[data$college==0],data$favor[data$college==0])
figdata3b<-cbind(data$Similaritymax5[data$college==1],data$favor[data$college==1])

figdata1a<-cbind(figdata1a, rep(1, length(figdata1a)))
figdata1b<-cbind(figdata1b, rep(2, length(figdata1b)))
figdata2a<-cbind(figdata2a, rep(3, length(figdata2a)))
figdata2b<-cbind(figdata2b, rep(4, length(figdata2b)))
figdata3a<-cbind(figdata3a, rep(5, length(figdata3a)))
figdata3b<-cbind(figdata3b, rep(6, length(figdata3b)))

figdata<-rbind(figdata1a, figdata1b, figdata2a, figdata2b, figdata3a, figdata3b)

figdata<-as.data.frame(figdata)

figdata[,2]<- factor(
  figdata[,2],
  levels = c("0", "1"),
  labels = c("Oppose", "Support")
)

figdata[,3] <- factor(
  figdata[,3],
  levels = c("1", "2", "3", "4", "5", "6"),
  labels = c("NotWhite", "White", "NotMale", "Male", "NotCollege", "College")
)


Position<-figdata[,2]

theme_set(theme_ridges())

p<-ggplot(figdata, aes(x=figdata[,1], y=figdata[,3], fill=Position)) +
  geom_density_ridges(alpha=0.5, show.legend = TRUE) +
  scale_fill_brewer(palette = 5) +
  labs(
    title = 'Bias Audit - Expand U.S. House (Human Data)',
    #subtitle = 'Cohen\'s D',
    x = "Cosine Similarity Distribution"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

print(p)

# ---- Save the figure -----------------------------------------------------------
ggsave("BiasAuditHumanSCMC.png", plot = p,
       width = 12, height = 8, dpi = 300)
ggsave("BiasAuditHumanSCMC.pdf", plot = p,
       width = 12, height = 8)


# End

