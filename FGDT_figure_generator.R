


# This code reproduces the figures in the paper:
#     Using LLM Digital Twin Simulation to Evaluate the Emergent Properties of Human Group Interaction: 
#     With an Application to Focus Group Design





library(foreign)
library(ggplot2)
library(ggridges)
#library(ggpubr)
#library(viridis)
library(tidyverse)

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


## Figure 1: Nuclear Power, four options

# N=600: 8323757419
# N=60: 3708584978
# N=6: 5045433629

# use the following code if four standpoint options (see below if you have 3 standpoint options)

# trial id 8323757419 is 600 participant, 4 option on nuclear power
setwd(paste0(project_dir,"/trial_id_8323757419"))

l1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in large N response 1 matrix
l2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in large N response 2 matrix
l3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in large N response 3 matrix
l4<-as.matrix(read.csv("similarityMatrix_standpoint3.csv", header = FALSE)) # read in large N response 4 matrix

# trial id 3708584978 is 60 participant
setwd(paste0(project_dir,"/trial_id_3708584978"))

m1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in medium N response 1 matrix
m2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in medium N response 2 matrix
m3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in medium N response 3 matrix
m4<-as.matrix(read.csv("similarityMatrix_standpoint3.csv", header = FALSE)) # read in medium N response 4 matrix

# trial id 5045433629 is 6 participant
setwd(paste0(project_dir,"/trial_id_5045433629"))

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


# rate that at least one topic not discussed, in one or both runs
# length(l1vec_valid[l1vec_valid<=0.1])/length(l1vec_valid)
# length(l2vec_valid[l2vec_valid<=0.1])/length(l2vec_valid)
# length(l3vec_valid[l3vec_valid<=0.1])/length(l3vec_valid)
# length(l4vec_valid[l4vec_valid<=0.1])/length(l4vec_valid)
# length(m1vec_valid[m1vec_valid<=0.1])/length(m1vec_valid)
# length(m2vec_valid[m2vec_valid<=0.1])/length(m2vec_valid)
# length(m3vec_valid[m3vec_valid<=0.1])/length(m3vec_valid)
# length(m4vec_valid[m4vec_valid<=0.1])/length(m4vec_valid)
# length(s1vec_valid[s1vec_valid<=0.1])/length(s1vec_valid)
# length(s2vec_valid[s2vec_valid<=0.1])/length(s2vec_valid)
# length(s3vec_valid[s3vec_valid<=0.1])/length(s3vec_valid)
# length(s4vec_valid[s4vec_valid<=0.1])/length(s4vec_valid)


# make all the same length for creating the figure
l1vec_valid<-as.vector(sample(l1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l2vec_valid<-as.vector(sample(l2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l3vec_valid<-as.vector(sample(l3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l4vec_valid<-as.vector(sample(l4vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m1vec_valid<-as.vector(sample(m1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m2vec_valid<-as.vector(sample(m2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m3vec_valid<-as.vector(sample(m3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m4vec_valid<-as.vector(sample(m4vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s1vec_valid<-as.vector(sample(s1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s2vec_valid<-as.vector(sample(s2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s3vec_valid<-as.vector(sample(s3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s4vec_valid<-as.vector(sample(s4vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure



## Main figure

nrows<-length(s1vec_valid)
ncols<-4 # this is the number of rows in the figure
figdata1<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata1[i+3*nrows,1] <- s4vec_valid[i]
      figdata1[i+3*nrows,2] <- j}
    if (j==2) {
      figdata1[i+2*nrows,1] <- s3vec_valid[i]
      figdata1[i+2*nrows,2] <- j}
    if (j==3) {
      figdata1[i+1*nrows,1] <- s2vec_valid[i]
      figdata1[i+1*nrows,2] <- j}
    if (j==4) {
      figdata1[i,1] <- s1vec_valid[i]
      figdata1[i,2] <- j}
  }
}

figdata1[,2] <- factor(
  figdata1[,2],
  levels = c("1", "2", "3", "4"),
  labels = c("Research", "Phase Out", "Maintain", "Expand")
)

figdata1$Design <- rep(1,length(figdata1[,1]))


nrows<-length(s1vec)
ncols<-4 # this is the number of rows in the figure
figdata2<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata2[i+3*nrows,1] <- m4vec_valid[i]
      figdata2[i+3*nrows,2] <- j}
    if (j==2) {
      figdata2[i+2*nrows,1] <- m3vec_valid[i]
      figdata2[i+2*nrows,2] <- j}
    if (j==3) {
      figdata2[i+1*nrows,1] <- m2vec_valid[i]
      figdata2[i+1*nrows,2] <- j}
    if (j==4) {
      figdata2[i,1] <- m1vec_valid[i]
      figdata2[i,2] <- j}
  }
}

figdata2[,2] <- factor(
  figdata2[,2],
  levels = c("1", "2", "3", "4"),
  labels = c("Research", "Phase Out", "Maintain", "Expand")
)

figdata2$Design <- rep(2,length(figdata2[,1]))


nrows<-length(s1vec)
ncols<-4 # this is the number of rows in the figure
figdata3<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata3[i+3*nrows,1] <- l4vec_valid[i]
      figdata3[i+3*nrows,2] <- j}
    if (j==2) {
      figdata3[i+2*nrows,1] <- l3vec_valid[i]
      figdata3[i+2*nrows,2] <- j}
    if (j==3) {
      figdata3[i+1*nrows,1] <- l2vec_valid[i]
      figdata3[i+1*nrows,2] <- j}
    if (j==4) {
      figdata3[i,1] <- l1vec_valid[i]
      figdata3[i,2] <- j}
  }
}

figdata3[,2] <- factor(
  figdata3[,2],
  levels = c("1", "2", "3", "4"),
  labels = c("Research", "Phase Out", "Maintain", "Expand")
)

figdata3$Design <- rep(3,length(figdata2[,1]))



figdata <- rbind(figdata3, figdata2, figdata1)

figdata[,3] <- factor(
  figdata[,3],
  levels = c("1", "2", "3"),
  labels = c("N=6", "N=60", "N=600")
)


theme_set(theme_ridges())


ggplot(figdata, aes(x=figdata[,1], y=figdata[,2], fill=Design)) +
  geom_density_ridges(alpha=0.5) +
  scale_fill_brewer(palette = 5) +
  labs(
    title = 'Figure 1: Structured Group Simulation Results - 4 options',
    #subtitle = 'Cohen\'s D',
    x = "Cosine Similarity Distribution"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())



## Figure 2: Nuclear Power, three options


# N=600: 2307512764
# N=60: 8971956292
# N=6: 5231569904


# ******** If only three standpoint options, use this:

# trial id 2307512764
setwd(paste0(project_dir,"/trial_id_2307512764"))
l1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in large N response 1 matrix
l2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in large N response 2 matrix
l3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in large N response 3 matrix
# trial id 8971956292
setwd(paste0(project_dir,"/trial_id_8971956292"))
m1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in medium N response 1 matrix
m2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in medium N response 2 matrix
m3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in medium N response 3 matrix
# trial id 5231569904
setwd(paste0(project_dir,"/trial_id_5231569904"))
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

l1vec_valid[l1vec_valid==1]<-0
l2vec_valid[l2vec_valid==1]<-0
l3vec_valid[l3vec_valid==1]<-0
m1vec_valid[m1vec_valid==1]<-0
m2vec_valid[m2vec_valid==1]<-0
m3vec_valid[m3vec_valid==1]<-0
s1vec_valid[s1vec_valid==1]<-0
s2vec_valid[s2vec_valid==1]<-0
s3vec_valid[s3vec_valid==1]<-0


# make all the same length for creating the figure
l1vec_valid<-as.vector(sample(l1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l2vec_valid<-as.vector(sample(l2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l3vec_valid<-as.vector(sample(l3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m1vec_valid<-as.vector(sample(m1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m2vec_valid<-as.vector(sample(m2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m3vec_valid<-as.vector(sample(m3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s1vec_valid<-as.vector(sample(s1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s2vec_valid<-as.vector(sample(s2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s3vec_valid<-as.vector(sample(s3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure



## Main figure

nrows<-length(s1vec_valid)
ncols<-3 # this is the number of rows in the figure
figdata1<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata1[i+2*nrows,1] <- s3vec_valid[i]
      figdata1[i+2*nrows,2] <- j}
    if (j==2) {
      figdata1[i+1*nrows,1] <- s2vec_valid[i]
      figdata1[i+1*nrows,2] <- j}
    if (j==3) {
      figdata1[i,1] <- s1vec_valid[i]
      figdata1[i,2] <- j}
  }
}

figdata1[,2] <- factor(
  figdata1[,2],
  levels = c("1", "2", "3"),
  labels = c("Phase Out", "Maintain", "Expand")
)

figdata1$Design <- rep(1,length(figdata1[,1]))


nrows<-length(s1vec)
ncols<-3 # this is the number of rows in the figure
figdata2<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata2[i+2*nrows,1] <- m3vec_valid[i]
      figdata2[i+2*nrows,2] <- j}
    if (j==2) {
      figdata2[i+1*nrows,1] <- m2vec_valid[i]
      figdata2[i+1*nrows,2] <- j}
    if (j==3) {
      figdata2[i,1] <- m1vec_valid[i]
      figdata2[i,2] <- j}
  }
}

figdata2[,2] <- factor(
  figdata2[,2],
  levels = c("1", "2", "3"),
  labels = c("Phase Out", "Maintain", "Expand")
)

figdata2$Design <- rep(2,length(figdata2[,1]))


nrows<-length(s1vec)
ncols<-3 # this is the number of rows in the figure
figdata3<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata3[i+2*nrows,1] <- l3vec_valid[i]
      figdata3[i+2*nrows,2] <- j}
    if (j==2) {
      figdata3[i+1*nrows,1] <- l2vec_valid[i]
      figdata3[i+1*nrows,2] <- j}
    if (j==3) {
      figdata3[i,1] <- l1vec_valid[i]
      figdata3[i,2] <- j}
  }
}

figdata3[,2] <- factor(
  figdata3[,2],
  levels = c("1", "2", "3"),
  labels = c("Phase Out", "Maintain", "Expand")
)

figdata3$Design <- rep(3,length(figdata2[,1]))



figdata <- rbind(figdata3, figdata2, figdata1)

figdata[,3] <- factor(
  figdata[,3],
  levels = c("1", "2", "3"),
  labels = c("N=6", "N=60", "N=600")
)



theme_set(theme_ridges())


ggplot(figdata, aes(x=figdata[,1], y=figdata[,2], fill=Design)) +
  geom_density_ridges(alpha=0.5) +
  scale_fill_brewer(palette = 5) +
  labs(
    title = 'Figure 2: Structured Group Simulation Results - 3 options',
    #subtitle = 'Cohen\'s D',
    x = "Cosine Similarity Distribution"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())




## Figure 3: Nuclear Power, four option extreme skew 

# N=600: 9649295679
# N=60: 5830640642
# N=6: 5569406254

# use the following code if four standpoint options 

# trial id 9649295679 is 600 participant, 4 option on nuclear power
setwd(paste0(project_dir,"/trial_id_9649295679"))

l1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in large N response 1 matrix
l2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in large N response 2 matrix
l3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in large N response 3 matrix
l4<-as.matrix(read.csv("similarityMatrix_standpoint3.csv", header = FALSE)) # read in large N response 4 matrix

# trial id 5830640642 is 60 participant
setwd(paste0(project_dir,"/trial_id_5830640642"))

m1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in medium N response 1 matrix
m2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in medium N response 2 matrix
m3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in medium N response 3 matrix
m4<-as.matrix(read.csv("similarityMatrix_standpoint3.csv", header = FALSE)) # read in medium N response 4 matrix

# trial id 5569406254 is 6 participant
setwd(paste0(project_dir,"/trial_id_5569406254"))

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


# rate that at least one topic not discussed, in one or both runs
# length(l1vec_valid[l1vec_valid<=0.1])/length(l1vec_valid)
# length(l2vec_valid[l2vec_valid<=0.1])/length(l2vec_valid)
# length(l3vec_valid[l3vec_valid<=0.1])/length(l3vec_valid)
# length(l4vec_valid[l4vec_valid<=0.1])/length(l4vec_valid)
# length(m1vec_valid[m1vec_valid<=0.1])/length(m1vec_valid)
# length(m2vec_valid[m2vec_valid<=0.1])/length(m2vec_valid)
# length(m3vec_valid[m3vec_valid<=0.1])/length(m3vec_valid)
# length(m4vec_valid[m4vec_valid<=0.1])/length(m4vec_valid)
# length(s1vec_valid[s1vec_valid<=0.1])/length(s1vec_valid)
# length(s2vec_valid[s2vec_valid<=0.1])/length(s2vec_valid)
# length(s3vec_valid[s3vec_valid<=0.1])/length(s3vec_valid)
# length(s4vec_valid[s4vec_valid<=0.1])/length(s4vec_valid)


# make all the same length for creating the figure
l1vec_valid<-as.vector(sample(l1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l2vec_valid<-as.vector(sample(l2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l3vec_valid<-as.vector(sample(l3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l4vec_valid<-as.vector(sample(l4vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m1vec_valid<-as.vector(sample(m1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m2vec_valid<-as.vector(sample(m2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m3vec_valid<-as.vector(sample(m3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m4vec_valid<-as.vector(sample(m4vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s1vec_valid<-as.vector(sample(s1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s2vec_valid<-as.vector(sample(s2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s3vec_valid<-as.vector(sample(s3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s4vec_valid<-as.vector(sample(s4vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure



## Main figure

nrows<-length(s1vec_valid)
ncols<-4 # this is the number of rows in the figure
figdata1<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata1[i+3*nrows,1] <- s4vec_valid[i]
      figdata1[i+3*nrows,2] <- j}
    if (j==2) {
      figdata1[i+2*nrows,1] <- s3vec_valid[i]
      figdata1[i+2*nrows,2] <- j}
    if (j==3) {
      figdata1[i+1*nrows,1] <- s2vec_valid[i]
      figdata1[i+1*nrows,2] <- j}
    if (j==4) {
      figdata1[i,1] <- s1vec_valid[i]
      figdata1[i,2] <- j}
  }
}

figdata1[,2] <- factor(
  figdata1[,2],
  levels = c("1", "2", "3", "4"),
  labels = c("Research", "Phase Out", "Maintain", "Expand")
)

figdata1$Design <- rep(1,length(figdata1[,1]))


nrows<-length(s1vec)
ncols<-4 # this is the number of rows in the figure
figdata2<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata2[i+3*nrows,1] <- m4vec_valid[i]
      figdata2[i+3*nrows,2] <- j}
    if (j==2) {
      figdata2[i+2*nrows,1] <- m3vec_valid[i]
      figdata2[i+2*nrows,2] <- j}
    if (j==3) {
      figdata2[i+1*nrows,1] <- m2vec_valid[i]
      figdata2[i+1*nrows,2] <- j}
    if (j==4) {
      figdata2[i,1] <- m1vec_valid[i]
      figdata2[i,2] <- j}
  }
}

figdata2[,2] <- factor(
  figdata2[,2],
  levels = c("1", "2", "3", "4"),
  labels = c("Research", "Phase Out", "Maintain", "Expand")
)

figdata2$Design <- rep(2,length(figdata2[,1]))


nrows<-length(s1vec)
ncols<-4 # this is the number of rows in the figure
figdata3<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata3[i+3*nrows,1] <- l4vec_valid[i]
      figdata3[i+3*nrows,2] <- j}
    if (j==2) {
      figdata3[i+2*nrows,1] <- l3vec_valid[i]
      figdata3[i+2*nrows,2] <- j}
    if (j==3) {
      figdata3[i+1*nrows,1] <- l2vec_valid[i]
      figdata3[i+1*nrows,2] <- j}
    if (j==4) {
      figdata3[i,1] <- l1vec_valid[i]
      figdata3[i,2] <- j}
  }
}

figdata3[,2] <- factor(
  figdata3[,2],
  levels = c("1", "2", "3", "4"),
  labels = c("Research", "Phase Out", "Maintain", "Expand")
)

figdata3$Design <- rep(3,length(figdata2[,1]))



figdata <- rbind(figdata3, figdata2, figdata1)

figdata[,3] <- factor(
  figdata[,3],
  levels = c("1", "2", "3"),
  labels = c("N=6", "N=60", "N=600")
)


theme_set(theme_ridges())


ggplot(figdata, aes(x=figdata[,1], y=figdata[,2], fill=Design)) +
  geom_density_ridges(alpha=0.5) +
  scale_fill_brewer(palette = 5) +
  labs(
    title = 'Figure 3: Structured Group Simulation Results - Extreme Skew',
    #subtitle = 'Cohen\'s D',
    x = "Cosine Similarity Distribution"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())




## Figure 4: Gun Control, including stratification design

# 60 simple randomization: 3634605881
# 6 stratified design: 5447221142
# 6 simple randomization: 7087039549


# ******** If only three standpoint options, use this:

# trial id 3634605881
setwd(paste0(project_dir,"/trial_id_3634605881"))
l1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in large N response 1 matrix
l2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in large N response 2 matrix
l3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in large N response 3 matrix
# trial id 5447221142
setwd(paste0(project_dir,"/trial_id_5447221142"))
m1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in medium N response 1 matrix
m2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in medium N response 2 matrix
m3<-as.matrix(read.csv("similarityMatrix_standpoint2.csv", header = FALSE)) # read in medium N response 3 matrix
# trial id 7087039549
setwd(paste0(project_dir,"/trial_id_7087039549"))
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

l1vec_valid[l1vec_valid==1]<-0
l2vec_valid[l2vec_valid==1]<-0
l3vec_valid[l3vec_valid==1]<-0
m1vec_valid[m1vec_valid==1]<-0
m2vec_valid[m2vec_valid==1]<-0
m3vec_valid[m3vec_valid==1]<-0
s1vec_valid[s1vec_valid==1]<-0
s2vec_valid[s2vec_valid==1]<-0
s3vec_valid[s3vec_valid==1]<-0


# make all the same length for creating the figure
l1vec_valid<-as.vector(sample(l1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l2vec_valid<-as.vector(sample(l2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l3vec_valid<-as.vector(sample(l3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m1vec_valid<-as.vector(sample(m1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m2vec_valid<-as.vector(sample(m2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m3vec_valid<-as.vector(sample(m3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s1vec_valid<-as.vector(sample(s1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s2vec_valid<-as.vector(sample(s2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s3vec_valid<-as.vector(sample(s3vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure



## Main figure

nrows<-length(s1vec_valid)
ncols<-3 # this is the number of rows in the figure
figdata1<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata1[i+2*nrows,1] <- s3vec_valid[i]
      figdata1[i+2*nrows,2] <- j}
    if (j==2) {
      figdata1[i+1*nrows,1] <- s2vec_valid[i]
      figdata1[i+1*nrows,2] <- j}
    if (j==3) {
      figdata1[i,1] <- s1vec_valid[i]
      figdata1[i,2] <- j}
  }
}

figdata1[,2] <- factor(
  figdata1[,2],
  levels = c("1", "2", "3"),
  labels = c("Less Strict", "About Right", "More Strict")
)

figdata1$Design <- rep(1,length(figdata1[,1]))


nrows<-length(s1vec)
ncols<-3 # this is the number of rows in the figure
figdata2<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata2[i+2*nrows,1] <- m3vec_valid[i]
      figdata2[i+2*nrows,2] <- j}
    if (j==2) {
      figdata2[i+1*nrows,1] <- m2vec_valid[i]
      figdata2[i+1*nrows,2] <- j}
    if (j==3) {
      figdata2[i,1] <- m1vec_valid[i]
      figdata2[i,2] <- j}
  }
}

figdata2[,2] <- factor(
  figdata2[,2],
  levels = c("1", "2", "3"),
  labels = c("Less Strict", "About Right", "More Strict")
)

figdata2$Design <- rep(2,length(figdata2[,1]))


nrows<-length(s1vec)
ncols<-3 # this is the number of rows in the figure
figdata3<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata3[i+2*nrows,1] <- l3vec_valid[i]
      figdata3[i+2*nrows,2] <- j}
    if (j==2) {
      figdata3[i+1*nrows,1] <- l2vec_valid[i]
      figdata3[i+1*nrows,2] <- j}
    if (j==3) {
      figdata3[i,1] <- l1vec_valid[i]
      figdata3[i,2] <- j}
  }
}

figdata3[,2] <- factor(
  figdata3[,2],
  levels = c("1", "2", "3"),
  labels = c("Less Strict", "About Right", "More Strict")
)

figdata3$Design <- rep(3,length(figdata2[,1]))



figdata <- rbind(figdata3, figdata2, figdata1)

figdata[,3] <- factor(
  figdata[,3],
  levels = c("1", "2", "3"),
  labels = c("N=6 (simple)", "N=6 (stratified)", "N=60 (simple)")
)



theme_set(theme_ridges())


ggplot(figdata, aes(x=figdata[,1], y=figdata[,2], fill=Design)) +
  geom_density_ridges(alpha=0.5) +
  scale_fill_brewer(palette = 5) +
  labs(
    title = 'Figure 4: Structured Group Simulation Results - with Stratification',
    #subtitle = 'Cohen\'s D',
    x = "Cosine Similarity Distribution"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())




## Figure 5: Bias Audit, human data house of representatives

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

ggplot(figdata, aes(x=figdata[,1], y=figdata[,3], fill=Position)) +
  geom_density_ridges(alpha=0.5, show.legend = TRUE) +
  scale_fill_brewer(palette = 5) +
  labs(
    title = 'Figure 5: Bias Audit - Human Data',
    #subtitle = 'Cohen\'s D',
    x = "Cosine Similarity Distribution"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())




## Figure A1: Bias Audit, simulated data nuclear power


# similarities in project id 8323757419

setwd(paste0(project_dir,"/trial_id_8323757419"))


data <- read.csv("ResponsesDF_similarities_n0.csv")

data <-
  data %>%
  mutate(maxsim = pmax(similarityColumn1, similarityColumn2, similarityColumn3, similarityColumn4, similarityColumn5, na.rm = TRUE),
         college = ifelse(education == "College graduate/some postgrad" | education == "Postgraduate", yes = 1, no = 0),
         female = ifelse(gender == "Female", yes = 1, no = 0),
         conservative = ifelse(politics == "Conservative" | politics == "Very conservative", yes = 1, no = 0),
         openai = ifelse(model =="openai/gpt-oss-120b", yes = 1, no = 0)
  )



## Main figure

figdata1a<-cbind(data$maxsim[data$female==0],data$openai[data$female==0])
figdata1b<-cbind(data$maxsim[data$female==1],data$openai[data$female==1])
figdata2a<-cbind(data$maxsim[data$college==0],data$openai[data$college==0])
figdata2b<-cbind(data$maxsim[data$college==1],data$openai[data$college==1])


figdata1a<-cbind(figdata1a, rep(1, length(figdata1a)))
figdata1b<-cbind(figdata1b, rep(2, length(figdata1b)))
figdata2a<-cbind(figdata2a, rep(3, length(figdata2a)))
figdata2b<-cbind(figdata2b, rep(4, length(figdata2b)))

figdata<-rbind(figdata1a, figdata1b, figdata2a, figdata2b)

figdata<-as.data.frame(figdata)


figdata[,3] <- factor(
  figdata[,3],
  levels = c("1", "2", "3", "4"),
  labels = c("Male", "Female", "NotCollege", "College")
)

figdata[,2]<- factor(
  figdata[,2],
  levels = c("0", "1"),
  labels = c("NotChatGPT", "ChatGPT")
)

LLM<-figdata[,2]

theme_set(theme_ridges())

ggplot(figdata, aes(x=figdata[,1], y=figdata[,3], fill=LLM)) +
  geom_density_ridges(alpha=0.5, show.legend = TRUE) +
  scale_fill_brewer(palette = 5) +
  labs(
    title = 'Figure A1: Bias Audit - Simulated Data on Nuclear Power',
    #subtitle = 'Cohen\'s D',
    x = "Cosine Similarity Distribution"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())




## Figure A2: Simulated house of representatives


# N=600: 8847511010
# N=60: 3990491471
# N=6: 6451141693

# ******** If only two standpoint options, use this:

# trial id 8847511010
setwd(paste0(project_dir,"/trial_id_8847511010"))
l1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in large N response 1 matrix
l2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in large N response 2 matrix
# trial id 3990491471
setwd(paste0(project_dir,"/trial_id_3990491471"))
m1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in medium N response 1 matrix
m2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in medium N response 2 matrix
# trial id 6451141693
setwd(paste0(project_dir,"/trial_id_6451141693"))
s1<-as.matrix(read.csv("similarityMatrix_standpoint0.csv", header = FALSE)) # read in small N response 1 matrix
s2<-as.matrix(read.csv("similarityMatrix_standpoint1.csv", header = FALSE)) # read in small N response 2 matrix

l1vec<-l1[lower.tri(l1)]
l2vec<-l2[lower.tri(l2)]
m1vec<-m1[lower.tri(m1)]
m2vec<-m2[lower.tri(m2)]
s1vec<-s1[lower.tri(s1)]
s2vec<-s2[lower.tri(s2)]

l1vec_valid<-l1vec
l2vec_valid<-l2vec
m1vec_valid<-m1vec
m2vec_valid<-m2vec
s1vec_valid<-s1vec
s2vec_valid<-s2vec

l1vec_valid[l1vec_valid==1]<-0
l2vec_valid[l2vec_valid==1]<-0
m1vec_valid[m1vec_valid==1]<-0
m2vec_valid[m2vec_valid==1]<-0
s1vec_valid[s1vec_valid==1]<-0
s2vec_valid[s2vec_valid==1]<-0

# Note: 9 percent of SCMC small N do not discuss response 2

# make all the same length for creating the figure
l1vec_valid<-as.vector(sample(l1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
l2vec_valid<-as.vector(sample(l2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m1vec_valid<-as.vector(sample(m1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
m2vec_valid<-as.vector(sample(m2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s1vec_valid<-as.vector(sample(s1vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure
s2vec_valid<-as.vector(sample(s2vec_valid, length(s1vec_valid), replace=TRUE)) # make the same length for figure



## Main figure

nrows<-length(s1vec_valid)
ncols<-2 # this is the number of rows in the figure
figdata1<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata1[i+1*nrows,1] <- s2vec_valid[i]
      figdata1[i+1*nrows,2] <- j}
    if (j==2) {
      figdata1[i,1] <- s1vec_valid[i]
      figdata1[i,2] <- j}
  }
}

figdata1[,2] <- factor(
  figdata1[,2],
  levels = c("1", "2"),
  labels = c("Keep Same", "Increase")
)

figdata1$Design <- rep(1,length(figdata1[,1]))


nrows<-length(s1vec)
ncols<-2 # this is the number of rows in the figure
figdata2<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata2[i+1*nrows,1] <- m2vec_valid[i]
      figdata2[i+1*nrows,2] <- j}
    if (j==2) {
      figdata2[i,1] <- m1vec_valid[i]
      figdata2[i,2] <- j}
  }
}

figdata2[,2] <- factor(
  figdata2[,2],
  levels = c("1", "2"),
  labels = c("Keep Same", "Increase")
)

figdata2$Design <- rep(2,length(figdata2[,1]))


nrows<-length(s1vec)
ncols<-2 # this is the number of rows in the figure
figdata3<-as.data.frame(matrix(rep(NA, nrows*ncols*2), nrow = nrows*ncols, ncol=2))

for (j in 1:ncols) {
  for (i in 1:nrows) {
    if (j==1) {
      figdata3[i+1*nrows,1] <- l2vec_valid[i]
      figdata3[i+1*nrows,2] <- j}
    if (j==2) {
      figdata3[i,1] <- l1vec_valid[i]
      figdata3[i,2] <- j}
  }
}

figdata3[,2] <- factor(
  figdata3[,2],
  levels = c("1", "2"),
  labels = c("Keep Same", "Increase")
)

figdata3$Design <- rep(3,length(figdata2[,1]))



figdata <- rbind(figdata3, figdata2, figdata1)

figdata[,3] <- factor(
  figdata[,3],
  levels = c("1", "2", "3"),
  labels = c("N=6", "N=60", "N=600")
)



theme_set(theme_ridges())

ggplot(figdata, aes(x=figdata[,1], y=figdata[,2], fill=Design)) +
  geom_density_ridges(alpha=0.5) +
  scale_fill_brewer(palette = 5) +
  labs(
    title = 'Figure A2: Structured Group Simulation Results - Expand House',
    #subtitle = 'Cohen\'s D',
    x = "Cosine Similarity Distribution"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())




## Figure A3: Bias Audit, simulated house of representatives


setwd(paste0(project_dir,"/trial_id_8847511010"))

data <- read.csv("ResponsesDF_similarities_n0.csv")

data <-
  data %>%
  mutate(maxsim = pmax(similarityColumn1, similarityColumn2, similarityColumn3, similarityColumn4, similarityColumn5, na.rm = TRUE),
         college = ifelse(education == "College graduate/some postgrad" | education == "Postgraduate", yes = 1, no = 0),
         female = ifelse(gender == "Female", yes = 1, no = 0),
         conservative = ifelse(politics == "Conservative" | politics == "Very conservative", yes = 1, no = 0),
         openai = ifelse(model =="openai/gpt-oss-120b", yes = 1, no = 0)
  )



## Main figure

figdata1a<-cbind(data$maxsim[data$female==0],data$openai[data$female==0])
figdata1b<-cbind(data$maxsim[data$female==1],data$openai[data$female==1])
figdata2a<-cbind(data$maxsim[data$college==0],data$openai[data$college==0])
figdata2b<-cbind(data$maxsim[data$college==1],data$openai[data$college==1])


figdata1a<-cbind(figdata1a, rep(1, length(figdata1a)))
figdata1b<-cbind(figdata1b, rep(2, length(figdata1b)))
figdata2a<-cbind(figdata2a, rep(3, length(figdata2a)))
figdata2b<-cbind(figdata2b, rep(4, length(figdata2b)))

figdata<-rbind(figdata1a, figdata1b, figdata2a, figdata2b)

figdata<-as.data.frame(figdata)


figdata[,3] <- factor(
  figdata[,3],
  levels = c("1", "2", "3", "4"),
  labels = c("Male", "Female", "NotCollege", "College")
)

figdata[,2]<- factor(
  figdata[,2],
  levels = c("0", "1"),
  labels = c("NotChatGPT", "ChatGPT")
)

LLM<-figdata[,2]

theme_set(theme_ridges())

ggplot(figdata, aes(x=figdata[,1], y=figdata[,3], fill=LLM)) +
  geom_density_ridges(alpha=0.5, show.legend = TRUE) +
  scale_fill_brewer(palette = 5) +
  labs(
    title = 'Figure A3: Bias Audit - Simulated Data on Expand House',
    #subtitle = 'Cohen\'s D',
    x = "Cosine Similarity Distribution"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())



## Unstructured:

# 60 simple randomization: 4841071884
# 6 stratified:
# 6 simple randomization: 5897463167


# No figures for the unstructured design...
