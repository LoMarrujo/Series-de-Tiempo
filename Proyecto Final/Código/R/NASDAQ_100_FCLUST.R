# ====================================================================================
# Load libraries and set up parallel computing
# ====================================================================================
library(dtwclust)
library(dtw)
library(lgcp)
library(TSclust)
library(readxl)
library(parallel)
library(foreach)
library(doParallel)
library(tidyverse)
cl <- makeCluster(detectCores() - 1)
invisible(clusterEvalQ(cl, library(dtwclust)))
registerDoParallel(cl, cores = detectCores() - 1)

# ====================================================================================
# Register Distances with proxy
# ====================================================================================
proxy::pr_DB$set_entry(FUN = diss.ACF,
                       names = c("ACFD"),
                       loop = TRUE,
                       type = "metric",
                       distance = TRUE,
                       description = "Autocorrelation-based distance")

proxy::pr_DB$set_entry(FUN = diss.AR.PIC,
                       names = c("AR.PIC"),
                       loop = TRUE,
                       type = "metric",
                       distance = TRUE,
                       description = "AR distance")

# ====================================================================================
# Load data set and put it into appropriate format
# ====================================================================================
NASDAQ_100_Stocks   <- read_excel("C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/NASDAQ_100.xlsx")
dim(NASDAQ_100_Stocks)

Df_matrix = array(as.numeric(unlist(NASDAQ_100_Stocks[ , -1])), dim=c(348, 101))

colnames(Df_matrix) <- colnames(NASDAQ_100_Stocks[, -1])
rownames(Df_matrix) <- rownames(NASDAQ_100_Stocks[, 1])

Df_matrix_T <- t(Df_matrix)

# ====================================================================================
# Fuzzy clustering with Euclidean distance
# ====================================================================================
fcmdd_euc <- tsclust(Df_matrix_T,
                      k = 4,
                      type    = "f",
                      distance = "Euclidean",
                      centroid = "fcmdd")

plot(fcmdd_euc)

# ====================================================================================
# Fuzzy clustering with ACFD distance
# ====================================================================================
fcmdd_ACFD <- tsclust(Df_matrix_T,
                     k = 4,
                     type    = "f",
                     distance = "ACFD",
                     centroid = "fcmdd")

plot(fcmdd_ACFD)

# ====================================================================================
# Fuzzy clustering with AR.LPC distance
# ====================================================================================
fcmdd_ar.pic <- tsclust(Df_matrix_T,
                      k = 4,
                      type    = "f",
                      distance = "AR.PIC",
                      centroid = "fcmdd")

plot(fcmdd_ar.pic)

# ====================================================================================
# Formed Clusters
# ====================================================================================
fcmdd_euc_groups <- data.frame(fcmdd_euc@cluster)
fcmdd_acf_groups <- data.frame(fcmdd_ACFD@cluster)
fcmdd_pic_groups <- data.frame(fcmdd_ar.pic@cluster)

# ====================================================================================
# Matrix to save the formed clusters
# ====================================================================================
df_groups <- matrix(c(fcmdd_euc_groups$fcmdd_euc.cluster,
                      fcmdd_acf_groups$fcmdd_ACFD.cluster,
                      fcmdd_pic_groups$fcmdd_ar.pic.cluster), nrow = 101)

write.csv(df_groups, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/NASDAQ_100_Fclust_groups.csv")

# ====================================================================================
# Matrices to save the formed centroids
# ====================================================================================
centroids_fcmdd_euc <- matrix(c(unlist(fcmdd_euc@centroids[1]),
                                unlist(fcmdd_euc@centroids[2]),
                                unlist(fcmdd_euc@centroids[3]),
                                unlist(fcmdd_euc@centroids[4])), nrow=348)

centroids_fcmdd_acf <- matrix(c(unlist(fcmdd_ACFD@centroids[1]),
                                unlist(fcmdd_ACFD@centroids[2]),
                                unlist(fcmdd_ACFD@centroids[3]),
                                unlist(fcmdd_ACFD@centroids[4])), nrow=348)

centroids_fcmdd_pic <- matrix(c(unlist(fcmdd_ar.pic@centroids[1]),
                                unlist(fcmdd_ar.pic@centroids[2]),
                                unlist(fcmdd_ar.pic@centroids[3]),
                                unlist(fcmdd_ar.pic@centroids[4])), nrow=348)

write.csv(centroids_fcmdd_euc, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_fcmdd_euc.csv")
write.csv(centroids_fcmdd_acf, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_fcmdd_acf.csv")
write.csv(centroids_fcmdd_pic, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_fcmdd_pic.csv")