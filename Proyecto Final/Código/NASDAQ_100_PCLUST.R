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
# Load data set and put it into appropriate format
# ====================================================================================
NASDAQ_100_Stocks   <- read_excel("C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/NASDAQ_100.xlsx")
Df_matrix           <- array(as.numeric(unlist(NASDAQ_100_Stocks[ , -1])), dim=c(348, 101))
colnames(Df_matrix) <- colnames(NASDAQ_100_Stocks[, -1])
rownames(Df_matrix) <- rownames(NASDAQ_100_Stocks[, 1])
Df_matrix_T         <- t(Df_matrix)

# ====================================================================================
# Register Distances with proxy
# ====================================================================================
proxy::pr_DB$set_entry(FUN = diss.ACF,
                       names = c("ACFD"),
                       loop = TRUE,
                       type = "metric",
                       distance = TRUE,
                       description = "Autocorrelation-based distance")

proxy::pr_DB$set_entry(FUN = diss.PACF,
                       names = c("PACFD"),
                       loop = TRUE,
                       type = "metric",
                       distance = TRUE,
                       description = "Autocorrelation-based distance")

proxy::pr_DB$set_entry(FUN = diss.AR.LPC.CEPS,
                       names = c("AR.LPC"),
                       loop = TRUE,
                       type = "metric",
                       distance = TRUE,
                       description = "AR.LPC-based distance")

proxy::pr_DB$set_entry(FUN = diss.AR.PIC,
                       names = c("AR.PIC"),
                       loop = TRUE,
                       type = "metric",
                       distance = TRUE,
                       description = "AR distance")


# ====================================================================================
# Partitional clustering with Euclidean distance and k-means
# ====================================================================================
pc_k_means <- tsclust(Df_matrix_T,
                      k = 4,
                      type    = "partitional",
                      distance = "Euclidean",
                      centroid = "mean")

plot(pc_k_means, labs.arg=c(title='AA'))
plot(pc_k_means, type = "series")
plot(pc_k_means, type = "centroid")

# ====================================================================================
# Partitional clustering with DTW distance and k-means
# ====================================================================================
pc_k_means_dtw <- tsclust(Df_matrix_T,
                      k = 4,
                      type    = "partitional",
                      distance = "DTW",
                      centroid = "mean")

plot(pc_k_means_dtw)
plot(pc_k_means_dtw, type = "series")  
plot(pc_k_means_dtw, type = "centroid")

# ====================================================================================
# Partitional clustering with DTW distance and PAM
# ====================================================================================
pc_dtw_pam <- tsclust(Df_matrix_T,
                   k = 4,
                   type    = "partitional",
                   distance = "DTW",
                   centroid = "pam")

plot(pc_dtw_pam)
plot(pc_dtw_pam, type = "series")                                     
plot(pc_dtw_pam, type = "centroid")

# ====================================================================================
# Partitional clustering with ACF distance and PAM 
# ====================================================================================
pc_ACFD <- tsclust(Df_matrix_T,
                  k = 4,
                  type = "partitional",
                  distance = "ACFD",
                  centroid = "pam")

plot(pc_ACFD)                                     
plot(pc_ACFD, type = "series")                                     
plot(pc_ACFD, type = "centroid")  

# ====================================================================================
# Partitional clustering with PACF distance and PAM 
# ====================================================================================
pc_PACFD <- tsclust(Df_matrix_T,
                   k = 4,
                   type = "partitional",
                   distance = "PACFD",
                   centroid = "pam")
plot(pc_PACFD)                                     
plot(pc_PACFD, type = "series")                                     
plot(pc_PACFD, type = "centroid")  

# ====================================================================================
# Partitional clustering with AR.PIC distance and PAM 
# ====================================================================================
pc_AR.PIC <- tsclust(Df_matrix_T,
                     k = 4L,
                     type = "partitional",
                     distance = "AR.PIC",
                     centroid = "pam",
                     norm = "L2")

plot(pc_AR.PIC)                                     
plot(pc_AR.PIC, type = "series")
plot(pc_AR.PIC, type = "centroid")   

# ====================================================================================
# Partitional clustering with LPC distance and PAM 
# ====================================================================================
pc_AR.LPC <- tsclust(Df_matrix_T,
                   k = 4L,
                   type = "partitional",
                   distance = "AR.LPC",
                   centroid = "pam",
                   norm = "L2")
plot(pc_AR.LPC)
plot(pc_AR.LPC, type = "series")
plot(pc_AR.LPC, type = "centroid")   

# ====================================================================================
# Partitional clustering with k-Shape
# ====================================================================================
# we load standarized data
Symbols <- read_csv("C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/NASDAQ_100_Symbols.csv")
NASDAQ_100_Train <- read_excel("C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/NASDAQ_100_train.xlsx")
dim(NASDAQ_100_Train)
Df_matrix_Train = array(as.numeric(unlist(NASDAQ_100_Train[ , -1])), dim=c(348, 101))
colnames(Df_matrix_Train) <- colnames(NASDAQ_100_Train[, -1])
rownames(Df_matrix_Train) <- rownames(NASDAQ_100_Train[, 1])
Df_matrix_Train_T <- t(Df_matrix_Train)

pc_shape <- tsclust(Df_matrix_Train_T,
                    k = 4L,
                    type = "partitional",
                    distance = "SBD",
                    centroid = "shape",
                    trace = TRUE,
                    seed = 8,
                    norm = "L2",
                    window.size = 20L,
                    args = tsclust_args(cent = list(trace = TRUE)))

plot(pc_shape)
plot(pc_shape, type = "centroid")   

# save standarized data
output <- matrix(unlist(pc_shape@datalist), ncol = 101, byrow = F)
write.csv(output, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/NASDAQ_100_kShape_std.csv")

# ====================================================================================
# Formed Clusters
# ====================================================================================
pc_k_means_euc_groups <- data.frame(pc_k_means@cluster)
pc_k_means_dtw_groups <- data.frame(pc_k_means_dtw@cluster)
pc_pam_dtw_groups     <- data.frame(pc_dtw_pam@cluster)
pc_pam_acf_groups     <- data.frame(pc_ACFD@cluster)
pc_pam_pacf_groups    <- data.frame(pc_PACFD@cluster)
pc_pam_arpic_groups   <- data.frame(pc_AR.PIC@cluster)
pc_pam_arlpc_groups   <- data.frame(pc_AR.LPC@cluster)
pc_shape_groups       <- data.frame(pc_shape@cluster)

# ====================================================================================
# Matrix to save the formed clusters
# ====================================================================================
df_groups <- matrix(c(pc_k_means_euc_groups$pc_k_means.cluster,
                     pc_k_means_dtw_groups$pc_k_means_dtw.cluster,
                     pc_pam_dtw_groups$pc_dtw_pam.cluster,
                     pc_pam_acf_groups$pc_ACFD.cluster,
                     pc_pam_pacf_groups$pc_PACFD.cluster,
                     pc_pam_arpic_groups$pc_AR.PIC.cluster,
                     pc_pam_arlpc_groups$pc_AR.LPC.cluster,
                     pc_pam_arlpc_groups$pc_AR.LPC.cluster), nrow = 101)

write.csv(df_groups, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/NASDAQ_100_Pclust_groups.csv")

# ====================================================================================
# Matrices to save the formed centroids
# ====================================================================================
centroids_means_euc <- matrix(c(unlist(pc_k_means@centroids[1]),
                                unlist(pc_k_means@centroids[2]),
                                unlist(pc_k_means@centroids[3]),
                                unlist(pc_k_means@centroids[4])), nrow=348)

centroids_means_dtw <- matrix(c(unlist(pc_k_means_dtw@centroids[1]),
                                unlist(pc_k_means_dtw@centroids[2]),
                                unlist(pc_k_means_dtw@centroids[3]),
                                unlist(pc_k_means_dtw@centroids[4])), nrow=348)

centroids_pam_dtw   <- matrix(c(unlist(pc_dtw_pam@centroids[1]),
                                unlist(pc_dtw_pam@centroids[2]),
                                unlist(pc_dtw_pam@centroids[3]),
                                unlist(pc_dtw_pam@centroids[4])), nrow=348)

centroids_pam_acf   <- matrix(c(unlist(pc_ACFD@centroids[1]),
                                unlist(pc_ACFD@centroids[2]),
                                unlist(pc_ACFD@centroids[3]),
                                unlist(pc_ACFD@centroids[4])), nrow=348)

centroids_pam_pacf  <- matrix(c(unlist(pc_PACFD@centroids[1]),
                                unlist(pc_PACFD@centroids[2]),
                                unlist(pc_PACFD@centroids[3]),
                                unlist(pc_PACFD@centroids[4])), nrow=348)

centroids_pam_arpic  <- matrix(c(unlist(pc_AR.PIC@centroids[1]),
                                unlist(pc_AR.PIC@centroids[2]),
                                unlist(pc_AR.PIC@centroids[3]),
                                unlist(pc_AR.PIC@centroids[4])), nrow=348)

centroids_pam_arlpc  <- matrix(c(unlist(pc_AR.LPC@centroids[1]),
                                unlist(pc_AR.LPC@centroids[2]),
                                unlist(pc_AR.LPC@centroids[3]),
                                unlist(pc_AR.LPC@centroids[4])), nrow=348)

centroids_pam_shape  <- matrix(c(unlist(pc_shape@centroids[1]),
                                unlist(pc_shape@centroids[2]),
                                unlist(pc_shape@centroids[3]),
                                unlist(pc_shape@centroids[4])), nrow=348)

write.csv(centroids_means_euc, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_means_euc.csv")
write.csv(centroids_means_dtw, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_means_dtw.csv")
write.csv(centroids_pam_dtw,   "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_pam_dtw.csv")
write.csv(centroids_pam_acf,   "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_pam_acf.csv")
write.csv(centroids_pam_pacf,  "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_pam_pacf.csv")
write.csv(centroids_pam_arpic, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_pam_arpic.csv")
write.csv(centroids_pam_arlpc, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_pam_arlpc.csv")
write.csv(centroids_pam_shape, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/centroids_pam_shape.csv")