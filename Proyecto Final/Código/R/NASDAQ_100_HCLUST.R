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
library(ggplot2)
library(ggdendro)
library(gghighlight)
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
# Hierarchical clustering with Euclidean distance
# ====================================================================================
# Similarity Matrix
d_euc <- diss(Df_matrix_T, "EUCL")
# H. clustering
hc.d_euc <- hclust(d_euc, method = "complete")
# Dendrogram
ggdendrogram(hc.d_euc,  rotate = TRUE, theme_dendro = FALSE) + theme_minimal() + xlab('') + ylab('') + labs(title="Clústering Jerárquico con Distancia Euclídea de los valores en el NASDAQ-100")
# We cut the dendrogram to get k = 4 clusters
d_euc_groups  <- cutree(hc.d_euc, k=4)
df_euc_groups <- data.frame(d_euc_groups)


# ====================================================================================
# Hierarchical clustering with DTW distance
# ====================================================================================
d_dtw         <- diss(Df_matrix_T, "DTW")
hc.d_dtw      <- hclust(d_dtw, method = "complete")
ggdendrogram(hc.d_dtw, rotate = TRUE, theme_dendro = FALSE) + theme_minimal() + xlab('') + ylab('') + labs(title="Clústering Jerárquico con Distancia DTW de los valores en el NASDAQ-100")
d_dtw_groups  <- cutree(hc.d_dtw, k=4)
df_dtw_groups <- data.frame(d_dtw_groups)


# ====================================================================================
# Hierarchical clustering with ACF distance
# ====================================================================================
d_acf         <- diss(Df_matrix_T, "ACF")
hc.d_acf      <- hclust(d_acf, method = "complete")
ggdendrogram(hc.d_acf, rotate = TRUE, theme_dendro = FALSE) + theme_minimal() + xlab('') + ylab('')  + labs(title="Clústering Jerárquico con Distancia ACF de los valores en el NASDAQ-100")
d_acf_groups  <- cutree(hc.d_acf, k=4)
df_acf_groups <- data.frame(d_acf_groups)


# ====================================================================================
# Hierarchical clustering with PACF distance
# ====================================================================================
d_pacf         <- diss(Df_matrix_T, "PACF")
hc.d_pacf      <- hclust(d_pacf, method = "complete")
ggdendrogram(hc.d_pacf, rotate = TRUE, theme_dendro = FALSE) + theme_minimal() + xlab('') + ylab('')  + labs(title="Clústering Jerárquico con Distancia PACF de los valores en el NASDAQ-100")
d_pacf_groups  <- cutree(hc.d_pacf, k=4)
df_pacf_groups <- data.frame(d_pacf_groups)


# ====================================================================================
# Hierarchical clustering with DTW distance
# ====================================================================================
d_ar.pic         <- diss(Df_matrix_T, "AR.PIC")
hc.d_ar.pic      <- hclust(d_ar.pic, method = "complete")
ggdendrogram(hc.d_ar.pic , rotate = TRUE, theme_dendro = FALSE) + theme_minimal() + xlab('') + ylab('')  + labs(title="Clústering Jerárquico con Distancia ARIMA de Piccolo de los valores en el NASDAQ-100")
d_ar_pic_groups  <- cutree(hc.d_ar.pic, k=4)
df_ar_pic_groups <- data.frame(d_ar_pic_groups)


# ====================================================================================
# Hierarchical clustering with DTW distance
# ====================================================================================
d_ar.LPC <- diss(Df_matrix_T, "AR.LPC.CEPS")
hc.d_ar.LPC <- hclust(d_ar.LPC, method = "complete")
ggdendrogram(hc.d_ar.LPC, rotate = TRUE, theme_dendro = FALSE) + theme_minimal() + xlab('') + ylab('')  + labs(title="Clústering Jerárquico con Distancia LPC Cepstrum de los valores en el NASDAQ-100")
d_ar_lpc_groups  <- cutree(hc.d_ar.LPC, k=4)
df_ar_lpc_groups <- data.frame(d_ar_lpc_groups)

# ====================================================================================
# Data Frame que guarda los resultados del clústering
# ====================================================================================
df_groups = matrix(c(df_euc_groups$d_euc_groups,
                     df_dtw_groups$d_dtw_groups,
                     df_acf_groups$d_acf_groups,
                     df_pacf_groups$d_pacf_groups,
                     df_ar_pic_groups$d_ar_pic_groups,
                     df_ar_lpc_groups$d_ar_lpc_groups), nrow = 101)

write.csv(df_groups, "C:/Users/luis9/OneDrive/CIMAT PPE/Semestres/S04/Series de Tiempo/Proyecto de Final de Curso/Data/NASDAQ_100_Hclust_groups.csv")
