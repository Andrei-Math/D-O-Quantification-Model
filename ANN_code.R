setwd("C:/Users/CraciunescuA/OneDrive - Willis Towers Watson/Cyber&D&O Pricing/D&O/Public/Data Prep")

library(dplyr)
#library(keras)
#library(magrittr)
#library(tibble)
#library(purrr)
#library(ggplot2)
#library(gridExtra)
library(splitTools)
library(tidyr)

options(encoding = 'UTF-8')
# set seed to obtain best reproducibility. note that the underlying architecture may affect results nonetheless, so full reproducibility cannot be guaranteed across different platforms.
seed <- 100
# Sys.setenv(PYTHONHASHSEED = seed)
set.seed(seed)
#reticulate::py_set_seed(seed)
#tensorflow::tf$random$set_seed(seed)

cdata = read.csv("cdata.csv")
cdata = cdata[,-1] # drop the index column when reading

# store the original data separately
odata <- cdata

# MinMax scaler
preprocess_minmax <- function(varData) {
  X <- as.numeric(varData)
  2 * (X - min(X)) / (max(X) - min(X)) - 1
}

# Dummy coding 
preprocess_catdummy <- function(data, varName, prefix) {
  varData <- data[[varName]]
  X <- as.integer(as.factor(varData))
  n0 <- length(unique(X))
  n1 <- 2:n0
  addCols <- purrr::map(n1, function(x, y) {as.integer(y == x)}, y = X) %>%
    rlang::set_names(paste0(prefix, n1))
  cbind(data, addCols)
}

# Feature pre-processing using MinMax Scaler and Dummy Coding
preprocess_features <- function(data) {
  data %>%
    #mutate_at(
    #  c(REVENUESX = "REVENUES"), #EMPLOYEESX = "EMPLOYEES"
    #  preprocess_minmax
    #) %>%
    mutate(
    is_USA_jurisX = as.integer(factor(ADR.Flag)) - 1.5,  # transform binary feature to -1/2, + 1/2
    RECENT_IPO = as.integer(factor(RECENT_IPO)) - 1.5  # transform binary feature to -1/2, + 1/2
    )%>%
    preprocess_catdummy("SUB_REGION", "R") %>%
    preprocess_catdummy("Sector", "S")
}

dat2 <- preprocess_features(cdata)
summary(dat2)

ind <- partition(dat2[["COMPANY_ID"]], p = c(train = 0.8, test = 0.2), 
                 seed = seed, type = "grouped")
train <- dat2[ind$train, ]
test <- dat2[ind$test, ]

# size of train/test
sprintf("Number of observations (train): %s", nrow(train))
sprintf("Number of observations (test): %s", nrow(test))
# Claims frequency of train/test
sprintf("Empirical frequency (train): %s", round(sum(train$ClaimNb) / nrow(train), 4))
## [1] "Empirical frequency (train): 0.0736"
sprintf("Empirical frequency (test): %s", round(sum(test$ClaimNb) / nrow(test), 4))


# select the feature space
col_start <- ncol(cdata)
col_end <- ncol(dat2)
features <- c(col_start:col_end)  # select features, be careful if pre-processing changes
print(colnames(train[, features]))

# feature matrix
Xtrain <- as.matrix(train[, features])  # design matrix training sample
Xtest <- as.matrix(test[, features])    # design matrix test sample
ytrain <- as.matrix(train[, c("ClaimNb")])  # design matrix training sample
ytest <- as.matrix(test[, c("ClaimNb")])    # design matrix test sample

rownames(Xtrain) <- NULL
rownames(Xtest) <- NULL

write.csv(Xtrain,"Xtrain.csv")
write.csv(Xtest,"Xtest.csv")
write.csv(ytrain,"ytrain.csv")
write.csv(ytest,"ytest.csv")


# # Initialization of neural network weights using empirical frequency
# # homogeneous model (train)
# lambda_hom <- sum(train$ClaimNb) / nrow(train)
# 
# 
# # define network and load pre-specified weights
# q0 <- length(features)                  # dimension of features
# q1 <- 20                                # number of hidden neurons in hidden layer
# 
# sprintf("Neural network with K=1 hidden layer")
# 
# sprintf("Input feature dimension: q0 = %s", q0)
# sprintf("Number of hidden neurons: q1 = %s", q1)
# sprintf("Output dimension: %s", 1)
# 
# Design  <- layer_input(shape = c(q0), dtype = 'float32', name = 'Design') 
# #LogVol  <- layer_input(shape = c(1), dtype = 'float32', name = 'LogVol')
# 
# Network <- Design %>%
#   layer_dense(units = q1, activation = 'tanh', name = 'layer1') #%>%
#   #layer_dense(units = 1, activation = 'linear', name = 'Network',
#               #weights = list(array(0, dim = c(q1, 1)), array(log(lambda_hom), dim = c(1))))
# 
# Response <- list(Network,) %>%
#   #layer_add(name = 'Add') %>%
#   layer_dense(units = 1, activation = k_exp, name = 'Response', trainable = FALSE,
#               weights = list(array(1, dim = c(1, 1)), array(0, dim = c(1))))
# 
# model_sh <- keras_model(inputs = c(Design), outputs = c(Response))