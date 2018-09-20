library(glmnet)

# Read data ####
modeldata <- readRDS("SampleDataset.RDS")

# Fit penalized GLM ####
x <- model.matrix(~ IN + Dur_IN + ITM:Dur_IN + Dur_OUT + ITM:Dur_OUT + DistCode)