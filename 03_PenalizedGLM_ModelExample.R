# Fit the same model, but shrink the distributor codes using penalization
# Use the base GLM fit in the 02_LME_ModelExample script as an offset for this example

# library(glmnet)

# Read data/model ####
glm.model.base <- readRDS("IgnoreList/GLM_base_ModelObject.RDS")
modeldata <- readRDS("SampleDataset.RDS")
# Note that the offset should be in the units of the link response (i.e. logodds, not probabilities)
modeldata[["offset"]] <- predict(glm.model.base, modeldata, type = "link")

# Prepare model matrix ####
# Consider using dummyVars() function from caret package
x.train <- model.matrix(object = ~ -1 + DistCode,
                        data = modeldata %>% 
                          filter(Sample == "training"),
                        contrasts.arg = contrasts(modeldata$DistCode[modeldata$Sample == "training"], 
                                                  contrasts = FALSE))


# Fit penalized GLM ####

# Elastic net fit
# Takes 5 - 6 minutes
system.time(
  cv.glmnet.model <- cv.glmnet(x = x.train,
                               y = modeldata$Surr[modeldata$Sample == "training"],
                               family = "binomial",
                               standardize = T,
                               intercept = T,
                               alpha = 0.5, # 50% LASSO and 50% Ridge penalties
                               nfolds = 5,
                               offset = modeldata$offset[modeldata$Sample == "training"])
  )

plot.cv.glmnet(cv.glmnet.model, sign.lambda=-1)


# Summarize coefficients ####
C <- coef(cv.glmnet.model, s = "lambda.min")
# Intercept
C[setdiff(c(1:nrow(C)), grep("DistCode", rownames(C))),]
# Summary of distributor coefficients
summary(C[grep("DistCode", rownames(C)),])


