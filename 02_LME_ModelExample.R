# Compare mixed model to linear model
# Want to incorporate distibutor as a variable

library(lme4)
library(ggplot2)
library(dplyr)
library(reshape2)

# Read data ####
modeldata <- readRDS("SampleDataset.RDS")

# Explore data ####
head(modeldata)
length(unique(modeldata$PolNumber)) # ~10,000 unique policyholders
length(unique(modeldata$DistCode)) # >300 unique distributors
summary(as.numeric(table(modeldata$DistCode))) # Some distributors are represented by as few as 1 quarterly record, and most <30

# Create holdout subset ####
# Guarantee that each distributor has observations in each subset
set.seed(2)
modeldata <- modeldata %>%
  group_by(DistCode) %>%
  mutate(Sample = ifelse(rep(n(), n()) == 1, 
                "training",
                ifelse(row_number() %in% sample(1:n(), ceiling(n()/2), replace = F),
                       "training", 
                       "holdout"))) %>%
  ungroup()

# Fit models ####
# Fit with distributor as a driver
# Each model takes 4 - 6 minutes

# GLM (without distributor)
system.time(
  glm.model.base <- glm(Surr ~  IN + Dur_IN + ITM:Dur_IN + Dur_OUT + ITM:Dur_OUT, 
                   data =  modeldata %>%
                     filter(Sample == "training"), 
                   family = "binomial")
)

# GLM
system.time(
  glm.model <- glm(Surr ~  IN + Dur_IN + ITM:Dur_IN + Dur_OUT + ITM:Dur_OUT + DistCode, 
                   data =  modeldata %>%
                     filter(Sample == "training"), 
                   family = "binomial")
)
# GLMM
system.time(
  lmm.model <- glmer(Surr ~  IN + Dur_IN + ITM:Dur_IN + Dur_OUT + ITM:Dur_OUT + (1 | DistCode), # Fits a random-effects intercept to each distributor
                     data = modeldata %>%
                       filter(Sample == "training"),
                     family = "binomial")
)

# Include predictions to original dataset
modeldata[["glm.pred"]] <- predict(glm.model, modeldata, type = "response")
modeldata[["lmm.pred"]] <- predict(lmm.model, modeldata, type = "response")
modeldata[["glm.pred.base"]] <- predict(glm.model.base, modeldata, type = "response")

saveRDS(glm.model,
        file = "IgnoreList/GLM_ModelObject.RDS")
saveRDS(lmm.model,
        file = "IgnoreList/LMM_ModelObject.RDS")
saveRDS(glm.model.base,
        file = "IgnoreList/GLM_base_ModelObject.RDS")
saveRDS(modeldata,
        file = "SampleDataset_withLMMPreds.RDS")

