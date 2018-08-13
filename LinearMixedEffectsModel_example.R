library(lme4)
library(glmnet)

modeldata <- readRDS("V:/ESB/ReconUE/Work/Analysis/CleanData/INDUSTRY/2017Q4/ModelingData/SurrModel/Surr_DatasetSubset_withPreds.rds")

# lme4 example ####
lme.form <- as.formula(Surr ~ 
                         IN:q_rel_scaled_cap + q_rel_cap_ge0 +  
                         SCPhase + SCPhase:ITM_cap + (1 | PolNum))
lme1 <- glmer(lme.form,
              data = modeldata,
              family = binomial)

glm.form <- as.formula(Surr ~ 
                         IN:q_rel_scaled_cap + q_rel_cap_ge0 +  
                         SCPhase + SCPhase:ITM_cap)
glm1 <- glm(glm.form,
              data = modeldata,
              family = binomial)

# add to other scripts...
# glmnet penalized example ####

# MCMC example ####