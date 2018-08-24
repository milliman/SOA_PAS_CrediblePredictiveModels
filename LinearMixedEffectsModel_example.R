# Compare mixed model to linear model
# Want to incorporate distibutor as a variable

library(lme4)
library(ggplot2)
library(dplyr)
library(reshape2)

# Read data ####
modeldata <- readRDS("SampleDataset.RDS")

# Modify distributor variable
modeldata <- modeldata %>%
  mutate(DistCode = factor(DistCode))

# Explore data ####
head(modeldata)
length(unique(modeldata$PolNumber)) # ~10,000 unique policyholders
length(unique(modeldata$DistCode)) # >300 unique distributors
summary(as.numeric(table(modeldata$DistCode))) # Some distributors are represented by as few as 1 quarterly records, and most <30

# Fit models ####
# Fit with distributor as a driver
# Each model takes 2 - 3 minutes

# GLM
system.time(
  glm.model <- glm(Surr ~  IN + ITM*Dur_IN + ITM*Dur_OUT + DistCode, 
                   data =  modeldata, 
                   family = "binomial")
)
# GLMM
system.time(
  lmm.model <- glmer(Surr ~  IN + ITM*Dur_IN + ITM*Dur_OUT + (1 | DistCode), # Fits a random-effects intercept to each distributor
                     data = modeldata,
                     family = "binomial")
)

# Assess models ####
# GLM predictions range from 0 to 1 because of overfitting to distributors
summary(glm.model$fitted.values)

# LMM predictions have a more believable range
summary(fitted(lmm.model))

# Similar fixed effects between models
summary(glm.model)$coef[setdiff(1:length(coef(glm.model)), grep("DistCode", names(coef(glm.model)))),]
summary(lmm.model)

# Line up DistCode coefficients
coef.df <- data.frame(DistCode = gsub("DistCode", "", grep("DistCode", names(coef(glm.model)), value = T)),
                      Coef = coef(glm.model)[grep("DistCode", names(coef(glm.model)))]) %>%
  left_join(data.frame(DistCode = rownames(coef(lmm.model)$DistCode),
                       Coef = coef(lmm.model)$DistCode$`(Intercept)`),
            by = "DistCode",
            suffix = c("_GLM", "_LMM")) %>%
  mutate(Coef_GLM = Coef_GLM + coef(glm.model)[1]) # include global intercept for each distributor effect

dist.N <- modeldata %>%
  group_by(DistCode) %>%
  summarize(N = n()) %>%
  ungroup()

C <- coef.df %>% 
  melt(id.vars = "DistCode", 
       measure.vars = c("Coef_GLM", "Coef_LMM")) %>% 
  mutate(y = ifelse(variable == "Coef_GLM", 1, 0),
         DistCode = factor(DistCode)) %>%
  left_join(dist.N, "DistCode")

C %>%
  ggplot(aes(x = value, y = y, group = DistCode)) +
  geom_point() + 
  geom_line(aes(size = log(N)), alpha = 0.25, color = "navy") +
  xlab("Log-odds coefficient (LMM)") + 
  ylab("") +
  ggtitle("Log-odds coefficient (GLM)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_blank()) +
  scale_size_continuous(guide = F, range = c(0.25, 1))

ggsave("ShrinkagePlot.png", width = 7, height = 4)
