library(dplyr)
library(ggplot2)
library(lme4)
library(reshape2)

# Read data and models ####
modeldata <- readRDS("SampleDataset_withLMMPreds.RDS")

# Assess models ####
# GLM predictions range from 0 to 1 because of overfitting to distributors
summary(glm.model$fitted.values)

# LMM predictions have a more believable range
summary(fitted(lmm.model))

# Very similar fixed effects (coefficients) between models
summary(glm.model)$coef[setdiff(1:length(coef(glm.model)), grep("DistCode", names(coef(glm.model)))),]
summary(lmm.model)

# Validation ####

# Log-loss metric comparison
# There is some edge to using the lmm with DistCode over ignoring DistCode altogether
modeldata %>% 
  filter(Sample == "holdout") %>%
  summarize(lmm.logloss = -sum(Surr*log(lmm.pred) + (1 - Surr)*log(1 - lmm.pred)),
            glm.logloss = -sum(Surr*log(glm.pred) + (1 - Surr)*log(1 - glm.pred)),
            glm.base.logloss = -sum(Surr*log(glm.pred.base) + (1 - Surr)*log(1 - glm.pred.base)))
  
# Two-way lift chart
# Compare the A/E of competing models in areas where those models disagree
# The LMM (blue) has an A/E closer to 1 in the extremes of the x-axis (where there is model disagreement)
twoway.plot <- modeldata %>%
  filter(Sample == "holdout") %>%
  mutate(PredRatio = lmm.pred/glm.pred.base) %>%
  group_by(PredRatio.bucket = ntile(PredRatio, 20)) %>%
  summarize(AoverE.glm.base = sum(Surr)/sum(glm.pred.base),
            AoverE.lmm = sum(Surr)/sum(lmm.pred))

twoway.plot %>%
  melt(id.vars = "PredRatio.bucket", measure.vars = c("AoverE.glm.base", "AoverE.lmm")) %>%
  ggplot(aes(x = PredRatio.bucket, y = value)) +
  geom_line(aes(color = variable)) +
  geom_line(aes(y = 1), linetype = 2, color = "black")

# Create shrinkage plot ####
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

# Distributor summary
summary(coef.df$Coef_LMM)
exp(max(coef.df$Coef_LMM) - min(coef.df$Coef_LMM))
exp(quantile(coef.df$Coef_LMM, 0.975) - quantile(coef.df$Coef_LMM, 0.025))