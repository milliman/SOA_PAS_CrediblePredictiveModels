library(dplyr)

# How many quarterly records?
num.records <- 200000

# Create covariates
set.seed(1)
sampledata <- data.frame(CSV = rexp(num.records, 1/120000) + 1000,
                         q = pmin(55, ceiling(rexp(num.records, 1/12))),
                         SCPeriod = 7,
                         ITM = rnorm(num.records, 1.2, 0.15),
                         DistCode = as.factor(ceiling(rexp(num.records, 1/50)))) %>%
  mutate(IN = ifelse(q <= SCPeriod*4, 1, 0),
         Dur_IN = pmin(0, (q - SCPeriod*4)/(SCPeriod*4)),
         Dur_OUT = pmax(0, q - SCPeriod*4),
         GLWBBenBase = ITM/CSV)

# Set distributor effects
distributors <- data.frame(DistCode = unique(sampledata$DistCode),
                           Effect = rnorm(length(unique(sampledata$DistCode)), -4, 0.25))

# Create surrender response
modeldata <- sampledata %>%
  left_join(distributors,
            by = "DistCode") %>%
  mutate(surr_logodds = -1.3*IN + 1.6*ITM + 4.6*Dur_IN - 0.3*Dur_OUT - 2.6*ITM*Dur_IN + 0.15*ITM*Dur_OUT + Effect)

modeldata <- modeldata %>%
  mutate(Surr = ifelse(runif(n()) < 1 / (1 + exp(-surr_logodds)), 1, 0))

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

# Save data
saveRDS(modeldata,
        file = "SampleDataset.RDS")