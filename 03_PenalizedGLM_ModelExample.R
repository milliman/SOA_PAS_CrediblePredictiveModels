# Fit the same model, but shrink the distributor codes using penalization

# library(glmnet)

# Read data ####
modeldata <- readRDS("SampleDataset.RDS")

# Make "IN" (whether the policyholder is in surrender charge period) categorical
modeldata <- modeldata %>%
  mutate(IN = factor(IN))

# Prepare model matrix ####
x.train <- model.matrix(object = ~ IN + Dur_IN + ITM:Dur_IN + Dur_OUT + ITM:Dur_OUT + DistCode,
                  data = modeldata %>% 
                    filter(Sample == "training"),
                  contrasts.arg = lapply(modeldata %>% 
                                           filter(Sample == "training") %>% 
                                           select(IN, DistCode),
                                         contrasts,
                                         contrasts = FALSE))

x.train <- x.train[,-1] # remove the intercept column

# Fit penalized GLM ####
p.fac <- ifelse(1:ncol(x.train) %in% grep("DistCode", colnames(x.train)), 1, 0) # select which columns to be shrunk

# Elastic net fit ####
# Takes 5 - 6 minutes
system.time(
  cv.glmnet.model <- cv.glmnet(x = x.train,
                               y = modeldata$Surr[modeldata$Sample == "training"],
                               family = "binomial",
                               penalty.factor = p.fac,
                               standardize = T,
                               alpha = 0.5, # 50% LASSO and 50% Ridge penalties
                               nfolds = 5
                               #,
                               #offset = OFFSET_COLNAME) # this is where you input your offset
  )
)

plot.cv.glmnet(cv.glmnet.model, sign.lambda=-1)


# Summarize coefficients ####
C <- coef(cv.glmnet.model, s = "lambda.min")
C[setdiff(c(1:nrow(C)), grep("DistCode", rownames(C))),]
summary(C[grep("DistCode", rownames(C)),])


