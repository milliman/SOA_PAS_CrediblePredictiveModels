# Order of scripts
library(ggplot2)
library(dplyr)
library(reshape2)
library(lme4)
library(glmnet)

source("01_CreateStylizeddata.R")
source("02_LME_ModelExample.R")
source("02b_LME_ModelValidation.R")
source("03_PenalizedGLM_ModelExample.R")
