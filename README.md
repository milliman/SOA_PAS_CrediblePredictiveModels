# Forming credible predictions with predictive models
This repository provides a examples of fitting a generalized linear model (GLM), a generalized linear mixed effects model (with lme4 package), a GLM with the LASSO penalty (with glmnet package), and a few examples of how to validate a model on a holdout dataset (specifcally with the mixed effects model). 

The repository includes a 00_MasterScript.R file that will run the scripts in an appropriate order so as to avoid errors. It also includes a script to create a stylized dataset for the purposes of model fitting, which contains no real policyholder data. However, the underlying trends between the covariates included and the reponse (a binary variable indicating the surrender of one's policy) are based on real data.

The contents of this repository were created to supplement Session 45: Assessing Credibility of Predictive Model at the SOA's 2018 Predictive Analytics Symposium.

This is provided for educational purposes only. 
