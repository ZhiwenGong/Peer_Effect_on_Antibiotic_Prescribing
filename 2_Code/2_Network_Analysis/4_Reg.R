# R code
# Regression


# -- 0. package & path
library(nlme)
library(dplyr)
library(tidyr)
library(tibble)
library(performance)

dir_work <- "*"
dir_data_raw <- file.path(dir_work, "1_Data/1_Data_Raw")
dir_data_clean <- file.path(dir_work, "1_Data/2_Data_Clean")
dir_code <- file.path(dir_work, "2_Code/2_Network_Analysis")

# import customized functions
source(file.path(dir_code, "0_Functions.R"))

# import regression data
load(file.path(dir_data_clean, "reg_data.RData"))



# -- 1. regression with random interceptions
# ---- 1.1 base model
model_base <- lme(rate_individual ~ rate_peer_prior_yr, 
                     random = ~ 1 | inst_code/phy_code,
                     data = df_for_reg)
f_tidy_lme_result(model_base)


# ---- 1.2 model with co-vars
model_var <- lme(rate_individual ~ rate_peer_prior_yr +
               c_deg_tier + c_clo_tier + c_bet_tier +
               sum_pat_tier + pct_male_tier + pct_age_over65_tier + pct_cci_over2_tier +
               inst_tier, 
             random = ~ 1 | inst_code/phy_code,
             data = df_for_reg)
f_tidy_lme_result(model_var)


# check for collinearity
check_collinearity(model_var)


