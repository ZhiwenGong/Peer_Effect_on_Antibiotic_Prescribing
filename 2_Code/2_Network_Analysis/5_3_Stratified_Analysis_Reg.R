# R code
# Regression - stratified analysis


# -- 0. package & path
library(nlme)
library(dplyr)
library(tidyr)
library(tibble)
library(performance)

dir_work <- "*"
dir_data_raw <- file.path(dir_work, "1_Data/1_Data_Raw")
dir_data_clean <- file.path(dir_work, "1_Data/2_Data_Clean")
dir_code <- file.path(dir_work, "2_Code")

# import customized functions
source(file.path(dir_code, "0_Functions.R"))

# import network data
load(file.path(dir_data_clean, "stratified_reg_data.RData"))



# -- 1. stratify by centrality
# ---- 1.1 by degree centrality
model_var <- lme(rate_individual ~ deg_h + deg_l +
               sum_pat_tier + pct_male_tier + pct_age_over65_tier + pct_cci_over2_tier +
               inst_tier, 
             random = ~ 1 | inst_code/phy_code,
             data = list_df_for_reg[["df_ap_rate_deg"]])
f_tidy_lme_result(model_var)


# ---- 1.2 by closeness centrality
model_var <- lme(rate_individual ~ clo_h + clo_l +
                   sum_pat_tier + pct_male_tier + pct_age_over65_tier + pct_cci_over2_tier +
                   inst_tier, 
                 random = ~ 1 | inst_code/phy_code,
                 data = list_df_for_reg[["df_ap_rate_clo"]])
f_tidy_lme_result(model_var)


# ---- 1.3 by betweenness centrality
model_var <- lme(rate_individual ~ bet_h + bet_l +
                   sum_pat_tier + pct_male_tier + pct_age_over65_tier + pct_cci_over2_tier +
                   inst_tier, 
                 random = ~ 1 | inst_code/phy_code,
                 data = list_df_for_reg[["df_ap_rate_bet"]])
f_tidy_lme_result(model_var)



# -- 2. stratify by hospital
model_var <- lme(rate_individual ~ inst_s + inst_d +
                   sum_pat_tier + pct_male_tier + pct_age_over65_tier + pct_cci_over2_tier +
                   inst_tier, 
                 random = ~ 1 | inst_code/phy_code,
                 data = list_df_for_reg[["df_ap_rate_inst"]])
f_tidy_lme_result(model_var)



# -- 3. stratify by both centrality and hospital
# ---- 3.1 by degree centralit
model_var <- lme(rate_individual ~ deg_h_inst_s + deg_h_inst_d + deg_l_inst_s + deg_l_inst_d +
                   sum_pat_tier + pct_male_tier + pct_age_over65_tier + pct_cci_over2_tier +
                   inst_tier, 
                 random = ~ 1 | inst_code/phy_code,
                 data = list_df_for_reg[["df_ap_rate_deg_inst"]])
f_tidy_lme_result(model_var)


# ---- 3.2 by closeness centrality
model_var <- lme(rate_individual ~ clo_h_inst_s + clo_h_inst_d + clo_l_inst_s + clo_l_inst_d +
                   sum_pat_tier + pct_male_tier + pct_age_over65_tier + pct_cci_over2_tier +
                   inst_tier, 
                 random = ~ 1 | inst_code/phy_code,
                 data = list_df_for_reg[["df_ap_rate_clo_inst"]])
f_tidy_lme_result(model_var)


# ---- 3.3 by betweenness centrality
model_var <- lme(rate_individual ~ bet_h_inst_s + bet_h_inst_d + bet_l_inst_s + bet_l_inst_d +
                   sum_pat_tier + pct_male_tier + pct_age_over65_tier + pct_cci_over2_tier +
                   inst_tier, 
                 random = ~ 1 | inst_code/phy_code,
                 data = list_df_for_reg[["df_ap_rate_bet_inst"]])
f_tidy_lme_result(model_var)



# -- 4. stratify by hospital tier
model_var <- lme(rate_individual ~ tier_h + tier_s + tier_l +
                   sum_pat_tier + pct_male_tier + pct_age_over65_tier + pct_cci_over2_tier +
                   inst_tier, 
                 random = ~ 1 | inst_code/phy_code,
                 data = list_df_for_reg[["df_ap_rate_tier"]])
f_tidy_lme_result(model_var)
