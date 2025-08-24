# R code
# Generate data set for regression


# -- 0. package & path
library(igraph)
library(dplyr)

dir_work <- "*"
dir_data_raw <- file.path(dir_work, "1_Data/1_Data_Raw")
dir_data_clean <- file.path(dir_work, "1_Data/2_Data_Clean")
dir_code <- file.path(dir_work, "2_Code/2_Network_Analysis")

# import customized functions
source(file.path(dir_code, "0_Functions.R"))

# import network data
load(file.path(dir_data_clean, "network_data.RData"))



# -- 1. appropriate antibiotic prescription rate for individual physician and peers
df_ap <- read.csv(file.path(dir_data_raw, "antibiotic_prescription_sum.csv"))

df_ap_rate <- df_ap %>%
  # gen appropriate antibiotic prescribing rate
  mutate(rate_appropriate = 1-(sum_ap_t3/sum_ap_year)) %>%
  select(phy_code, rate_appropriate, year)

list_rate_appropriate <- split(df_ap_rate, df_ap_rate$year)

# calculate peers' appropriate antibiotic prescribing rate
list_ap_rate <- lapply(names(list_adjmat_normalized), function(the_year) {
  f_get_peer_rate(list_adjmat_normalized[[the_year]], 
                  list_rate_appropriate[[the_year]],
                  the_year)
})

# rbind
df_ap_rate <- do.call(rbind, list_ap_rate)
rm(df_ap, list_ap_rate, list_rate_appropriate)
rm(list_adjmat_normalized)

# peer rate in prior year
df_ap_rate <- df_ap_rate %>%
  arrange(phy_code, year) %>%
  group_by(phy_code) %>%
  mutate(rate_peer_prior_yr = lag(rate_peer, order_by = year)) %>% 
  ungroup() %>%
  filter(!is.na(rate_peer_prior_yr))



# -- 2. co-variate 
# ---- 2.1 centrality
# calculate centrality
list_covar_centrality <- lapply(names(list_phy_netw), function(the_year) {
  f_calc_centrality(list_phy_netw[[the_year]], the_year)
})
# rbind
df_covar_centrality <- do.call(rbind, list_covar_centrality)
rm(list_phy_netw, list_covar_centrality)


# ---- 2.2 physicians co-var
df_covar_phy <- read.csv(file.path(dir_data_raw, "physician_info.csv"))
# calculate physician characteristics
df_covar_phy <- df_covar_phy %>%
  mutate(pct_male = sum_male / sum_pat,
         pct_age_over65 = sum_age_over65/sum_pat,
         pct_cci_over2 = sum_cci_over2/sum_pat) %>% 
  select(phy_code, year, sum_pat, pct_male, pct_age_over65, pct_cci_over2)


# ---- 2.3 hospitals co-var
df_covar_hos <- read.csv(file.path(dir_data_raw, "hospital_info.csv"))
df_covar_hos <- df_covar_hos %>%
  distinct(inst_code, phy_code, inst_tier, year)


# ---- 2.4 join
df_for_reg <- df_ap_rate %>%
  inner_join(df_covar_centrality, by = c("phy_code","year")) %>%
  inner_join(df_covar_phy, by = c("phy_code","year")) %>% 
  inner_join(df_covar_hos, by = c("phy_code","year"))

rm(df_ap_rate, df_covar_centrality, df_covar_phy, df_covar_hos)




# -- 3. data clean
# ---- 3.1 Function: trans continuous var to categorical var
f_apply_ntile <- function(data, vars) {
  
  data <- data %>%
    mutate(across(
      all_of(vars),
      ~{
        qs <- quantile(.x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE, names = FALSE)
        if (length(unique(qs)) < 4) {
          factor(dplyr::ntile(.x, 3), levels = 1:3, labels = c("low", "medium", "high"))
        } else {
          cut(.x, breaks = qs, labels = c("low", "medium", "high"), include.lowest = TRUE)
        }
      },
      .names = "{.col}_tier"
    ))

  return(data)
}

vars = c("c_deg", "c_clo", "c_bet",
         "sum_pat", "pct_male", "pct_age_over65", "pct_cci_over2")
df_for_reg <- f_apply_ntile(df_for_reg, vars)


# ---- 3.2 set factor var
df_for_reg$inst_tier <- factor(df_for_reg$inst_tier, levels = c("1", "2", "3"))


keep_obj <- c("df_for_reg",
              "threshold_abs",
              "dir_data_clean")
rm(list = setdiff(ls(), keep_obj))
gc()


save.image(file = file.path(dir_data_clean, "reg_data.RData"))

