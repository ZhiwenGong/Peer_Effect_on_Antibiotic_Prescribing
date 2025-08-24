# R code
# Generate data set for regression - stratified analysis


# -- 0. package & path
library(igraph)
library(dplyr)

dir_work <- "*"
dir_data_raw <- file.path(dir_work, "1_Data/1_Data_Raw")
dir_data_clean <- file.path(dir_work, "1_Data/2_Data_Clean")
dir_code <- file.path(dir_work, "2_Code")

# import customized functions
source(file.path(dir_code, "0_Functions.R"))

# import network data
load(file.path(dir_data_clean, "stratified_network_data.RData"))



# -- 1. appropriate antibiotic prescription rate for individual physician and peers
df_ap <- read.csv(file.path(dir_data_raw, "antibiotic_prescription_sum.csv"))

df_ap_rate <- df_ap %>%
  # gen appropriate antibiotic prescribing rate
  mutate(rate_appropriate = 1-(sum_ap_t3/sum_ap_year)) %>%
  select(phy_code, rate_appropriate, year)

list_rate_appropriate <- split(df_ap_rate, df_ap_rate$year)
rm(df_ap, df_ap_rate)



# ---- 1.1 stratify by centrality
# ------ 1.1.1 by degree centrality
list_ap_rate_deg <- lapply(names(list_adjmat_deg_h), function(the_year) {
  f_get_peer_rate_multi_mat(
    list(list_adjmat_deg_h[[the_year]], list_adjmat_deg_l[[the_year]]),
    list_rate_appropriate[[the_year]],
    the_year,
    c("deg_h", "deg_l")
  )
})
# rbind
df_ap_rate_deg <- do.call(rbind, list_ap_rate_deg)
rm(list_ap_rate_deg)
rm(list_adjmat_deg_h, list_adjmat_deg_l)


# ------ 1.1.2 by closeness centrality
list_ap_rate_clo <- lapply(names(list_adjmat_clo_h), function(the_year) {
  f_get_peer_rate_multi_mat(
    list(list_adjmat_clo_h[[the_year]], list_adjmat_clo_l[[the_year]]),
    list_rate_appropriate[[the_year]],
    the_year,
    c("clo_h", "clo_l")
  )
})
# rbind
df_ap_rate_clo <- do.call(rbind, list_ap_rate_clo)
rm(list_ap_rate_clo)
rm(list_adjmat_clo_h, list_adjmat_clo_l)


# ------ 1.1.3 by betweenness centrality
list_ap_rate_bet <- lapply(names(list_adjmat_bet_h), function(the_year) {
  f_get_peer_rate_multi_mat(
    list(list_adjmat_bet_h[[the_year]], list_adjmat_bet_l[[the_year]]),
    list_rate_appropriate[[the_year]],
    the_year,
    c("bet_h", "bet_l")
  )
})
# rbind
df_ap_rate_bet <- do.call(rbind, list_ap_rate_bet)
rm(list_ap_rate_bet)
rm(list_adjmat_bet_h, list_adjmat_bet_l)



# ---- 1.2 stratify by hospital
list_ap_rate_inst <- lapply(names(list_adjmat_inst_s), function(the_year) {
  f_get_peer_rate_multi_mat(
    list(list_adjmat_inst_s[[the_year]], list_adjmat_inst_d[[the_year]]),
    list_rate_appropriate[[the_year]],
    the_year,
    c("inst_s", "inst_d")
  )
})
# rbind
df_ap_rate_inst <- do.call(rbind, list_ap_rate_inst)
rm(list_ap_rate_inst)
rm(list_adjmat_inst_s, list_adjmat_inst_d)



# ---- 1.3 stratify by both centrality and hospital
# ------ 1.3.1 by degree centrality
list_ap_rate_deg_inst <- lapply(names(list_adjmat_deg_h_inst_s), function(the_year) {
  f_get_peer_rate_multi_mat(
    list(list_adjmat_deg_h_inst_s[[the_year]], list_adjmat_deg_h_inst_d[[the_year]],
         list_adjmat_deg_l_inst_s[[the_year]], list_adjmat_deg_l_inst_d[[the_year]]),
    list_rate_appropriate[[the_year]],
    the_year,
    c("deg_h_inst_s", "deg_h_inst_d", "deg_l_inst_s", "deg_l_inst_d")
  )
})
# rbind
df_ap_rate_deg_inst <- do.call(rbind, list_ap_rate_deg_inst)
rm(list_ap_rate_deg_inst)
rm(list_adjmat_deg_h_inst_s, list_adjmat_deg_h_inst_d,
   list_adjmat_deg_l_inst_s, list_adjmat_deg_l_inst_d)


# ------ 1.3.2 by closeness centrality
list_ap_rate_clo_inst <- lapply(names(list_adjmat_clo_h_inst_s), function(the_year) {
  f_get_peer_rate_multi_mat(
    list(list_adjmat_clo_h_inst_s[[the_year]], list_adjmat_clo_h_inst_d[[the_year]],
         list_adjmat_clo_l_inst_s[[the_year]], list_adjmat_clo_l_inst_d[[the_year]]),
    list_rate_appropriate[[the_year]],
    the_year,
    c("clo_h_inst_s", "clo_h_inst_d", "clo_l_inst_s", "clo_l_inst_d")
  )
})
# rbind
df_ap_rate_clo_inst <- do.call(rbind, list_ap_rate_clo_inst)
rm(list_ap_rate_clo_inst)
rm(list_adjmat_clo_h_inst_s, list_adjmat_clo_h_inst_d,
   list_adjmat_clo_l_inst_s, list_adjmat_clo_l_inst_d)


# ------ 1.3.3 by betweenness centrality
list_ap_rate_bet_inst <- lapply(names(list_adjmat_bet_h_inst_s), function(the_year) {
  f_get_peer_rate_multi_mat(
    list(list_adjmat_bet_h_inst_s[[the_year]], list_adjmat_bet_h_inst_d[[the_year]],
         list_adjmat_bet_l_inst_s[[the_year]], list_adjmat_bet_l_inst_d[[the_year]]),
    list_rate_appropriate[[the_year]],
    the_year,
    c("bet_h_inst_s", "bet_h_inst_d", "bet_l_inst_s", "bet_l_inst_d")
  )
})
# rbind
df_ap_rate_bet_inst <- do.call(rbind, list_ap_rate_bet_inst)
rm(list_ap_rate_bet_inst)
rm(list_adjmat_bet_h_inst_s, list_adjmat_bet_h_inst_d,
   list_adjmat_bet_l_inst_s, list_adjmat_bet_l_inst_d)



# ---- 1.4 stratify by hospital tier
list_ap_rate_tier <- lapply(names(list_adjmat_tier_h), function(the_year) {
  f_get_peer_rate_multi_mat(
    list(list_adjmat_tier_h[[the_year]], list_adjmat_tier_s[[the_year]],
         list_adjmat_tier_l[[the_year]]),
    list_rate_appropriate[[the_year]],
    the_year,
    c("tier_h", "tier_s", "tier_l")
  )
})
# rbind
df_ap_rate_tier <- do.call(rbind, list_ap_rate_tier)
rm(list_ap_rate_tier)
rm(list_adjmat_tier_h, list_adjmat_tier_s, list_adjmat_tier_l)

rm(list_rate_appropriate)



# -- 2. co-variate 
load(file.path(dir_data_clean, "reg_data.RData"))

df_covar <- df_for_reg %>% 
  select(phy_code, year, 
         sum_pat_tier, pct_male_tier, pct_age_over65_tier, pct_cci_over2_tier,
         inst_code, inst_tier)
rm(df_for_reg)



# -- 3. join co-variate 
# get all df names
list_names <- ls(pattern = "^df_ap_rate_")

# join
list_df_for_reg <- lapply(mget(list_names), function(lst) {
  df_for_reg <- lst %>%
    inner_join(df_covar, by = c("phy_code","year"))
})



# -- 4. save data
keep_obj <- c("list_df_for_reg",
              "threshold_abs",
              "dir_data_clean")
rm(list = setdiff(ls(), keep_obj))
gc()


save.image(file = file.path(dir_data_clean, "stratified_reg_data.RData"))


