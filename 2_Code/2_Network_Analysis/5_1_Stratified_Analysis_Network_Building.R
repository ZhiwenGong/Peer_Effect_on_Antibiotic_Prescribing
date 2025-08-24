# R code
# Stratifying physicians patient-sharing network by centrality and hospital


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
load(file.path(dir_data_clean, "network_data.RData"))



# -- 1. stratify by centrality
# ---- 1.1 calculate centrality for each physician
list_centrality <- lapply(names(list_phy_netw), function(the_year) {
  f_calc_centrality(list_phy_netw[[the_year]], the_year)
})
names(list_centrality) <- years


# ---- 1.2 stratify by degree centrality
#  higher degree centrality
list_adjmat_deg_h <- lapply(names(list_adjmat_cutoff), function(the_year) {
  f_filter_by_high_centrality(list_adjmat_cutoff[[the_year]],
    setNames(list_centrality[[the_year]]$c_deg, 
             list_centrality[[the_year]]$phy_code)) # set name for the vector
})
names(list_adjmat_deg_h) <- years
# lower degree centrality
list_adjmat_deg_l <- lapply(names(list_adjmat_cutoff), function(the_year) {
  adjmat <- list_adjmat_cutoff[[the_year]] - list_adjmat_deg_h[[the_year]]
})
names(list_adjmat_deg_l) <- years


# ---- 1.3 stratify by closeness centrality
# higher closeness centrality
list_adjmat_clo_h <- lapply(names(list_adjmat_cutoff), function(the_year) {
  f_filter_by_high_centrality(list_adjmat_cutoff[[the_year]],
    setNames(list_centrality[[the_year]]$c_clo, 
             list_centrality[[the_year]]$phy_code))
})
names(list_adjmat_clo_h) <- years
# lower closeness centrality
list_adjmat_clo_l <- lapply(names(list_adjmat_cutoff), function(the_year) {
  adjmat <- list_adjmat_cutoff[[the_year]] - list_adjmat_deg_h[[the_year]]
})
names(list_adjmat_clo_l) <- years


# ---- 1.4 stratify by betweenness centrality
# higher betweenness centrality
list_adjmat_bet_h <- lapply(names(list_adjmat_cutoff), function(the_year) {
  f_filter_by_high_centrality(list_adjmat_cutoff[[the_year]],
    setNames(list_centrality[[the_year]]$c_bet, 
             list_centrality[[the_year]]$phy_code))
})
names(list_adjmat_bet_h) <- years
# lower betweenness centrality
list_adjmat_bet_l <- lapply(names(list_adjmat_cutoff), function(the_year) {
  adjmat <- list_adjmat_cutoff[[the_year]] - list_adjmat_deg_h[[the_year]]
})
names(list_adjmat_bet_l) <- years



# -- 2. stratify by hospital
# ---- 2.1 hospital information
df_hos_info <- read.csv(file.path(dir_data_raw, "hospital_info.csv"))
df_hos_info <- df_hos_info %>%
  distinct(inst_code, phy_code, inst_tier, year)

list_hos_info <- split(df_hos_info, df_hos_info$year)
rm(df_hos_info)


# ---- 2.2 stratify by hospital
# same hospital
list_adjmat_inst_s <- lapply(names(list_adjmat_cutoff), function(the_year) {
  f_filter_by_same_hospital(list_adjmat_cutoff[[the_year]],
    setNames(list_hos_info[[the_year]]$inst_code,
             list_hos_info[[the_year]]$phy_code)) # set name for the vector
})
names(list_adjmat_inst_s) <- years

# different hospital
list_adjmat_inst_d <- lapply(names(list_adjmat_cutoff), function(the_year) {
  adjmat <- list_adjmat_cutoff[[the_year]] - list_adjmat_inst_s[[the_year]]
})
names(list_adjmat_inst_d) <- years



# ---- 3. stratify by both centrality and hospital
# ---- 3.1 stratify by degree centrality
# higher centrality
list_adjmat_deg_h_inst_s <- lapply(names(list_adjmat_deg_h), function(the_year) {
  f_filter_by_same_hospital(list_adjmat_deg_h[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_code,
                                     list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_deg_h_inst_s) <- years
list_adjmat_deg_h_inst_d <- lapply(names(list_adjmat_deg_h), function(the_year) {
  adjmat <- list_adjmat_deg_h[[the_year]] - list_adjmat_deg_h_inst_s[[the_year]]
})
names(list_adjmat_deg_h_inst_d) <- years
# lower centrality
list_adjmat_deg_l_inst_s <- lapply(names(list_adjmat_deg_l), function(the_year) {
  f_filter_by_same_hospital(list_adjmat_deg_l[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_code,
                                     list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_deg_l_inst_s) <- years
list_adjmat_deg_l_inst_d <- lapply(names(list_adjmat_deg_l), function(the_year) {
  adjmat <- list_adjmat_deg_l[[the_year]] - list_adjmat_deg_l_inst_s[[the_year]]
})
names(list_adjmat_deg_l_inst_d) <- years


# ---- 3.2 stratify by closeness centrality
# higher centrality
list_adjmat_clo_h_inst_s <- lapply(names(list_adjmat_clo_h), function(the_year) {
  f_filter_by_same_hospital(list_adjmat_clo_h[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_code,
                                     list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_clo_h_inst_s) <- years
list_adjmat_clo_h_inst_d <- lapply(names(list_adjmat_clo_h), function(the_year) {
  adjmat <- list_adjmat_clo_h[[the_year]] - list_adjmat_clo_h_inst_s[[the_year]]
})
names(list_adjmat_clo_h_inst_d) <- years
# lower centrality
list_adjmat_clo_l_inst_s <- lapply(names(list_adjmat_clo_l), function(the_year) {
  f_filter_by_same_hospital(list_adjmat_clo_l[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_code,
                                     list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_clo_l_inst_s) <- years
list_adjmat_clo_l_inst_d <- lapply(names(list_adjmat_clo_l), function(the_year) {
  adjmat <- list_adjmat_clo_l[[the_year]] - list_adjmat_clo_l_inst_s[[the_year]]
})
names(list_adjmat_clo_l_inst_d) <- years


# ---- 3.3 stratify by betweenness centrality
# higher centrality
list_adjmat_bet_h_inst_s <- lapply(names(list_adjmat_bet_h), function(the_year) {
  f_filter_by_same_hospital(list_adjmat_bet_h[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_code,
                                     list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_bet_h_inst_s) <- years
list_adjmat_bet_h_inst_d <- lapply(names(list_adjmat_bet_h), function(the_year) {
  adjmat <- list_adjmat_bet_h[[the_year]] - list_adjmat_bet_h_inst_s[[the_year]]
})
names(list_adjmat_bet_h_inst_d) <- years
# lower centrality
list_adjmat_bet_l_inst_s <- lapply(names(list_adjmat_bet_l), function(the_year) {
  f_filter_by_same_hospital(list_adjmat_bet_l[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_code,
                                     list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_bet_l_inst_s) <- years
list_adjmat_bet_l_inst_d <- lapply(names(list_adjmat_bet_l), function(the_year) {
  adjmat <- list_adjmat_bet_l[[the_year]] - list_adjmat_bet_l_inst_s[[the_year]]
})
names(list_adjmat_bet_l_inst_d) <- years



# ---- 4. stratify by hospital tier
# higher tier hospital
list_adjmat_tier_h <- lapply(names(list_adjmat_cutoff), function(the_year) {
  f_filter_by_higher_tier(list_adjmat_cutoff[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_tier,
                                     list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_tier_h) <- years

# lower tier hospital
list_adjmat_tier_l <- lapply(names(list_adjmat_cutoff), function(the_year) {
  f_filter_by_lower_tier(list_adjmat_cutoff[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_tier,
                                     list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_tier_l) <- years

# same tier hospital
list_adjmat_tier_s <- lapply(names(list_adjmat_cutoff), function(the_year) {
  adjmat <- list_adjmat_cutoff[[the_year]] - list_adjmat_tier_h[[the_year]] - list_adjmat_tier_l[[the_year]]
})
names(list_adjmat_tier_s) <- years



# -- 5. normalize all adjacency matrix
rm(list_adjmat_sampled, list_adjmat_cutoff, list_adjmat_normalized)

# get all adjacency matrix names
list_names <- ls(pattern = "^list_adjmat_")

# normalizing
processed <- lapply(mget(list_names), function(lst) lapply(lst, f_row_normalize))

list2env(processed, envir = .GlobalEnv)



# -- 6. save network data
keep_obj <- c(list_names, "threshold_abs","years", "dir_data_clean")
rm(list = setdiff(ls(), keep_obj))
gc()


save.image(file = file.path(dir_data_clean, "stratified_network_data.RData"))

