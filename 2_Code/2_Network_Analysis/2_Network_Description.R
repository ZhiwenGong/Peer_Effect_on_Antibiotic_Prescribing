# R code
# Description of network characteristics


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



# -- 1. Description of network without applying the threshold
# ---- 1.1 filter adjmat by hosital
# ------ 1.1.1 hospital information
df_hos_info <- read.csv(file.path(dir_data_raw, "hospital_info.csv"))
df_hos_info <- df_hos_info %>%
  distinct(inst_code, phy_code, inst_tier, year)

list_hos_info <- split(df_hos_info, df_hos_info$year)
rm(df_hos_info)

# ------ 1.1.2 whether in same hospital
list_adjmat_inst_s <- lapply(names(list_adjmat_sampled), function(the_year) {
  f_filter_by_same_hospital(list_adjmat_sampled[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_code,
                                     list_hos_info[[the_year]]$phy_code)) # set name for the vector
})
names(list_adjmat_inst_s) <- years

# ----- 1.1.3 by hosital tier
# higher tier hospital
list_adjmat_tier_h <- lapply(names(list_adjmat_sampled), function(the_year) {
  f_filter_by_higher_tier(list_adjmat_sampled[[the_year]],
                          setNames(list_hos_info[[the_year]]$inst_tier,
                                   list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_tier_h) <- years


# -- 1.2 number of edges
# all edges
list_num_edges <- lapply(names(list_adjmat_sampled), function(the_year) {
  adjmat_bin <- (list_adjmat_sampled[[the_year]] > 0) * 1
  num_edges <- sum(adjmat_bin) / 2
})
print(unlist(list_num_edges))

# edges in same hospitals
list_num_edges <- lapply(names(list_adjmat_inst_s), function(the_year) {
  adjmat_bin <- (list_adjmat_inst_s[[the_year]] > 0) * 1
  num_edges <- sum(adjmat_bin) / 2
})
print(unlist(list_num_edges))

# edges in hospitals with other tier
list_num_edges <- lapply(names(list_adjmat_tier_h), function(the_year) {
  adjmat_bin <- (list_adjmat_tier_h[[the_year]] > 0) * 1
  num_edges <- sum(adjmat_bin)
})
print(unlist(list_num_edges))


# -- 1.3 distribution of number of shared patients
list_num_shared_pat <- lapply(names(list_adjmat_sampled), function(the_year) {
  adjmat <- list_adjmat_sampled[[the_year]]
  w <- adjmat[upper.tri(adjmat)]
  num_shared_pat <- c(sum(w >= 1  & w < 5),
                      sum(w >= 5  & w < 10),
                      sum(w >= 10 & w < 20),
                      sum(w >= 20))
})
names(list_num_shared_pat) <- years
print(list_num_shared_pat)



# -- 2. Description of network with the threshold
# ---- 2.1 filter adjmat by hosital
# ------ 2.1.1 whether in same hospital
list_adjmat_inst_s <- lapply(names(list_adjmat_cutoff), function(the_year) {
  f_filter_by_same_hospital(list_adjmat_cutoff[[the_year]],
                            setNames(list_hos_info[[the_year]]$inst_code,
                                     list_hos_info[[the_year]]$phy_code)) # set name for the vector
})
names(list_adjmat_inst_s) <- years

# ----- 2.1.2 by hosital tier
# higher tier hospital
list_adjmat_tier_h <- lapply(names(list_adjmat_cutoff), function(the_year) {
  f_filter_by_higher_tier(list_adjmat_cutoff[[the_year]],
                          setNames(list_hos_info[[the_year]]$inst_tier,
                                   list_hos_info[[the_year]]$phy_code))
})
names(list_adjmat_tier_h) <- years


# -- 2.2 number of edges
# all edges
list_num_edges <- lapply(names(list_adjmat_cutoff), function(the_year) {
  adjmat_bin <- (list_adjmat_cutoff[[the_year]] > 0) * 1
  num_edges <- sum(adjmat_bin) / 2
})
print(unlist(list_num_edges))

# edges in same hospitals
list_num_edges <- lapply(names(list_adjmat_inst_s), function(the_year) {
  adjmat_bin <- (list_adjmat_inst_s[[the_year]] > 0) * 1
  num_edges <- sum(adjmat_bin) / 2
})
print(unlist(list_num_edges))

# edges in hospitals with other tier
list_num_edges <- lapply(names(list_adjmat_tier_h), function(the_year) {
  adjmat_bin <- (list_adjmat_tier_h[[the_year]] > 0) * 1
  num_edges <- sum(adjmat_bin)
})
print(unlist(list_num_edges))


# -- 2.3 other characteristics
list_netw_char <- lapply(names(list_phy_netw), function(the_year) {

  netw <- list_phy_netw[[the_year]]
  
  df_netw_char <- data.frame(
    year = as.numeric(the_year),
    num_node = vcount(netw),
    netw_diameter = diameter(netw, directed = FALSE, unconnected = TRUE),
    netw_mean_distance = mean_distance(netw, directed = FALSE),
    netw_density = edge_density(netw, loops = FALSE),
    netw_cluster_coeff = transitivity(netw, type = "average")
  )
})
df_netw_char <- do.call(rbind, list_netw_char)
View(df_netw_char)


