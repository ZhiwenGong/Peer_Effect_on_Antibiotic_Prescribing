# R code
# Build physicians patient-sharing network from phy-pat edgelist


# -- 0. package & path
library(igraph)
library(dplyr)

dir_work <- "*"
dir_data_raw <- file.path(dir_work, "1_Data/1_Data_Raw")
dir_data_clean <- file.path(dir_work, "1_Data/2_Data_Clean")
dir_code <- file.path(dir_work, "2_Code/2_Network_Analysis")

# import customized functions
source(file.path(dir_code, "0_Functions.R"))



# -- 1. raw physicians adjacency matrix building
# ---- 1.1 data import
df_edgelist <- read.csv(file.path(dir_data_raw, "edgelist_phy_pat.csv"))

df_edgelist <- df_edgelist %>%
  select(phy_code, pat_code, year)


# ---- 1.2 split data by year
years <- sort(unique(df_edgelist$year))
list_edgelist <- split(df_edgelist, df_edgelist$year)
rm(df_edgelist)


# ---- 1.3 build adjacency matrices for all years
list_adjmat_raw <- lapply(list_edgelist, f_build_phy_phy_adjmat)
names(list_adjmat_raw) <- as.character(years)
rm(list_edgelist)



# -- 2. physicians sampling
df_ap <- read.csv(file.path(dir_data_raw, "antibiotic_prescription_sum.csv"))

# ---- 2.1 filter physicians prescribed over 100 antibiotic prescriptions in a year
df_phy_sampled <- df_ap %>%
  select(phy_code, sum_ap_year, year) %>%
  filter(sum_ap_year >= 100)


# ---- 2.2 filter adjacency matrix by sampled physician code
list_adjmat_sampled <- lapply(names(list_adjmat_raw), function(the_year) {
  f_sampling_adjmat(list_adjmat_raw[[the_year]], df_phy_sampled, the_year)
})
names(list_adjmat_sampled) <- years



# -- 3. apply phy-phy connection threshold
# ---- 3.1 set threshold
# absolute threshold for number of shared patients
threshold_abs <- 10
# relative threshold for the closest physician connections
# threshold_rel <- 0.1

# ---- 3.2 apply threshold
list_adjmat_cutoff <- lapply(list_adjmat_sampled, f_apply_threshold, mode = "abs", threshold_abs)
# list_adjmat_cutoff <- lapply(list_adjmat_sampled, f_apply_threshold, mode = "rel", threshold_rel)



# -- 4. normalize phy-phy adjacency matrix (for calculating peer rate)
list_adjmat_normalized <- lapply(list_adjmat_cutoff, f_row_normalize)



# -- 5. undirection, weighted network building
# use the adjacency matrix without normalizing
list_phy_netw <- lapply(list_adjmat_cutoff, f_network_build)



# -- 6. save network data
# clean other data
keep_obj <- c("list_adjmat_sampled",
              "list_adjmat_cutoff",
              "list_adjmat_normalized",
              "list_phy_netw",
              "threshold_abs",
              "years",
              "dir_data_clean")
rm(list = setdiff(ls(), keep_obj))
gc()

save.image(file = file.path(dir_data_clean, "network_data.RData"))


