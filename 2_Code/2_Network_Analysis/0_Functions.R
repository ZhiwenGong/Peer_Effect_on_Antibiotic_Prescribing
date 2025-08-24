# R code
# Functions for data clean and analysis


# Function: build phy-phy adjacency matrix from phy-pat edgelist for one year
f_build_phy_phy_adjmat <- function(edgelist) {
  # build bipartite graph
  phy_pat_adjmat <- table(edgelist$phy_code, edgelist$pat_code)
  
  # project to phy-phy adjacency matrix
  phy_phy_adjmat <- phy_pat_adjmat %*% t(phy_pat_adjmat)
  gc()
  
  # remove self-connection
  diag(phy_phy_adjmat) <- 0
  
  return(phy_phy_adjmat)
}



# Function: sampling physicians from adjacency matrix
f_sampling_adjmat <- function(adjmat, df_phy_sampled, year) {
  # select the physician samples
  year_c <- as.character(df_phy_sampled$year)
  keep <- df_phy_sampled$phy_code[year_c == as.character(year)]
  
  # intersect - join on physician code
  keep <- intersect(rownames(adjmat), unique(keep))
  
  # drop other physicians
  return(adjmat[keep, keep, drop = FALSE])
}



# Function: apply the threshold for adjacency matrix
f_apply_threshold <- function(adjmat, mode = c("abs", "rel"), threshold) {
  mode <- match.arg(mode)
  if (mode == "abs") {
    adjmat[adjmat < threshold] <- 0
  } 
  else {
    for (i in seq_len(nrow(adjmat))) {
      # find all connections
      row <- adjmat[i, ]
      pos_idx <- which(row > 0)
      
      # skip null
      if (length(pos_idx) == 0) next
      
      # find closest connections
      k <- max(1L, ceiling(threshold * length(pos_idx)))
      cutoff <- sort(row[pos_idx], decreasing = TRUE)[k]
      
      # drop less closer connections
      adjmat[i, pos_idx[row[pos_idx] < cutoff]] <- 0
    }
  }
  return(adjmat)
}



# Function: row-normalizing for adjacency matrix
f_row_normalize <- function(adjmat) {
  
  row_sum <- rowSums(adjmat)
  row_not_zero <- row_sum > 0
  
  adjmat[row_not_zero, ] <- sweep(adjmat[row_not_zero, , drop = FALSE], 1, row_sum[row_not_zero], "/")
  
  return(adjmat)
}



# Function: calculate centrality for each physician in the given network
f_calc_centrality <- function(netw, year) {
  # get weight
  wght <- E(netw)$weight
  
  # physicians' name
  phy_codes <- V(netw)$name
  
  # degree centrality
  c_deg <- degree(netw, mode = "all", loops = FALSE)
  # closeness centrality
  c_clo <- closeness(netw, mode = "all", weights = wght, normalized = FALSE)
  c_clo[!is.finite(c_clo)] <- 0
  # betweenness centrality 
  c_bet <- betweenness(netw, directed = FALSE, weights = wght, normalized = FALSE)
  
  df_covar_centrality <- data.frame(
    phy_code = phy_codes,
    year = as.numeric(year),
    c_deg = c_deg,
    c_clo = c_clo,
    c_bet = c_bet,
    row.names = NULL
  )
  return(df_covar_centrality)
}



# Function: filter adjacency matrix by higher centrality
f_filter_by_high_centrality <- function(adjmat, vec_centrality) {
  
  phy_codes <- rownames(adjmat)
  
  # sort centrality
  vec_centrality <- vec_centrality[phy_codes] 
  
  mask <- outer(vec_centrality, vec_centrality, `<=`)
  adjmat_filtered <- adjmat * mask
  
  adjmat_filtered[is.na(adjmat_filtered)] <- 0
  
  return(adjmat_filtered)
}



# Function: filter adjacency matrix by same hospital
f_filter_by_same_hospital <- function(adjmat, vec_hos) {
  
  phy_codes <- rownames(adjmat)
  
  # sort centrality
  vec_hos <- vec_hos[phy_codes] 
  
  mask <- outer(vec_hos, vec_hos, `==`)
  adjmat_filtered <- adjmat * mask
  
  adjmat_filtered[is.na(adjmat_filtered)] <- 0
  
  return(adjmat_filtered)
}



# Function: filter adjacency matrix by higher hospital tier
f_filter_by_higher_tier <- function(adjmat, vec_tier) {
  
  phy_codes <- rownames(adjmat)
  
  # sort centrality
  vec_tier <- vec_tier[phy_codes] 
  
  mask <- outer(vec_tier, vec_tier, `<`)
  adjmat_filtered <- adjmat * mask
  
  adjmat_filtered[is.na(adjmat_filtered)] <- 0
  
  return(adjmat_filtered)
}



# Function: filter adjacency matrix by lower hospital tier
f_filter_by_lower_tier <- function(adjmat, vec_tier) {
  
  phy_codes <- rownames(adjmat)
  
  # sort centrality
  vec_tier <- vec_tier[phy_codes] 
  
  mask <- outer(vec_tier, vec_tier, `>`)
  adjmat_filtered <- adjmat * mask
  
  adjmat_filtered[is.na(adjmat_filtered)] <- 0
  
  return(adjmat_filtered)
}



# --- Function: calculate peer rate and assemble rates
f_get_peer_rate <- function(adjmat, rate, year) {
  
  # trans df to vector
  rate_vec <- setNames(rate$rate_appropriate, rate$phy_code)
  # align
  rate_vec_align <- rate_vec[colnames(adjmat)]  
  
  # multiply
  rate_peer <- as.numeric(adjmat %*% rate_vec_align)
  
  # assemble
  df_ap_rate <- data.frame(
    phy_code = rownames(adjmat),
    year = as.numeric(year),
    rate_individual = rate_vec_align,
    rate_peer = rate_peer,
    row.names = NULL
  )
  
  return(df_ap_rate)
}



# --- Function: calculate peer rate and assemble rates - allow multi-adjmat
f_get_peer_rate_multi_mat <- function(list_adjmat, rate, year, peer_names) {
  
  # trans df to vector
  rate_vec <- setNames(rate$rate_appropriate, rate$phy_code)
  # align
  rate_vec_align <- rate_vec[colnames(list_adjmat[[1]])]
  
  list_rate_peer <- lapply(list_adjmat, function(adjmat) {
    # multiply
    rate_peer <- as.numeric(adjmat %*% rate_vec_align)
  })
  names(list_rate_peer) <- peer_names
  
  # assemble
  df_ap_rate <- data.frame(
    phy_code = rownames(list_adjmat[[1]]),
    year = as.numeric(year),
    rate_individual = rate_vec_align,
    row.names = NULL
  )
  
  df_ap_rate <- bind_cols(df_ap_rate, as.data.frame(list_rate_peer))
  
  return(df_ap_rate)
}



# Function: tidy results of regression
f_tidy_lme_result <- function(model) {
  
  results <- summary(model)$tTable %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    rename(
      Coeff = Value,
      SE = Std.Error,
      p_value   = `p-value`
    )
  conf_level = 0.95
  crit  <- qt(1 - (1-conf_level)/2, df = results$DF)
  
  results <- results %>%
    select(term, Coeff, SE, p_value) %>%
    mutate(
      CI_low  = Coeff - crit * SE,
      CI_high = Coeff + crit * SE
    ) %>%
    mutate(
      Coeff = round(Coeff, 2),
      SE = round(SE, 2),
      p_value = round(p_value, 3),
      CI_low = round(CI_low, 2),
      CI_high = round(CI_high, 2)
    )
  
  return(results)
}

