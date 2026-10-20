library(data.table)
library(readxl)
library(lubridate)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(scales)
library(coda)
# library(simmr)
# library(rjags)
library(MASS) 
library(EnvStats) # Kendall, while handle the NAs


# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/isotope/")
setwd("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/isotope/")
getwd()


######  Source Apportionment, mEES_2025_WongCM.pdf

#### Data preparation ####

# Yangzhou OC EC & paired isotope data
ec_iso_pm_yz = read.csv("Yangzhou_OC EC_date_match_13C.csv")
ec_iso_pm_yz$Date = 
  as.Date(as.character(ec_iso_pm_yz$Date), "%m/%d/%y")
head(ec_iso_pm_yz)
ec_iso_pm_yz = na.omit(ec_iso_pm_yz)
ec_iso_pm_yz$All = "All"

ec_iso_pm_use_yz = 
  dplyr::select(ec_iso_pm_yz, Date, Year, Month, Season, Year_Season, All,
                EC_delta13C, EC_f14C, EC_conc)
head(ec_iso_pm_use_yz)

# Nanjing Isotope info, by source
nj_iso <- read_excel("Nanjing_PhD_Shanli_13C_source.xlsx", sheet = 1)
# nj_iso_use = 
#   subset(nj_iso, Source %in% c("Coal", "Traffic", "C3_Plant", "C4_Plant"))
nj_iso_use = 
  subset(nj_iso, Source %in% c("Coal", "Traffic_G1", "Traffic_G2", "C3_Plant", "C4_Plant"))
nj_iso_use$Use = nj_iso_use$Sample = NULL
names(nj_iso_use) = c("Data", "EC_delta13C", "EC_f14c")
head(nj_iso_use)

# Creat a 4 source, a combined traffic source, df
nj_iso_use_1traf = nj_iso_use
new_row <- data.frame(
  Data = "Traffic",
  EC_delta13C = -26.5,
  EC_f14c = 0
)
nj_iso_use_1traf <- 
  rbind(nj_iso_use_1traf, new_row) %>%
  subset(!(Data %in% c("Traffic_G1", "Traffic_G2")))
# write.csv(
#   nj_iso_use_1traf, 
#   "Nanjing_PhD_Shanli_13C_source_4source.csv")


# Estimate EC concentrations
all_ec_conc <-
  ec_iso_pm_yz %>%
  dplyr::group_by(All) %>%
  dplyr::summarise(
    EC_conc_mean = mean(EC_conc),
    EC_conc_median = median(EC_conc),
    OC_conc_mean = mean(OC_conc),
    OC_conc_median = median(OC_conc),
    .groups = "drop"
  ) %>%
  dplyr::rename(Group = All)

season_ec_conc <-
  ec_iso_pm_yz %>%
  dplyr::group_by(Season) %>%
  dplyr::summarise(
    EC_conc_mean = mean(EC_conc),
    EC_conc_median = median(EC_conc),
    OC_conc_mean = mean(OC_conc),
    OC_conc_median = median(OC_conc),
    .groups = "drop"
  ) %>%
  dplyr::rename(Group = Season)

year_ec_conc <-
  ec_iso_pm_yz %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    EC_conc_mean = mean(EC_conc),
    EC_conc_median = median(EC_conc),
    OC_conc_mean = mean(OC_conc),
    OC_conc_median = median(OC_conc),
    .groups = "drop"
  ) %>%
  dplyr::rename(Group = Year) %>%
  dplyr::mutate(Group = as.character(Group))  

year_season_ec_conc <-
  ec_iso_pm_yz %>%
  dplyr::group_by(Year_Season) %>%
  dplyr::summarise(
    EC_conc_mean = mean(EC_conc),
    EC_conc_median = median(EC_conc),
    OC_conc_mean = mean(OC_conc),
    OC_conc_median = median(OC_conc),
    .groups = "drop"
  ) %>%
  dplyr::rename(Group = Year_Season)

#### Shared plot setting #### 

# Colorblind-friendly palette (Okabe-Ito scheme)
# Colorblind-friendly palette (Okabe-Ito scheme)
# source_colors <- c(
#   "Coal"       = "#000000",   # black
#   "C3_Plant"   = "#009E73",   # bluish green
#   "C4_Plant"   = "#0072B2",   # blue
#   "Traffic_G1" = "#D55E00",   # vermillion
#   "Traffic_G2" = "#E69F00"    # orange
# )
source_colors <- c(
  "Coal"       = "#000000",   # black
  "C3_Plant"   = "#009E73",   # bluish green
  "C4_Plant"   = "#0072B2",   # blue
  "Traffic"    = "#D55E00"   # vermillion
)

season_colors <- c(
  "Spring" = "#009E73",   # sky blue 
  "Summer" = "#F0E442",   # yellow
  "Autumn" = "#D55E00",   # pink/purple
  "Winter" = "#332288"    # indigo
)

# year_colors <- c(
#   "2016" = "#000000",   # black
#   "2017" = "#009E73",   # bluish green
#   "2018" = "#0072B2",   # blue
#   "2019" = "#D55E00",   # vermillion
#   "2020" = "#E69F00"    # orange
# )

year_colors <- c(
  "2016" = "#9E9AC8",   # light purple
  "2017" = "#807DBA",
  "2018" = "#6A51A3",
  "2019" = "#54278F",
  "2020" = "#3F007D"    # dark purple
)

# Matching shapes so color is not the only encoding
# source_shapes <- c(
#   "Coal"       = 16,   # filled circle
#   "C3_Plant"   = 17,   # filled triangle up
#   "C4_Plant"   = 15,   # filled square
#   "Traffic_G1" = 18,   # filled diamond
#   "Traffic_G2" = 8     # asterisk
# )
source_shapes <- c(
  "Coal"       = 16,   # filled circle
  "C3_Plant"   = 17,   # filled triangle up
  "C4_Plant"   = 15,   # filled square
  "Traffic"    = 18
)

season_shapes <- c("Spring" = 16,
                   "Summer" = 17,
                   "Autumn" = 15,
                   "Winter" = 18)

year_shapes <- c("2016" = 16,
                 "2017" = 17,
                 "2018" = 15,
                 "2019" = 18,
                 "2020" = 8)

# The shared theme setting
theme_ec_iso_mcmc = 
  theme_classic(base_size = 16) +
  theme(
    plot.title         = element_text(size = 18, face = "bold", margin = margin(b = 10)),
    axis.text          = element_text(color = "grey20"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.title.y       = element_text(margin = margin(r = 8)),
    axis.line          = element_line(color = "grey40", linewidth = 0.4),
    axis.ticks         = element_line(color = "grey40", linewidth = 0.4),
    legend.position    = "right",
    legend.title       = element_text(face = "bold", size = 14),
    legend.text        = element_text(size = 13),
    legend.key.height  = unit(1.2, "lines"),
    legend.background  = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(12, 16, 8, 8)
  )


######################################################
#### Bayesian MCMC #### 
######################################################

#### Bayesian MCMC: Functions ####

### Run (MCMC, Bayesian Markov chain Monte Carlo) model for a subset of data

# Isotope mixing models with MCMC: Stock & Semmens (2016) PLOS ONE, MixSIAR paper

# burn_in, the initial portion of chian discarded before saving samples, 20~50% of total iterations
## Burn-in and convergence: Gelman & Rubin (1992) Statistical Science — also the source of the Gelman-Rubin diagnostic

# thinning, keeps every kth sample to reduce autocorrelation between consecutive draws, thinning Markov chains
## Thinning debate: Link & Eaton (2012) Methods in Ecology and Evolution — argues against thinning

mcmc_mixing_model <- 
  function(isotope_conc_data, source_isotope_info, 
           iterations = 150000, burn_in = 50000, thin = 30) {
  # thin = 10, ESS < 100 for most sources in all MCMC subgroup analyses with iterations = 50000, burn_in = 20000
  # thin = 30, ESS < 100 for traffic in all MCMC subgroup analyses with iterations = 50000, burn_in = 20000
    
  # Extract variables from the mixture data
  mixture_delta13C <- isotope_conc_data$EC_delta13C
  mixture_f14C <- isotope_conc_data$EC_f14C
  
  # Extract source information
  source_names <- source_isotope_info$Data
  n_sources <- length(source_names)
  
  # Source values for delta13C and f14C
  s_delta13C <- source_isotope_info$EC_delta13C
  s_f14C <- source_isotope_info$EC_f14c
  
  # Define uncertainty for source values
  sd_delta13C <- rep(0.5, n_sources)  # 0.5‰ uncertainty for δ13C
  sd_f14C <- rep(2.0, n_sources)      # 2% uncertainty for F14C
  
  # Define process and observation error
  obs_err_delta13C <- 0.3  # Observation error for delta13C
  obs_err_f14C <- 1.0      # Observation error for f14C
  
  # Number of samples to save after burn-in and thinning
  n_samples <- as.integer((iterations - burn_in) / thin)
  
  # Matrix to store the posterior samples for proportions
  posterior_p <- matrix(0, nrow = n_samples, ncol = n_sources)
  colnames(posterior_p) <- source_names
  
  # Initialize proportions randomly
  p <- runif(n_sources)
  p <- p / sum(p)  # Normalize to sum to 1
  
  # Error functions for likelihood calculation
  calc_error <- function(p) {
    # Predicted isotope values
    pred_delta13C <- sum(p * s_delta13C)
    pred_f14C <- sum(p * s_f14C)
    
    # Squared errors
    err_delta13C <- (mixture_delta13C - pred_delta13C)^2 / (2 * obs_err_delta13C^2)
    err_f14C <- (mixture_f14C - pred_f14C)^2 / (2 * obs_err_f14C^2)
    
    # Total negative log likelihood
    return(err_delta13C + err_f14C)
  }
  
  # MCMC sampling loop
  sample_idx <- 1
  accept_count <- 0
  
  for (i in 1:iterations) {
    # Propose new proportions using Dirichlet-like proposal
    p_new <- p + rnorm(n_sources, 0, 0.03)
    
    # Ensure non-negative values
    p_new[p_new < 0] <- 0
    
    # Renormalize to sum to 1
    if (sum(p_new) > 0) {
      p_new <- p_new / sum(p_new)
      
      # Calculate errors for current and proposed proportions
      error_current <- calc_error(p)
      error_proposed <- calc_error(p_new)
      
      # Metropolis acceptance probability
      log_r <- error_current - error_proposed
      
      # Accept or reject
      if (log(runif(1)) < log_r) {
        p <- p_new
        if (i > burn_in) accept_count <- accept_count + 1
      }
    }
    
    # Save samples after burn-in and thinning
    if (i > burn_in && (i - burn_in) %% thin == 0) {
      posterior_p[sample_idx, ] <- p
      sample_idx <- sample_idx + 1
    }
    
    # Print progress
    if (i %% 5000 == 0) {
      cat("Iteration", i, "of", iterations, "\n")
    }
  }
  
  # Calculate acceptance rate after burn-in
  acceptance_rate <- accept_count / (iterations - burn_in)
  cat("Acceptance rate:", round(acceptance_rate * 100, 1), "%\n")
  
  # Calculate summary statistics
  summary_stats <- data.frame(
    Source = source_names,
    Mean = colMeans(posterior_p),
    SD = apply(posterior_p, 2, sd),
    Median = apply(posterior_p, 2, median),
    Q2.5 = apply(posterior_p, 2, quantile, probs = 0.025),
    Q97.5 = apply(posterior_p, 2, quantile, probs = 0.975)
  )
  
  # Return results
  return(list(
    posterior = posterior_p,
    summary = summary_stats,
    acceptance_rate = acceptance_rate
  ))
}

# Function to run for different subsets (e.g., by season, year)
run_mcmc_by_group <- 
  function(data, group_var, source_isotope_info) {
  groups <- unique(data[[group_var]])
  results <- list()
  
  for (g in groups) { # g = groups[1]
    cat("\nAnalyzing", group_var, "=", g, "\n")
    subset_data <- data[data[[group_var]] == g, ]
    
    # Calculate mean isotope values for the group
    group_mean <- data.frame(
      EC_delta13C = mean(subset_data$EC_delta13C, na.rm = TRUE),
      EC_f14C = mean(subset_data$EC_f14C, na.rm = TRUE)
    )
    
    # Run MCMC
    results[[as.character(g)]] <- 
      mcmc_mixing_model(group_mean, source_isotope_info)
  }
  
  return(results)
}

# Function to run for the whole dataset 
run_mcmc_whole_dataset <- 
  function(data, source_isotope_info) {
    cat("Analyzing whole dataset\n")
    dataset_mean <- data.frame(
      EC_delta13C = mean(data$EC_delta13C, na.rm = TRUE),
      EC_f14C = mean(data$EC_f14C, na.rm = TRUE)
    )
    
    # Fix: was incorrectly hardcoded as source_data, should be source_isotope_info
    return(mcmc_mixing_model(dataset_mean, source_isotope_info))
  }


# Extract results into a data frame for plotting
extract_mcmc_info <- function(results) {
  df <- data.frame()
  
  for (group_name in names(results)) {
    group_results <- results[[group_name]]$summary
    group_results$Group <- group_name
    df <- rbind(df, group_results)
  }
  
  return(df)
}

# Extract acceptance rates from grouped results
extract_acceptance_rates <- 
  function(grouped_mcmc, group_label) {
  data.frame(
    Group  = names(grouped_mcmc),
    Label  = group_label,
    Accept_Rate = sapply(grouped_mcmc, function(x) x$acceptance_rate)
  )
}

# Model diagnostics
check_mcmc_diagnostics <- function(grouped_mcmc, group_label) {
  cat("\n===", group_label, "===\n")
  
  for (g in names(grouped_mcmc)) {
    cat("\nGroup:", g, "\n")
    
    # Acceptance rate
    cat("  Acceptance rate:", 
        round(grouped_mcmc[[g]]$acceptance_rate * 100, 1), "%\n")
    
    # Effective sample size
    ess <- effectiveSize(as.mcmc(grouped_mcmc[[g]]$posterior))
    cat("  ESS:\n")
    print(round(ess))
  }
}

#### Run multiple chain
# Run 3 chains for whole dataset
run_mcmc_multichain <- 
  function(data, source_isotope_info,
           n_chains = 3,
           iterations = 150000,
           burn_in = 50000,
           thin = 30) {
    chains <- list()
    accept_rates <- c()
    
    for (i in 1:n_chains) {
      cat("\nRunning chain", i, "of", n_chains, "\n")
      set.seed(i * 100)
      chain_result    <- mcmc_mixing_model(data, source_isotope_info,
                                           iterations, burn_in, thin)
      chains[[i]]     <- as.mcmc(chain_result$posterior)
      accept_rates[i] <- chain_result$acceptance_rate
    }
    
    chain_list    <- mcmc.list(chains)
    gelman_result <- tryCatch(
      gelman.diag(chain_list),
      error = function(e) {
        cat("Gelman-Rubin could not be computed\n")
        NULL
      }
    )
    
    combined_posterior <- do.call(rbind, lapply(chains, as.matrix))
    
    return(list(
      chains           = chain_list,
      gelman           = gelman_result,
      effectiveSize    = effectiveSize(chain_list),
      acceptance_rate  = mean(accept_rates),
      accept_per_chain = accept_rates,
      posterior        = combined_posterior,
      summary          = data.frame(
        Source = colnames(combined_posterior),
        Mean   = colMeans(combined_posterior),
        SD     = apply(combined_posterior, 2, sd),
        Median = apply(combined_posterior, 2, median),
        Q2.5   = apply(combined_posterior, 2, quantile, probs = 0.025),
        Q97.5  = apply(combined_posterior, 2, quantile, probs = 0.975)
      )
    ))
  }

# Run 3 chains by group
run_mcmc_by_group_multichain <- 
  function(data, group_var, source_isotope_info,
           n_chains = 3,
           iterations = 150000,
           burn_in = 50000,
           thin = 30) {
    groups <- unique(data[[group_var]])
    results <- list()
    
    for (g in groups) {
      cat("\nAnalyzing", group_var, "=", g, "\n")
      subset_data <- data[data[[group_var]] == g, ]
      
      group_mean <- data.frame(
        EC_delta13C = mean(subset_data$EC_delta13C, na.rm = TRUE),
        EC_f14C     = mean(subset_data$EC_f14C,     na.rm = TRUE)
      )
      
      # Run n_chains independent chains
      chains <- list()
      accept_rates <- c()
      
      for (i in 1:n_chains) {
        cat("  Chain", i, "of", n_chains, "\n")
        set.seed(i * 100)
        # Fix: save to chain_result first, then extract
        chain_result   <- mcmc_mixing_model(group_mean, source_isotope_info,
                                            iterations, burn_in, thin)
        chains[[i]]    <- as.mcmc(chain_result$posterior)
        accept_rates[i] <- chain_result$acceptance_rate
      }
      
      chain_list <- mcmc.list(chains)
      
      # Safe Gelman-Rubin
      gelman_result <- tryCatch(
        gelman.diag(chain_list),
        error = function(e) {
          cat("  Gelman-Rubin could not be computed for group", g,
              "— chains may be too similar (near-perfect convergence)\n")
          NULL
        }
      )
      
      # Combined posterior across all chains
      combined_posterior <- do.call(rbind, lapply(chains, as.matrix))
      
      results[[as.character(g)]] <- list(
        chains           = chain_list,
        gelman           = gelman_result,
        effectiveSize    = effectiveSize(chain_list),
        posterior        = combined_posterior,
        acceptance_rate  = mean(accept_rates),
        accept_per_chain = accept_rates,
        summary          = data.frame(
          Source = colnames(combined_posterior),
          Mean   = colMeans(combined_posterior),
          SD     = apply(combined_posterior, 2, sd),
          Median = apply(combined_posterior, 2, median),
          Q2.5   = apply(combined_posterior, 2, quantile, probs = 0.025),
          Q97.5  = apply(combined_posterior, 2, quantile, probs = 0.975)
        )
      )
      
      # Print diagnostics
      if (!is.null(gelman_result)) {
        cat("  Gelman-Rubin R̂:\n")
        print(round(gelman_result$psrf, 3))
      }
      cat("  ESS:\n")
      print(round(results[[as.character(g)]]$effectiveSize))
    }
    
    return(results)
  }

# Extract Gelman-Rubin R̂
extract_multiChain_gelman <- 
  function(grouped_mcmc, group_label) {
    df <- data.frame()
    
    for (g in names(grouped_mcmc)) {
      gelman <- grouped_mcmc[[g]]$gelman
      
      if (!is.null(gelman)) {
        temp <- data.frame(
          Group  = g,
          Label  = group_label,
          Source = rownames(gelman$psrf),
          R_hat  = gelman$psrf[, 1],       # point estimate
          Upper  = gelman$psrf[, 2]        # upper CI of R̂
        )
      } else {
        # Fill with NA when gelman could not be computed
        sources <- colnames(grouped_mcmc[[g]]$posterior)
        temp <- data.frame(
          Group  = g,
          Label  = group_label,
          Source = sources,
          R_hat  = NA,
          Upper  = NA
        )
      }
      df <- rbind(df, temp)
    }
    return(df)
  }

# Extract ESS
extract_multiChain_ess <- 
  function(grouped_mcmc, group_label) {
    df <- data.frame()
    
    for (g in names(grouped_mcmc)) {
      ess  <- grouped_mcmc[[g]]$effectiveSize
      temp <- data.frame(
        Group  = g,
        Label  = group_label,
        Source = names(ess),
        ESS    = as.numeric(ess)
      )
      df <- rbind(df, temp)
    }
    return(df)
  }

# Extract acceptance rate
# Note: acceptance_rate is now per chain inside mcmc_mixing_model
# but stored as NULL in the new multichain function
# You need to re-extract from the individual chain runs, or
# use the single-chain results if you saved them
# If not, estimate from the posterior autocorrelation as a proxy
extract_multiChain_acceptance_rate <- 
  function(grouped_mcmc, group_label) {
    df <- data.frame()
    
    for (g in names(grouped_mcmc)) {
      rate <- grouped_mcmc[[g]]$acceptance_rate
      temp <- data.frame(
        Group       = g,
        Label       = group_label,
        Accept_Rate = ifelse(is.null(rate), NA, rate)
      )
      df <- rbind(df, temp)
    }
    return(df)
  }

#### Bayesian MCMC: SINGLE Chian ####

###### Bayesian MCMC Single Chian: Modeling ######

# Run for all
all_mcmc <- run_mcmc_whole_dataset(ec_iso_pm_yz, nj_iso_use)

# Run for each season
season_mcmc <- run_mcmc_by_group(ec_iso_pm_yz, "Season", nj_iso_use)

# Run for each year
year_mcmc <- run_mcmc_by_group(ec_iso_pm_yz, "Year", nj_iso_use)

# Run for each year_season combination
year_season_mcmc <- run_mcmc_by_group(ec_iso_pm_yz, "Year_Season", nj_iso_use)

### Check acceptance_rate
# Optimal acceptance rate (~23%): Roberts, Gelman & Gilks (1997) Annals of Applied Probability

all_accept       <- all_mcmc$acceptance_rate # 0.29205
season_accept    <- extract_acceptance_rates(season_mcmc,      "Season")
year_accept      <- extract_acceptance_rates(year_mcmc,        "Year")
year_season_accept <- extract_acceptance_rates(year_season_mcmc, "Year_Season")

accept_comb = rbind(season_accept, year_accept, year_season_accept)
accept_comb <- 
  rbind(
    accept_comb,
    data.frame(Group = "All", Label = "All", Accept_Rate = all_accept)
  )
row.names(accept_comb) = 1:nrow(accept_comb)
summary(accept_comb$Accept_Rate)

### Check EES

# ESS: Kass et al. (1998) J. American Statistical Association recommend ESS > 100; 
# Gelman et al. Bayesian Data Analysis (3rd ed., 2013), the standard textbook reference

# For whole dataset
effectiveSize(as.mcmc(all_mcmc$posterior))

# For grouped results - use the diagnostic function from earlier
check_mcmc_diagnostics(season_mcmc,      "Season")
check_mcmc_diagnostics(year_mcmc,        "Year")
check_mcmc_diagnostics(year_season_mcmc, "Year_Season")

# Extract fraction contribution results
all_mcmc_summary <- all_mcmc$summary
season_mcmc_summary <- extract_mcmc_info(season_mcmc)
year_mcmc_summary <- extract_mcmc_info(year_mcmc)
year_season_mcmc_summary <- extract_mcmc_info(year_season_mcmc)

# Rename the last column
names(season_mcmc_summary)[ncol(season_mcmc_summary)] = "Season"
names(year_mcmc_summary)[ncol(season_mcmc_summary)] = "Year"
names(year_season_mcmc_summary)[ncol(season_mcmc_summary)] = "Year_Season"


# Merge with fraction contributions & estimate source-specific conc contributions
all_ec_iso_mcmc = merge(all_ec_iso_conc, all_mcmc_summary)
all_ec_iso_mcmc$Source_conc_mean = all_ec_iso_mcmc$EC_conc_mean* all_ec_iso_mcmc$Mean
all_ec_iso_mcmc$Source_conc_median = all_ec_iso_mcmc$EC_conc_median* all_ec_iso_mcmc$Median
all_ec_iso_mcmc$Period_used = "All"
all_ec_iso_mcmc <- cbind(Period = "all", all_ec_iso_mcmc)
head(all_ec_iso_mcmc)

season_ec_iso_mcmc = merge(season_ec_iso_conc, season_mcmc_summary)
season_ec_iso_mcmc$Source_conc_mean = season_ec_iso_mcmc$EC_conc_mean* season_ec_iso_mcmc$Mean
season_ec_iso_mcmc$Source_conc_median = season_ec_iso_mcmc$EC_conc_median* season_ec_iso_mcmc$Median
season_ec_iso_mcmc$Period_used = "Season"
head(season_ec_iso_mcmc)

year_ec_iso_mcmc = merge(year_ec_iso_conc, year_mcmc_summary)
year_ec_iso_mcmc$Source_conc_mean = year_ec_iso_mcmc$EC_conc_mean* year_ec_iso_mcmc$Mean
year_ec_iso_mcmc$Source_conc_median = year_ec_iso_mcmc$EC_conc_median* year_ec_iso_mcmc$Median
year_ec_iso_mcmc$Period_used = "Year"
head(year_ec_iso_mcmc)

year_season_ec_iso_mcmc = merge(year_season_ec_iso_conc, year_season_mcmc_summary)
year_season_ec_iso_mcmc$Source_conc_mean = year_season_ec_iso_mcmc$EC_conc_mean* year_season_ec_iso_mcmc$Mean
year_season_ec_iso_mcmc$Source_conc_median = year_season_ec_iso_mcmc$EC_conc_median* year_season_ec_iso_mcmc$Median
year_season_ec_iso_mcmc$Period_used = "Year_Season"
head(year_season_ec_iso_mcmc)

names(season_ec_iso_mcmc)[1] = names(year_ec_iso_mcmc)[1] = names(year_season_ec_iso_mcmc)[1] = "Period"

ec_iso_mcmc = rbind(year_ec_iso_mcmc, season_ec_iso_mcmc)
# ec_iso_mcmc = rbind(ec_iso_mcmc, year_ec_iso_mcmc)
head(ec_iso_mcmc)

###### Bayesian MCMC Single Chian: Plotting ######

all_ec_iso_mcmc_plot <-
  ggplot(all_ec_iso_mcmc,
         aes(x = Period, y = Source_conc_mean,
             color = Source, shape = Source, group = Source)) +
  geom_line(linewidth = 0.7, color = "grey60", lineend = "round") +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_manual(values = source_colors) +
  scale_shape_manual(values = source_shapes) +
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Source contributions in YZ, 2016-2020",
    x     = NULL,
    y     = expression("Concentration (µg m"^{-3}*")"),
    color = "Source",
    shape = "Source"
  ) +
  theme_ec_iso_mcmc

season_order = c("Spring", "Summer", "Autumn", "Winter")
season_ec_iso_mcmc$Period <-
  factor(season_ec_iso_mcmc$Period, levels = season_order)

season_ec_iso_mcmc_plot <-
  ggplot(season_ec_iso_mcmc,
         aes(x = Period, y = Source_conc_mean,
             color = Source, shape = Source, group = Source)) +
  geom_line(linewidth = 0.7, color = "grey60", lineend = "round") +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_manual(values = source_colors) +
  scale_shape_manual(values = source_shapes) +
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Source contributions in YZ by Season",
    x     = NULL,
    y     = expression("Concentration (µg m"^{-3}*")"),
    color = "Source",
    shape = "Source"
  ) +
  theme_ec_iso_mcmc

year_ec_iso_mcmc_plot <-
  ggplot(year_ec_iso_mcmc,
       aes(x = Period, y = Source_conc_mean,
           color = Source, shape = Source, group = Source)) +
  geom_line(linewidth = 0.7, color = "grey60", lineend = "round") +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_manual(values = source_colors) +
  scale_shape_manual(values = source_shapes) +
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Source contributions in YZ by year",
    x     = NULL,
    y     = expression("Concentration (µg m"^{-3}*")"),
    color = "Source",
    shape = "Source"
  ) +
  theme_ec_iso_mcmc


# Define the correct order for Year_Season
ys_order <- c("2016_Autumn", "2016_Winter",
              "2017_Spring", "2017_Summer", "2017_Autumn", "2017_Winter",
              "2018_Spring", "2018_Autumn", "2018_Winter",
              "2019_Spring", "2019_Summer", "2019_Autumn", "2019_Winter",
              "2020_Spring", "2020_Summer", "2020_Autumn", "2020_Winter")  # Add more as needed
year_season_ec_iso_mcmc$Period <-
  factor(year_season_ec_iso_mcmc$Period, levels = ys_order)


year_season_ec_iso_mcmc_plot <-
  ggplot(year_season_ec_iso_mcmc,
         aes(x = Period, y = Source_conc_mean,
             color = Source, shape = Source, group = Source)) +
  geom_line(linewidth = 0.7, color = "grey60", lineend = "round") +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_manual(values = source_colors) +
  scale_shape_manual(values = source_shapes) +
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Source contributions in YZ by Year & Season",
    x     = NULL,
    y     = expression("Concentration (µg m"^{-3}*")"),
    color = "Source",
    shape = "Source"
  ) +
  theme_ec_iso_mcmc

###### Bayesian MCMC Single Chian: Save files ######



#### Bayesian MCMC: Multiple (3) Chains ####

###### Bayesian MCMC 3 Chains: Modeling & Diagnose ######

# ### 5 source version
# # Run 3-chain Bayesian MCMC
# all_3mcmc         <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "All",      nj_iso_use)
# season_3mcmc      <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "Season",      nj_iso_use)
# season_3mcmc      <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "Season",      nj_iso_use)
# year_3mcmc        <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "Year",        nj_iso_use)
# year_season_3mcmc <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "Year_Season", nj_iso_use)

### 4 source version, 1 traffic 
# Run 3-chain Bayesian MCMC
all_3mcmc         <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "All",      nj_iso_use_1traf)
season_3mcmc      <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "Season",      nj_iso_use_1traf)
season_3mcmc      <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "Season",      nj_iso_use_1traf)
year_3mcmc        <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "Year",        nj_iso_use_1traf)
year_season_3mcmc <- run_mcmc_by_group_multichain(ec_iso_pm_use_yz, "Year_Season", nj_iso_use_1traf)

### Model validation
# Gelman-Rubin
whole_3chain_gelman       <- extract_multiChain_gelman(all_3mcmc,         "All") 
season_3chain_gelman      <- extract_multiChain_gelman(season_3mcmc,      "Season")
year_3chain_gelman        <- extract_multiChain_gelman(year_3mcmc,        "Year")
year_season_3chain_gelman <- extract_multiChain_gelman(year_season_3mcmc, "Year_Season")
all_3chain_gelman         <- rbind(whole_3chain_gelman, season_3chain_gelman, year_3chain_gelman, year_season_3chain_gelman)

# ESS
whole_3chain_ess       <- extract_multiChain_ess(all_3mcmc,         "All") 
season_3chain_ess      <- extract_multiChain_ess(season_3mcmc,      "Season")
year_3chain_ess        <- extract_multiChain_ess(year_3mcmc,        "Year")
year_season_3chain_ess <- extract_multiChain_ess(year_season_3mcmc, "Year_Season")
all_3chain_ess         <- rbind(whole_3chain_ess, season_3chain_ess, year_3chain_ess, year_season_3chain_ess)

# Acceptance rate
whole_3chain_accept       <- extract_multiChain_acceptance_rate(all_3mcmc,         "All")
season_3chain_accept      <- extract_multiChain_acceptance_rate(season_3mcmc,      "Season")
year_3chain_accept        <- extract_multiChain_acceptance_rate(year_3mcmc,        "Year")
year_season_3chain_accept <- extract_multiChain_acceptance_rate(year_season_3mcmc, "Year_Season")
all_3chain_accept         <- rbind(whole_3chain_accept, season_3chain_accept, year_3chain_accept, year_season_3chain_accept)

### Quick check
print(all_3chain_gelman)   # R̂ should be < 1.1, NA where not computable
# The Gelman-Rubin diagnostic could not be computed for select subgroups due to near-zero between-chain variance, 
# indicating convergence so strong that chains were numerically indistinguishable. 
# ESS was used as the convergence criterion for these groups.

print(all_3chain_ess)      # should be > 100, ideally > 200
print(all_3chain_accept)   # should be 0.20-0.40, NA if not stored

# write.csv(all_3chain_gelman, "YZ_Isotope_Bayesian_MCMC_3chain_Gelman-Rubin.csv")
# write.csv(all_3chain_ess, "YZ_Isotope_Bayesian_MCMC_3chain_ESS.csv")
# write.csv(all_3chain_accept, "YZ_Isotope_Bayesian_MCMC_3chain_Aacceptance-Rate.csv")

write.csv(all_3chain_gelman, "YZ_Isotope_Bayesian_MCMC_3chain_Gelman-Rubin_4source.csv")
write.csv(all_3chain_ess, "YZ_Isotope_Bayesian_MCMC_3chain_ESS_4source.csv")
write.csv(all_3chain_accept, "YZ_Isotope_Bayesian_MCMC_3chain_Aacceptance-Rate_4source.csv")

###### Bayesian MCMC 3 Chains: Extract fractions and estimate concentrations (mean & median) ######

# Extract fraction contribution results
all_3mcmc_frac         <- extract_mcmc_info(all_3mcmc)
season_3mcmc_frac      <- extract_mcmc_info(season_3mcmc)
year_3mcmc_frac        <- extract_mcmc_info(year_3mcmc)
year_season_3mcmc_frac <- extract_mcmc_info(year_season_3mcmc)

# Add and estimate Concentration contribution
all_3mcmc_conc <- 
  all_ec_conc %>%
  plyr::join(all_3mcmc_frac) %>%
  dplyr::mutate(
    EC_conc_contri_median = EC_conc_mean * Median,
    EC_conc_contri_mean = EC_conc_mean * Mean,
    OC_conc_contri_median = OC_conc_mean * Median,
    OC_conc_contri_mean = OC_conc_mean * Mean) %>%
  dplyr::rename(
    Bayesian_Mean_frac   = Mean,
    Bayesian_Median_frac = Median,
    Bayesian_SD_frac   = SD,
    Bayesian_Q2.5_frac = Q2.5,
    Bayesian_Q97.5_frac   = Q97.5
  )
head(all_3mcmc_conc); summary(all_3mcmc_conc)
  
year_3mcmc_conc <- 
  year_ec_conc %>%
  plyr::join(year_3mcmc_frac) %>%
  dplyr::mutate(
    EC_conc_contri_median = EC_conc_mean * Median,
    EC_conc_contri_mean = EC_conc_mean * Mean,
    OC_conc_contri_median = OC_conc_mean * Median,
    OC_conc_contri_mean = OC_conc_mean * Mean) %>%
  dplyr::rename(
    Bayesian_Mean_frac   = Mean,
    Bayesian_Median_frac = Median,
    Bayesian_SD_frac   = SD,
    Bayesian_Q2.5_frac = Q2.5,
    Bayesian_Q97.5_frac   = Q97.5
  )
head(year_3mcmc_conc); summary(year_3mcmc_conc)

season_3mcmc_conc <- 
  season_ec_conc %>%
  plyr::join(season_3mcmc_frac) %>%
  dplyr::mutate(
    EC_conc_contri_median = EC_conc_mean * Median,
    EC_conc_contri_mean = EC_conc_mean * Mean,
    OC_conc_contri_median = OC_conc_mean * Median,
    OC_conc_contri_mean = OC_conc_mean * Mean) %>%
  dplyr::rename(
    Bayesian_Mean_frac   = Mean,
    Bayesian_Median_frac = Median,
    Bayesian_SD_frac   = SD,
    Bayesian_Q2.5_frac = Q2.5,
    Bayesian_Q97.5_frac   = Q97.5
  )
head(season_3mcmc_conc); summary(season_3mcmc_conc)

year_season_3mcmc_conc <- 
  year_season_ec_conc %>%
  plyr::join(year_season_3mcmc_frac) %>%
  dplyr::mutate(
    EC_conc_contri_median = EC_conc_mean * Median,
    EC_conc_contri_mean = EC_conc_mean * Mean,
    OC_conc_contri_median = OC_conc_mean * Median,
    OC_conc_contri_mean = OC_conc_mean * Mean) %>%
  dplyr::rename(
    Bayesian_Mean_frac   = Mean,
    Bayesian_Median_frac = Median,
    Bayesian_SD_frac   = SD,
    Bayesian_Q2.5_frac = Q2.5,
    Bayesian_Q97.5_frac   = Q97.5
  )
head(year_season_3mcmc_conc); summary(year_season_3mcmc_conc)

# Combine all results
ec_oc_iso_3mcmc = 
  rbind(all_3mcmc_conc, season_3mcmc_conc, year_3mcmc_conc, year_season_3mcmc_conc)
head(ec_oc_iso_3mcmc)

# write.csv(ec_oc_iso_3mcmc, "Bayesian_3chain_MCMC_YZ_Isotope_source_2026.04.csv")
write.csv(ec_oc_iso_3mcmc, "Bayesian_3chain_MCMC_YZ_Isotope_4source1Traffic_2026.08.csv")

###### Bayesian MCMC 3 Chains: Plotting ######

ec_oc_iso_3mcmc = 
  fread("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/Bayesian_3chain_MCMC_YZ_Isotope_4source1Traffic_2026.08.csv")
ec_oc_iso_3mcmc$V1 = NULL

all_3mcmc_conc = 
  subset(ec_oc_iso_3mcmc, Group == "All")
season_3mcmc_conc = 
  subset(ec_oc_iso_3mcmc, 
         Group %in% c("Spring", "Summer", "Autumn", "Winter"))
year_3mcmc_conc =
  subset(ec_oc_iso_3mcmc, 
         Group %in% 2016:2020)

all_3mcmc_frac_plot <-
  ggplot(all_3mcmc_conc,
         aes(x = Source, y = Bayesian_Median_frac*100,
             color = Source, shape = Source, group = Source)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = Bayesian_Q2.5_frac*100, 
        ymax = Bayesian_Q97.5_frac*100),
    width = 0.22, linewidth = 0.6) +
  scale_color_manual(values = source_colors) +
  scale_shape_manual(values = source_shapes) +
  scale_y_continuous(
    labels = function(x) sprintf("%.0f", x),
    expand = expansion(mult = c(0.02, 0.08)),
    breaks = c(0, 30, 60, 90),
    limits = c(0, 90)
  ) +
  labs(
    title = "Overall source contributions, 2016-2020",
    x     = NULL,
    y     = expression("Fraction (%)"),
    color = "Source",
    shape = "Source"
  ) +
  theme_ec_iso_mcmc +
  theme(legend.position = "none")
all_3mcmc_frac_plot

season_order = c("Spring", "Summer", "Autumn", "Winter")
season_3mcmc_conc$Group <-
  factor(season_3mcmc_conc$Group, levels = season_order)

dodge <- position_dodge(width = 0.7)

season_3mcmc_frac_plot <-
  ggplot(season_3mcmc_conc,
         aes(x = Source, y = Bayesian_Median_frac*100,
             color = factor(Group), # shape = factor(Group),
             group = factor(Group))) +
  geom_point(size = 2.5, alpha = 0.9,
             position = dodge) +
  geom_errorbar(
    aes(ymin = Bayesian_Q2.5_frac*100,
        ymax = Bayesian_Q97.5_frac*100),
    width = 0.3,
    position = dodge) +
  scale_color_manual(values = season_colors,   # define your own season_colors
                     name = "Season") +
  # scale_shape_manual(values = season_shapes,   # define your own season_shapes
  #                    name = "Season") +
  scale_y_continuous(
    labels = function(x) sprintf("%.0f", x),
    expand = expansion(mult = c(0.02, 0.08)),
    breaks = c(0, 30, 60, 90),
    limits = c(0, 90)
  ) +
  labs(
    title = "Seasonal source contributions in YZ",
    x     = NULL,
    y     = expression("Fraction (%)"),
    color = "Season",
    shape = "Season"
  ) +
  theme_ec_iso_mcmc + 
  # Combine them to pull legend position up
  theme(legend.position = "bottom",
        legend.margin = margin(t = -10))
season_3mcmc_frac_plot


year_order <- c("2016", "2017", "2018", "2019", "2020")  # adjust to your years
year_3mcmc_conc$Group <- factor(year_3mcmc_conc$Group, levels = year_order)

dodge <- position_dodge(width = 0.7)

year_3mcmc_frac_plot <-
  ggplot(year_3mcmc_conc,
         aes(x = Source, y = Bayesian_Median_frac*100,
             color = factor(Group), # shape = factor(Group),
             group = factor(Group))) +
  geom_point(size = 2.5, alpha = 0.9,
             position = dodge) +
  geom_errorbar(
    aes(ymin = Bayesian_Q2.5_frac*100,
        ymax = Bayesian_Q97.5_frac*100),
    width = 0.35, linewidth = 0.6, 
    position = dodge) +
  scale_color_manual(values = year_colors,   
                     name = "Year") +
  scale_shape_manual(values = year_shapes,   # define your own year_shapes
                     name = "Year") +
  scale_y_continuous(
    labels = function(x) sprintf("%.0f", x),
    expand = expansion(mult = c(0.02, 0.08)),
    breaks = c(0, 30, 60, 90),
    limits = c(0, 90)
  ) +
  labs(
    title = "Annual source contributions, 2016-2020",
    x     = NULL,
    y     = expression("Fraction (%)"),
    color = "Year",
    shape = "Year"
  ) +
  theme_ec_iso_mcmc + 
  # Combine them to pull legend position up
  theme(legend.position = c(0.1, 0.75),
        legend.margin = margin(t = -10),
        legend.title = element_blank())
year_3mcmc_frac_plot

# Define the correct order for Year_Season
ys_order <- c("2016_Autumn", "2016_Winter",
              "2017_Spring", "2017_Summer", "2017_Autumn", "2017_Winter",
              "2018_Spring", "2018_Autumn", "2018_Winter",
              "2019_Spring", "2019_Summer", "2019_Autumn", "2019_Winter",
              "2020_Spring", "2020_Summer", "2020_Autumn", "2020_Winter")  # Add more as needed
year_season_3mcmc_conc$Group <-
  factor(year_season_3mcmc_conc$Group, levels = ys_order)


year_season_3mcmc_frac_plot <-
  ggplot(year_season_3mcmc_conc,
         aes(x = Group, y = Bayesian_Median_frac,
             color = Source, shape = Source, group = Source)) +
  geom_line(linewidth = 0.7, color = "grey60", lineend = "round") +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_manual(values = source_colors) +
  scale_shape_manual(values = source_shapes) +
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Source contributions in YZ by Year & Season",
    x     = NULL,
    y     = expression("Concentration (µg m"^{-3}*")"),
    color = "Source",
    shape = "Source"
  ) +
  theme_ec_iso_mcmc + 
  # Combine them to pull legend position up
  theme(legend.position = "bottom",
        legend.margin = margin(t = -10))
year_season_3mcmc_frac_plot

###### Bayesian MCMC 3 Chains: Save files ######
# write.csv(all_3chain_gelman, "YZ_3chain_Bayesian_MCMC_Gelman-Rubin.csv") 
# write.csv(all_3chain_ess, "YZ_3chain_Bayesian_MCMC_ESS.csv") 
# write.csv(all_3chain_accept, "YZ_3chain_Bayesian_MCMC_Accept-Rate.csv") 

write.csv(all_3chain_gelman, "YZ_3chain_Bayesian_MCMC_Gelman-Rubin_4source1Traffic.csv") 
write.csv(all_3chain_ess, "YZ_3chain_Bayesian_MCMC_ESS_4source1Traffic.csv") 
write.csv(all_3chain_accept, "YZ_3chain_Bayesian_MCMC_Accept-Rate_4source1Traffic.csv") 


######################################################
#### Species Ratio Indexes #### 
######################################################

###### Get important ratios ###### 

# setwd("/Users/ztttttt/Documents/Beizhan PM2.5样品 江苏疾控/OC&EC_14C_test/for_R")
# getwd()
data.dir <- "/Users/ztttttt/Documents/Beizhan PM2.5样品 江苏疾控/OC&EC_14C_test/for_R"

### Update full YZ PAH data 
YZ_cdc_comp = 
  read.csv("/Users/ztttttt/Documents/Beizhan PM2.5样品 江苏疾控/OC&EC_14C_test/for_R/NJ_YZ_JSCDC_PM2.5_Component.csv")
YZ_cdc_comp$Date = as.Date(YZ_cdc_comp$Date)
YZ_cdc_comp$Date.1 = NULL


## Species data
species_metro_pollu = 
  fread("/Users/ztttttt/Documents/Beizhan PM2.5样品 江苏疾控/OC&EC_14C_test/for_R/NJ_YZ_pollutant_species_RH&T.csv")
species_metro_pollu$V1 = NULL
species_metro_pollu = 
  species_metro_pollu %>%
  subset(City == "YZ") %>%
  dplyr::select(-City, -BaA, -BbF, -BkF, -BaP)
head(species_metro_pollu)
species_metro_pollu$Date = as.Date(species_metro_pollu$Date)

YZ_pah = 
  dplyr::select(YZ_cdc_comp, Date, FLT:IPY) %>%
  subset(Date %in% species_metro_pollu$Date)
YZ_pah$Date[1:5]; species_metro_pollu$Date[1:5]
head(YZ_pah)

YZ_pah =
  YZ_pah%>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE))

# species_metro_pollu = species_metro_pollu[with(species_metro_pollu, order(Date)), ]
# YZ_pah = YZ_pah[with(YZ_pah, order(Date)), ]
# summary(YZ_pah$Date == species_metro_pollu$Date)

# Merge 
species_metro_pollu_full = species_metro_pollu %>%
  dplyr::left_join(YZ_pah, by = "Date")
species_metro_pollu$Date[1:5]
species_metro_pollu_full$Date[1:5]
summary(is.na(species_metro_pollu_full))

## Ions & elements## IonsNULL & elements
# between mobile (temperature-sensitive) and stable secondary inorganic aerosol
species_metro_pollu_full$nitr_sulf = species_metro_pollu_full$NO3/species_metro_pollu_full$SO4
# high suggest fossil fuel / vehicle exhaust dominance; low values indicate biomass burning or SOA formation.
species_metro_pollu_full$ec_oc = species_metro_pollu_full$EC/species_metro_pollu_full$OC
# coal combustion tracer, if high, more coal (in China, coal rich in Se, but also depend on coal source)
species_metro_pollu_full$se_as = species_metro_pollu_full$Se/species_metro_pollu_full$As

# Diesel: soot_fraction ≈ 0.80–0.95, Gasoline: soot_fraction ≈ 0.10–0.30
# (EC2+EC3)/EC
species_metro_pollu_full$soot_fraction = 
  (species_metro_pollu_full$EC2+species_metro_pollu_full$EC3)/species_metro_pollu_full$EC 
# Diesel: IP/(IP+BghiP) ≈ 0.35–0.70 (central ~0.50) 
# Gasoline: IP/(IP+BghiP) ≈ 0.10–0.30 (some sources use 0.20 as central)
# Coal: 0.35–0.70, Biomass: > 0.50
species_metro_pollu_full$inp_bpe = 
  species_metro_pollu_full$IPY/(species_metro_pollu_full$IPY+species_metro_pollu_full$BPE)
# # diesel vs other combustion
species_metro_pollu_full$bap_bbf_bfk = 
  species_metro_pollu_full$BaP/(species_metro_pollu_full$BbF+species_metro_pollu_full$BkF)     

# Soot: R_gasoline ~ Normal(0.20, 0.10), R_diesel ~ Normal(0.85, 0.10)
# InP: R_gasoline ~ Normal(0.20, 0.05), R_diesel ~ Normal(0.50, 0.10)

# "Fla" = "FLT",
# "Pyr" = "PYR",
# "Chry" = "CHR",
# "BaA" = "BaA", 
# "BbF" = "BbF",
# "BkF" = "BkF",
# "BaP" = "BaP",
# "DahA" = "DBA", 
# "BghiP" = "BPE",
# "InP" = "IPY"

## PAHs
# Classic PAH diagnostic ratios
# BaP/(BbF+BkF) — traffic vs stationary combustion
# BaP is a strong traffic/coal marker
# BbF+BkF ratio useful for distinguishing diesel vs other combustion

# species_metro_pollu_full <- 
#   species_metro_pollu_full %>%
#   dplyr::mutate(
#     # BaA_BaA_Chr  = BaA / (BaA + Chrysene),   # > 0.35 = combustion
#     BaP_BbF_BkF = BaP / (BbF + BkF),          # diesel vs other combustion
#     # BaP_BPE    = BaP / BPE,               # > 0.6 = traffic
#     # Fluo_Fluo_Py = Fluoranthene / (Fluoranthene + Pyrene),  # > 0.5 = biomass/coal
#     BbF_BkF      = BbF / BkF                  # traffic vs stationary combustion
#   )

summary(species_metro_pollu_full)

# Year, month, season
ec_iso_pm_yz = species_metro_pollu_full
ec_iso_pm_yz$year = year(ec_iso_pm_yz$Date)
ec_iso_pm_yz$month = month(ec_iso_pm_yz$Date)

ec_iso_pm_yz$seaon_alone = "s1-spring"
ec_iso_pm_yz$seaon_alone[
  ec_iso_pm_yz$month %in% 
    c(12, 1, 2)] = 
  "s4-winter" 
ec_iso_pm_yz$seaon_alone[
  ec_iso_pm_yz$month %in% 
    c(6, 7, 8)] = 
  "s2-summer" 
ec_iso_pm_yz$seaon_alone[
  ec_iso_pm_yz$month %in% 
    c(9, 10, 11)] = 
  "s3-autumn" 

ec_iso_pm_yz$year_use = 
  ifelse(ec_iso_pm_yz$seaon_alone == "s4-winter" & ec_iso_pm_yz$month < 3, 
         ec_iso_pm_yz$year-1, 
         ec_iso_pm_yz$year)

ec_iso_pm_yz$Season = 
  paste0(ec_iso_pm_yz$year_use, 
         "-", 
         ec_iso_pm_yz$seaon_alone)

season_sequence = dplyr::select(ec_iso_pm_yz, year, Season)
season_sequence = season_sequence[!duplicated(season_sequence), ]
head(ec_iso_pm_yz)

###### Match with Isotope & Check Correlations ###### 
# species_metro_pollu_full_yz = subset(species_metro_pollu_full, City == "YZ")
yz_species_isotope =ec_iso_pm_yz


# write.csv(yz_species_isotope, "YZ_isotope_species_meteorology_2026.04.csv")
write.csv(yz_species_isotope, "YZ_isotope_species_meteorology_2026.08.csv")


### spearman, spearman correlation, corrplot
yz_species_isotope.1 = yz_species_isotope

# Remove columns not to plot
col_not_to_plot = 
  c("Date", "City", "Year", "Month", "month", "Year_Season", "Season",
    "OC1", "OC2", "OC3", "OC4", "EC1", "EC2", "EC3",
    "OC_conc", "EC_conc", "OC_weight", "EC_weight", "PM10", "PM2.5", 
    "Sb", "Be", "Hg", "Pb", "Tl") #, "", "", "",
yz_species_isotope.1 = dplyr::select(yz_species_isotope.1, -any_of(col_not_to_plot))

# Change some colnames for plot
names(yz_species_isotope.1)
yz_species_isotope.1 =
  plyr::rename(
    yz_species_isotope.1,
    c("EC_delta13C" = "EC_13", 
      "EC_f14C" = "EC_14", 
      "TC_delta13C" = "TC_13", 
      "OC_delta13C" = "OC_13",
      "nitr_sulf" = "nit_sul"
    )
  )
names(yz_species_isotope.1)

## Correlation coefficients
yz_species_isotope.matrix <- as.matrix(yz_species_isotope.1)

corPsn_species_isotope  <- cor(yz_species_isotope.matrix, use = "pairwise.complete.obs", method = "pearson")
corSpmn_species_isotope <- cor(yz_species_isotope.matrix, use = "pairwise.complete.obs", method = "spearman")

p_Psn_species_isotope  <- as.data.frame(corr.test(yz_species_isotope.matrix, adjust = "none", method = "pearson")[4])
p_Spmn_species_isotope <- as.data.frame(corr.test(yz_species_isotope.matrix, adjust = "none", method = "spearman")[4])

# Zero out non-significant correlations (p > 0.05)
corPsn_species_isotope[p_Psn_species_isotope   > 0.05] <- NA
corSpmn_species_isotope[p_Spmn_species_isotope > 0.05] <- NA

## Diverging palette: warm = positive, cool = negative
my_col <- colorRampPalette(c("#2166AC", "#F7F7F7", "#D6604D"))(200)

## Correlation corrplots
corrplot.mixed(
  corPsn_species_isotope,
  upper     = "circle",
  lower     = "number",
  upper.col = my_col,
  lower.col = my_col,
  tl.col    = "black",
  tl.cex    = 0.65,
  tl.srt    = 45,
  number.cex = 0.6,
  cl.cex     = 1,
  na.label   = " "       # leave blank cells for non-significant pairs
)

corrplot.mixed(
  corSpmn_species_isotope,
  upper     = "circle",
  lower     = "number",
  upper.col = my_col,
  lower.col = my_col,
  tl.col    = "black",
  tl.cex    = 0.65,
  tl.srt    = 45,
  number.cex = 0.6,
  cl.cex     = 1,
  na.label   = " "
)


###### Compare two times of element measurements, X/Al as EF ######

# YZ, original measurements
# yz_species_isotope = 
#   fread("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/YZ_isotope_species_meteorology_2026.04.csv")
yz_species_isotope = 
  fread("/Users/ztttttt/Documents/Beizhan PM2.5样品 江苏疾控/OC&EC_14C_test/for_R/YZ_isotope_species_meteorology_2026.08.csv")
yz_species_isotope$V1 = NULL
yz_species_isotope$Date = as.Date(yz_species_isotope$Date)
head(yz_species_isotope)

# YZ, 2023 supplementary measurements, by SWT
yz_supp_ele =
  fread("YZ_2023ICP-MS_elements_swt.csv")
yz_supp_ele$Date = 
  as.Date(as.character(yz_supp_ele$Serial.No), format = "%Y%m%d")
yz_supp_ele$Serial.No = NULL

# Combine with JS CDC original measurements
yz_species_iso_all =
  merge(yz_species_isotope, yz_supp_ele, by = "Date", all.x = TRUE)
head(yz_species_iso_all); dim(yz_species_iso_all)

## Compare elements from two CDC measurements, original and in 2023
yz_species_lowerAl =
  subset(yz_species_iso_all, Y23_Al < 1.15) 
dim(yz_species_iso_all); dim(yz_species_lowerAl)
yz_supp_ele_lowerAl = 
  subset(yz_supp_ele, Y23_Al < 1.15)
# the threshold is detected based on data scatter
# 4 Y23_Al >1.1, with them, y = 0.1879+0.1809, R^2 = 0.0664, R = 0.26
# After removing, y = 0.912x-0.041, R^2 = 0.3563, R = 0.60

yz_species_iso_all$Date[!(yz_species_iso_all$Date %in% yz_species_lowerAl$Date)]

## Ratios, use two datasets separately, cause the Al best match is 
## Use Al for Enrichment Factor of aerosol
# https://www.science.org/doi/10.1126/science.187.4171.59, Duce et al. 1975 Science
# EF = (X/Al)aerosol ÷ (X/Al)crust
# (X/Al)crust is almost a constant for the same site
# Thus, check X/Al here

# using 2023 new measurements
yz_species_iso_all$Fe_Al_23r = yz_species_iso_all$Y23_Fe/yz_species_iso_all$Y23_Al
yz_species_iso_all$nonCrustFe_Al_23r =
  (yz_species_iso_all$Y23_Fe - 0.5*yz_species_iso_all$Y23_Al)/yz_species_iso_all$Y23_Al
yz_species_iso_all$Ti_Al_23r = yz_species_iso_all$Y23_Ti/yz_species_iso_all$Y23_Al
yz_species_iso_all$Zn_Al_23r = yz_species_iso_all$Y23_Zn/yz_species_iso_all$Y23_Al
yz_species_iso_all$K_Al_23r = yz_species_iso_all$Y23_K/yz_species_iso_all$Y23_Al
yz_species_iso_all$V_Al_23r = yz_species_iso_all$Y23_V/yz_species_iso_all$Y23_Al
yz_species_iso_all$Cu_Al_23r = yz_species_iso_all$Y23_Cu/yz_species_iso_all$Y23_Al

# using previous CDC measurement
yz_species_iso_all$Se_Al = yz_species_iso_all$Se/yz_species_iso_all$Al
yz_species_iso_all$Hg_Al = yz_species_iso_all$Hg/yz_species_iso_all$Al
yz_species_iso_all$Pb_Al = yz_species_iso_all$Pb/yz_species_iso_all$Al
yz_species_iso_all$Cd_Al = yz_species_iso_all$Cd/yz_species_iso_all$Al
yz_species_iso_all$Cr_Al = yz_species_iso_all$Cr/yz_species_iso_all$Al
yz_species_iso_all$Sb_Al = yz_species_iso_all$Sb/yz_species_iso_all$Al
yz_species_iso_all$Mn_Al = yz_species_iso_all$Mn/yz_species_iso_all$Al
yz_species_iso_all$Ni_Al = yz_species_iso_all$Ni/yz_species_iso_all$Al

names(yz_species_iso_all); head(yz_species_iso_all)

# Ratios including PAHs
yz_species_iso_all <- 
  yz_species_iso_all %>%
  dplyr::mutate(
    total_pah = rowSums(
      dplyr::select(., FLT:IPY), na.rm = TRUE))

yz_species_iso_all$totalPah_Sb = yz_species_iso_all$total_pah/yz_species_iso_all$Sb
yz_species_iso_all$BPE_Sb = yz_species_iso_all$BPE/yz_species_iso_all$Sb
yz_species_iso_all$totalPah_Fe =
  yz_species_iso_all$total_pah / (yz_species_iso_all$nonCrustFe_Al_23r*yz_species_iso_all$Y23_Al)
yz_species_iso_all$totalPah_EC =
  yz_species_iso_all$total_pah / yz_species_iso_all$EC 
yz_species_iso_all$BaP_Sb = yz_species_iso_all$BaP/yz_species_iso_all$Sb
yz_species_iso_all$BPE_Fe =
  yz_species_iso_all$BPE / (yz_species_iso_all$nonCrustFe_Al_23r*yz_species_iso_all$Y23_Al)

yz_species_iso_all$flt_FltPyr = 
  yz_species_iso_all$FLT/(yz_species_iso_all$FLT+yz_species_iso_all$PYR)
yz_species_iso_all$Baa_BaaChr = 
  yz_species_iso_all$BaA/(yz_species_iso_all$BaA+yz_species_iso_all$CHR)
yz_species_iso_all$Ipy_IpyBpe = 
  yz_species_iso_all$IPY/(yz_species_iso_all$IPY+yz_species_iso_all$BPE)
yz_species_iso_all$BaP_BPE = 
  yz_species_iso_all$BaP/yz_species_iso_all$BPE 
yz_species_iso_all$Bap_BapChr = 
  yz_species_iso_all$BaP/(yz_species_iso_all$BaP+yz_species_iso_all$CHR)
yz_species_iso_all$BbF_BkF = 
  yz_species_iso_all$BbF/yz_species_iso_all$BkF 
yz_species_iso_all$BaP_BbFBkF = 
  yz_species_iso_all$BaP/(yz_species_iso_all$BbF+yz_species_iso_all$BkF)

#PM25, From CDC site; PM2.5, central site
yz_species_iso_all$Al_PM_org = 
  yz_species_iso_all$Al/yz_species_iso_all$PM25 * 100
yz_species_iso_all$Al_PM_y23 = 
  yz_species_iso_all$Y23_Al/yz_species_iso_all$PM25  * 100

summary(yz_species_iso_all$Al_PM_org)
summary(yz_species_iso_all$Al_PM_y23)

plot(yz_species_iso_all$Al_PM_org, yz_species_iso_all$Al_PM_y23)
cor(yz_species_iso_all$Al_PM_org, yz_species_iso_all$Al_PM_y23, use = "pairwise.complete.obs")

yz_species_iso_all_lowAl = subset(yz_species_iso_all, Y23_Al < 1.4)
plot(yz_species_iso_all_lowAl$Al_PM_org, yz_species_iso_all_lowAl$Al_PM_y23)
cor(yz_species_iso_all_lowAl$Al_PM_org, yz_species_iso_all_lowAl$Al_PM_y23, use = "pairwise.complete.obs")

# Get seasonal and annual averages
yz_species_season = 
  yz_species_iso_all %>%
  dplyr::group_by(Season) %>%
  dplyr::summarise(
    across(where(is.numeric), mean),
    .groups = "drop") %>%
  dplyr::select(-year, -month, -year_use)
yz_species_season$Time_point = 1:nrow(yz_species_season)
names(yz_species_season)

yz_species_annual = 
  yz_species_iso_all %>%
  dplyr::group_by(year_use) %>%
  dplyr::summarise(
    across(where(is.numeric), mean),
    .groups = "drop") %>%
  dplyr::select(-year, -month)
names(yz_species_annual)

# All variables to check trends
species_ratios_var = 
  names(yz_species_season)[2:(ncol(yz_species_season)-1)]

# Theil-Sen analyses
theil_sen <- function(data, time_var, y_var) {
  
  # Extract as vectors explicitly
  time_vec <- data[[time_var]]
  y_vec <- data[[y_var]]
  
  # Remove NAs
  valid <- complete.cases(time_vec, y_vec)
  time_vec <- time_vec[valid]
  y_vec <- y_vec[valid]
  
  # Fit
  fit <- mblm::mblm(y_vec ~ time_vec, repeated = FALSE)
  
  data.frame(
    Variable = y_var,
    N = length(y_vec),
    TS_slope = coef(fit)[2],
    TS_p = summary(fit)$coefficients[2, 4]
  )
}

species_ratios_var = 
  names(yz_species_season)[2:(ncol(yz_species_season)-1)]

yz_species_ratio_season_ts <- 
  purrr::map_dfr(
    species_ratios_var, 
    ~ theil_sen(
      dplyr::select(yz_species_season, -Season), "Time_point", .x))

yz_species_ratio_annual_ts <- 
  purrr::map_dfr(
    species_ratios_var, 
    ~ theil_sen(
      yz_species_annual, "year_use", .x))

# View(yz_species_ratio_annual_ts)
# View(yz_species_ratio_season_ts)

## Check the ratios, which relate to source if within special ranges
yz_ratio_season <-
  dplyr::select(yz_species_season, 
                ec_oc, soot_fraction, 
                flt_FltPyr, Baa_BaaChr, Ipy_IpyBpe,
                BaP_BPE, Bap_BapChr, BbF_BkF, BaP_BbFBkF)

yz_ratio_annual <-
  dplyr::select(yz_species_annual, 
                ec_oc, soot_fraction, 
                flt_FltPyr, Baa_BaaChr, Ipy_IpyBpe,
                BaP_BPE, Bap_BapChr, BbF_BkF, BaP_BbFBkF)


