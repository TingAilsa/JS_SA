###############################################################################
#  Source-specific EC / OC contribution trends vs. policies
#
#  Part 1  build_group_panel_mkts()          -> Theil-Sen per source x SEASON
#  Part 2  build_group_panel_mkts_overall()  -> Theil-Sen per source, ALL Year_Season points
#
#  x-axis  = Time (1, 2, 3, ... in chronological Year_Season order),
#            labelled with Year_Season (2016_Spring, 2016_Summer, ...)
#  Labels  = ggtext markdown subtitle in each panel (p < 0.05 in bold)
###############################################################################

library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)
library(data.table)

# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/isotope/")
setwd("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/isotope/")
getwd()

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

season_order  <- c("Spring", "Summer", "Autumn", "Winter")

policy_colors <- c("National" = "#B2182B",
                   "Local"    = "#2166AC")


###### Bayesian MCMC concentrations & Policies, main plot ######
### Bayesian source contribution
# ec_oc_iso_3mcmc = 
#   fread("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/Bayesian_3chain_MCMC_YZ_Isotope_source_2026.04.csv")
ec_oc_iso_3mcmc = 
  fread("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/Bayesian_3chain_MCMC_YZ_Isotope_4source1Traffic_2026.08.csv")
ec_oc_iso_3mcmc$V1 = NULL

ec_oc_iso_3mcmc_yearseason <- 
  ec_oc_iso_3mcmc %>%
  # keep only Year_Season rows, e.g. "2016_Spring"
  dplyr::filter(
    stringr::str_detect(
      Group, "^\\d{4}_(Spring|Summer|Autumn|Winter)$")) %>%
  dplyr::select(
    Year_Season = Group, Source,
    EC_conc_contri = EC_conc_contri_mean, OC_conc_contri = OC_conc_contri_mean) %>%
  dplyr::mutate(
    # Get Year and Season
    Year   = as.numeric(stringr::str_extract(Year_Season, "^\\d{4}")),
    Season = factor(sub(".*_", "", Year_Season), levels = season_order),
    # Get Time, a numeric index representing Year_Season
    # 4 slots per year: Spring = 1, Summer = 2, Autumn = 3, Winter = 4
    Time   = (Year - min(Year)) * 4 + as.integer(Season),
    Time   = Time - min(Time) + 1 # first observed season = 1
  ) %>%
  dplyr::arrange(Time)

# Check if Time column is correct
dplyr::distinct(ec_oc_iso_3mcmc_yearseason, Year_Season, Time)
length(unique(ec_oc_iso_3mcmc_yearseason$Time))

# ── Define source groups ──────────────────────────────────────────────────────
# source_groups <- 
#   list(
#     Biomass = c("C3_Plant", "C4_Plant"),
#     Traffic = c("Traffic_G1", "Traffic_G2"),
#     Coal    = c("Coal")
#   )

source_groups <- 
  list(
    Biomass = c("C3_Plant", "C4_Plant"),
    Traffic = c("Traffic"),
    Coal    = c("Coal")
  )

all_sources   <- unlist(source_groups, use.names = FALSE)
source_shapes <- 
  setNames(c(21, 24, 22, 23)[seq_along(all_sources)], all_sources)
source_lines  <- 
  setNames(c("solid", "dashed", "solid", "solid")[seq_along(all_sources)],
           all_sources)

# ── Define policies per group ─────────────────────────────────────────────────
# policies_biomass <- 
#   data.frame(
#     Year_num = c(2015.85, 2016.0, 2018.42, 2019.0),
#     # 2015.85 = Nov 2015; 2018.42 = June 2018 (State Council meeting)
#     label    = c("Four Ministries' Notice on Further Accelerating Crop Straw Comprehensive Utilization and Burning Ban",
#                  "Jiangsu Straw Burning Ban Special Fund and Inspection System",
#                  "State Council's Three-Year Action Plan for Winning the Blue Sky Defense War",
#                  "Jiangsu Straw Burning Ban Special Campaign"),
#     level    = c("National", "Local", "National", "Local"),
#     stringsAsFactors = FALSE
#   )
# 
# policies_traffic <-
#   data.frame(
#     Year_num = c(2016.0, 2017.85, 2018.42, 2018.75, 2019.0),
#     # 2017.85 = Nov 2017; 2018.75 = Oct 2018; 2019.0 = Jan 2019 (expansion of prohibited zones)
#     label    = c("Eastern 11 Provinces/Cities: Early Supply of National V Vehicle Gasoline and Diesel",
#                  "Jiangsu Preparation for National VI Vehicle Diesel",
#                  "State Council's Three-Year Action Plan for Winning the Blue Sky Defense War",
#                  "Jiangsu-Zhejiang-Shanghai Early Supply of National VI Gasoline and Diesel",
#                  "Expansion of High-Pollution Fuel Combustion Prohibited Zones in Jiangsu"),
#     level    = c("National", "Local", "National", "National", "Local"),
#     stringsAsFactors = FALSE
#   )
# 
# policies_coal <- 
#   data.frame(
#     Year_num = c(2015.35, 2016.42, 2016.96, 2017.67, 2018.42, 2018.85),
#     # 2015.35 = May 2015; 2016.42 = June 2016; 2016.96 = Dec 2016; 2017.67 = Sep 2017; 2018.85 = Nov 2018
#     label    = c("National Energy Administration's Clean and Efficient Coal Utilization Action Plan (2015-2020)",
#                  "Jiangsu EPA Coal-Fired Boiler Remediation Plan (Phase out <10 t/h by end of 2017)",
#                  "Jiangsu Coal Consumption Reduction and Substitution '1+2' Policy System (1.5× substitution for non-power sectors)",
#                  "Jiangsu Notice on Further Promoting Provincial Coal Reduction (Complete shutdown of ≤10 t/h boilers)",
#                  "State Council's Three-Year Action Plan for Winning the Blue Sky Defense War (Phase out <35 t/h boilers in key regions)",
#                  "Yangtze River Delta 2018-2019 Autumn-Winter Air Pollution Comprehensive Control Action Plan (Covers Jiangsu's 13 cities)"),
#     level    = c("National", "Local", "Local", "Local", "National", "National"),
#     # Yangtze River Delta plan: regional but treated as National per your instruction
#     stringsAsFactors = FALSE
#   )

policies_biomass <- 
  data.frame(
    Year_num = c(2015.85, 2016.0, 2018.42, 2019.0),
    label_full    = c("Four Ministries' Notice on Further Accelerating Crop Straw Comprehensive Utilization and Burning Ban",
                 "Jiangsu Straw Burning Ban Special Fund and Inspection System",
                 "State Council's Three-Year Action Plan for Winning the Blue Sky Defense War",
                 "Jiangsu Straw Burning Ban Special Campaign"),
    label = c("4-Ministry straw burning ban",
                    "JS straw ban fund & inspection",
                    "Blue Sky Defense War",
                    "JS straw ban campaign"),
    level    = c("National", "Local", "National", "Local"),
    stringsAsFactors = FALSE
  )

policies_traffic <-
  data.frame(
    Year_num = c(2016.0, 2017.85, 2018.42, 2018.75, 2019.0),
    label_full    = c("Eastern 11 Provinces/Cities: Early Supply of National V Vehicle Gasoline and Diesel",
                 "Jiangsu Preparation for National VI Vehicle Diesel",
                 "State Council's Three-Year Action Plan for Winning the Blue Sky Defense War",
                 "Jiangsu-Zhejiang-Shanghai Early Supply of National VI Gasoline and Diesel",
                 "Expansion of High-Pollution Fuel Combustion Prohibited Zones in Jiangsu"),
    label = c("China V fuel (11 eastern prov.)",
                    "JS China VI diesel prep.",
                    "Blue Sky Defense War",
                    "China VI fuel (JS-ZJ-SH)",
                    "JS no-burn zone expansion"),
    level    = c("National", "Local", "National", "National", "Local"),
    stringsAsFactors = FALSE
  )

policies_coal <- 
  data.frame(
    Year_num = c(2015.35, 2016.42, 2016.96, 2017.67, 2018.42, 2018.85),
    label_full    = c("National Energy Administration's Clean and Efficient Coal Utilization Action Plan (2015-2020)",
                 "Jiangsu EPA Coal-Fired Boiler Remediation Plan (Phase out <10 t/h by end of 2017)",
                 "Jiangsu Coal Consumption Reduction and Substitution '1+2' Policy System (1.5× substitution for non-power sectors)",
                 "Jiangsu Notice on Further Promoting Provincial Coal Reduction (Complete shutdown of ≤10 t/h boilers)",
                 "State Council's Three-Year Action Plan for Winning the Blue Sky Defense War (Phase out <35 t/h boilers in key regions)",
                 "Yangtze River Delta 2018-2019 Autumn-Winter Air Pollution Comprehensive Control Action Plan (Covers Jiangsu's 13 cities)"),
    label = c("NEA clean coal plan 2015–2020",
                    "JS boiler plan (<10 t/h)",
                    "JS coal '1+2' policy",
                    "JS ≤10 t/h boiler shutdown",
                    "Blue Sky Defense War (<35 t/h boilers)",
                    "YRD 2018–19 autumn-winter plan"),
    level    = c("National", "Local", "Local", "Local", "National", "National"),
    stringsAsFactors = FALSE
  )

# ── Convert policy years to Time index ───────────────────────────────────────
# Each Time point is placed at its season mid-point (decimal year); policy
# # Winter = 1.04, "2016_Winter" = Dec 2016 - Feb 2017.
season_mid <- 
  c("Spring" = 0.29, "Summer" = 0.54, "Autumn" = 0.79, "Winter" = 1.04)

# Get Time point of each Year_Season
time_lookup <- 
  ec_oc_iso_3mcmc_yearseason %>%
  dplyr::distinct(Time, Year_Season, Year, Season) %>%
  dplyr::arrange(Time) %>%
  dplyr::mutate(Year_dec = Year + season_mid[as.character(Season)])

time_range <- range(ec_oc_iso_3mcmc_yearseason$Time)
year_range <- range(ec_oc_iso_3mcmc_yearseason$Year)

# year_to_time <- 
#   function(yr) {
#     (yr - year_range[1]) / diff(year_range) * diff(time_range) + time_range[1]
#   }

year_to_time <-
  function(yr) {
    x   <- time_lookup$Year_dec
    y   <- time_lookup$Time
    out <- approx(x, y, xout = yr)$y
    lo  <- yr < min(x)
    hi  <- yr > max(x)
    out[lo] <- y[1]         - (x[1]         - yr[lo]) * 4
    out[hi] <- y[length(y)] + (yr[hi] - x[length(x)]) * 4
    out
  }

policies_biomass <- policies_biomass %>%
  dplyr::mutate(Time_idx = year_to_time(Year_num),
                label_wrap = stringr::str_wrap(label, 45))
policies_traffic <- policies_traffic %>%
  dplyr::mutate(Time_idx = year_to_time(Year_num),
                label_wrap = stringr::str_wrap(label, 45))
policies_coal <- policies_coal %>%
  dplyr::mutate(Time_idx = year_to_time(Year_num),
                label_wrap = stringr::str_wrap(label, 45))

# Same x-range in every panel (data + all policies)
time_xlim <- 
  range(c(time_lookup$Time,
          policies_biomass$Time_idx,
          policies_traffic$Time_idx,
          policies_coal$Time_idx)) + c(-0.5, 0.5)

# ── Theil-Sen slope + Mann-Kendal + 95% CI of the slope ────────────────────
#             min/max (ci_truncated = TRUE)
# MK tau, p : trend::mk.test (tie-corrected)
# Gilbert 1987, rank-based 95% CI, with very few points, it is wider than all pairwise slopes

compute_ts_mk <-
  function(x, y, conf = 0.95) {
    ok <- is.finite(x) & is.finite(y)
    x  <- x[ok]; y <- y[ok]
    o  <- order(x); x <- x[o]; y <- y[o]
    n  <- length(y)
    out <- list(n = n, slope = NA_real_, slope_lwr = NA_real_, slope_upr = NA_real_,
                ci_truncated = NA, mk_tau = NA_real_, mk_p = NA_real_)
    if (n < 3) return(out)
    
    ij     <- combn(n, 2)
    dx     <- x[ij[2, ]] - x[ij[1, ]]
    dy     <- y[ij[2, ]] - y[ij[1, ]]
    slopes <- sort((dy / dx)[dx != 0]) # slope between every pair of points
    N      <- length(slopes)
    
    mk   <- trend::mk.test(y)
    varS <- unname(mk$estimates["varS"])
    C    <- qnorm(1 - (1 - conf) / 2) * sqrt(varS)
    M1   <- floor((N - C) / 2)
    M2   <- ceiling((N + C) / 2) + 1
    
    # Theil-Sen slope = median of all pairwise slopes
    # textbook definition, and it gives the same number as mblm(y ~ x, repeated = FALSE)
    out$slope        <- median(slopes) 
    out$slope_lwr    <- slopes[max(M1, 1)]
    out$slope_upr    <- slopes[min(M2, N)]
    out$ci_truncated <- M1 < 1 || M2 > N
    out$mk_tau       <- unname(mk$estimates["tau"])
    out$mk_p         <- unname(mk$p.value)
    out
  }

# Fitted line and band at the data points
# band = envelope of the lines with slope, lower slope and upper slope
compute_ts_band <-
  function(x, y, st) {
    if (is.na(st$slope)) {
      return(data.frame(fitted = rep(NA_real_, length(x)),
                        lwr    = rep(NA_real_, length(x)),
                        upr    = rep(NA_real_, length(x))))
    }
    b  <- c(st$slope, st$slope_lwr, st$slope_upr)
    a  <- sapply(b, function(bb) median(y - bb * x, na.rm = TRUE))  # Conover intercept
    pr <- matrix(sapply(1:3, function(k) a[k] + b[k] * x), ncol = 3)
    data.frame(fitted = pr[, 1],
               lwr    = apply(pr, 1, min),
               upr    = apply(pr, 1, max))
  }

# Seasonal Mann-Kendall (Hirsch-Slack, EnvStats): pooled within-season slope (per Year)
compute_smk <-
  function(src_data, y_var) {
    smk_result <- tryCatch(
      EnvStats::kendallSeasonalTrendTest(
        y      = src_data[[y_var]],
        season = droplevels(src_data$Season),
        year   = src_data$Year
      ),
      error = function(e) NULL
    )
    if (is.null(smk_result)) {
      return(list(slope = NA_real_, tau = NA_real_, p = NA_real_, p_het = NA_real_))
    }
    list(slope = as.numeric(smk_result$estimate["slope"]),
         tau   = as.numeric(smk_result$estimate["tau"]),
         p     = as.numeric(smk_result$p.value["z (Trend)"]),
         p_het = as.numeric(smk_result$p.value["Chi-Square (Het)"]))
  }

# ── Label helpers (ggtext markdown) ──────────────────────────────────────────
fmt_num  <- 
  function(x, d = 3) ifelse(is.na(x), "NA", sprintf(paste0("%.", d, "f"), x))
fmt_p    <- 
  function(p) ifelse(is.na(p), "p = NA",
                               ifelse(p < 0.001, "p &lt; 0.001",
                                      paste0("p = ", sprintf("%.3f", p))))
bold_sig <- 
  function(txt, p) ifelse(!is.na(p) & p < 0.05, paste0("**", txt, "**"), txt)


# ── Part 1 function: per-season Theil-Sen (x = Year) + seasonal MK ───────────
compute_seasonal_trend <-
  function(data, y_var) {
    results <- data.frame()
    
    for (src in unique(data$Source)) {
      src_data <- data %>%
        dplyr::filter(Source == src) %>%
        dplyr::arrange(Time)
      
      smk <- compute_smk(src_data, y_var)
      
      season_fits <- list()
      for (ssn in season_order) {
        ssn_data <- src_data %>% dplyr::filter(Season == ssn) %>% dplyr::arrange(Year)
        if (nrow(ssn_data) == 0) next
        
        st   <- compute_ts_mk(ssn_data$Year, ssn_data[[y_var]])  # slope per Year
        band <- compute_ts_band(ssn_data$Year, ssn_data[[y_var]], st)
        
        season_fits[[ssn]] <- ssn_data %>%
          dplyr::mutate(
            ts_fitted           = band$fitted,
            ts_lwr              = band$lwr,
            ts_upr              = band$upr,
            season_n            = st$n,
            season_ts_slope     = st$slope,
            season_ts_slope_lwr = st$slope_lwr,
            season_ts_slope_upr = st$slope_upr,
            season_ci_truncated = st$ci_truncated,
            season_mk_tau       = st$mk_tau,
            season_mk_p         = st$mk_p,
            label_season = paste0(
              "<span style='color:", season_colors[ssn], "'>", ssn, "</span>: ",
              fmt_num(st$slope), " yr<sup>-1</sup> (MK τ = ", fmt_num(st$mk_tau, 2),
              ", ", bold_sig(fmt_p(st$mk_p), st$mk_p), ")")
          )
      }
      
      src_fitted <- dplyr::bind_rows(season_fits) %>%
        dplyr::mutate(
          overall_smk_slope = smk$slope,   # per Year, pooled Hirsch-Slack
          overall_smk_tau   = smk$tau,
          overall_smk_p     = smk$p,
          overall_smk_p_het = smk$p_het,
          label_smk = paste0(
            "**", src, "** seasonal MK: slope = ", fmt_num(smk$slope),
            " µg m<sup>-3</sup> yr<sup>-1</sup>, τ = ", fmt_num(smk$tau, 2), ", ",
            bold_sig(fmt_p(smk$p), smk$p),
            " (heterogeneity ", fmt_p(smk$p_het), ")")
        )
      
      results <- rbind(results, src_fitted)
    }
    return(results)
  }

# ── Part 2 function: Theil-Sen on all Year_Season points (x = Time) ──────────
compute_overall_trend <-
  function(data, y_var) {
    results <- data.frame()
    
    for (src in unique(data$Source)) {
      src_data <- data %>%
        dplyr::filter(Source == src) %>%
        dplyr::arrange(Time)
      
      st   <- compute_ts_mk(src_data$Time, src_data[[y_var]])  # slope per Time step (= per season)
      band <- compute_ts_band(src_data$Time, src_data[[y_var]], st)
      smk  <- compute_smk(src_data, y_var)
      
      src_fitted <- src_data %>%
        dplyr::mutate(
          ts_fitted            = band$fitted,
          ts_lwr               = band$lwr,
          ts_upr               = band$upr,
          overall_ts_slope     = st$slope,
          overall_ts_slope_lwr = st$slope_lwr,
          overall_ts_slope_upr = st$slope_upr,
          overall_mk_tau       = st$mk_tau,
          overall_mk_p         = st$mk_p,
          overall_smk_p        = smk$p,
          label_ts = paste0(
            "**", src, "**: TS slope = ", fmt_num(st$slope),
            " µg m<sup>-3</sup> per season [", fmt_num(st$slope_lwr), ", ",
            fmt_num(st$slope_upr), "]; MK τ = ", fmt_num(st$mk_tau, 2), ", ",
            bold_sig(fmt_p(st$mk_p), st$mk_p),
            "; seasonal MK ", bold_sig(fmt_p(smk$p), smk$p))
        )
      
      results <- rbind(results, src_fitted)
    }
    return(results)
  }


###### Plot helper (shared by Part 1 and Part 2) ######
plot_trend_panel <-
  function(data, y_var, y_lab, group_sources, policies,
           mode = c("season", "overall"), title = NULL, show_x = TRUE) {
    mode  <- match.arg(mode)
    d_grp <- 
      data %>% dplyr::filter(Source %in% group_sources)
    
    p <- ggplot(d_grp, aes(x = Time))
    
    if (mode == "season") {
      subtitle <- sapply(group_sources, function(src) {
        s <- d_grp %>% dplyr::filter(Source == src)
        if (nrow(s) == 0) return(NA_character_)
        ssn_lab <- s %>% dplyr::distinct(Season, label_season) %>%
          dplyr::arrange(Season) %>% dplyr::pull(label_season)
        ssn_lab <- split(ssn_lab, ceiling(seq_along(ssn_lab) / 2))   # 2 seasons per line
        paste(c(unique(s$label_smk),
                sapply(ssn_lab, paste, collapse = " · ")), collapse = "<br>")
      })
      col_limits <- c(season_order, names(policy_colors))
      col_name   <- "Season trend / Policy"
      
      p <- p +
        geom_ribbon(aes(ymin = ts_lwr, ymax = ts_upr, fill = Season,
                        group = interaction(Source, Season)),
                    alpha = 0.12) +
        geom_line(aes(y = ts_fitted, colour = Season, linetype = Source,
                      group = interaction(Source, Season)),
                  linewidth = 0.8)
    } else {
      subtitle <- sapply(group_sources, function(src) {
        unique(d_grp$label_ts[d_grp$Source == src])[1]
      })
      col_limits <- names(policy_colors)
      col_name   <- "Policy"
      
      p <- p +
        geom_ribbon(aes(ymin = ts_lwr, ymax = ts_upr, group = Source),
                    fill = "grey40", alpha = 0.15) +
        geom_line(aes(y = ts_fitted, linetype = Source, group = Source),
                  colour = "grey20", linewidth = 0.8)
    }
    subtitle <- paste(subtitle[!is.na(subtitle)], collapse = "<br>")
    
    p +
      # Policy lines and labels
      geom_vline(data = policies,
                 aes(xintercept = Time_idx, colour = level),
                 linetype = "dashed", linewidth = 0.5,
                 inherit.aes = FALSE) +
      geom_text(data = policies,
                aes(x = Time_idx, y = Inf, label = label_wrap, colour = level),
                angle = 90, hjust = 1.02, vjust = 1.3,   # inside panel, right of line
                size = 3.5, lineheight = 0.85,
                show.legend = FALSE, inherit.aes = FALSE) +
      # Estimated contributions
      geom_point(aes(y = .data[[y_var]]), 
                 # # For by season
                 # fill = Season, shape = Source, colour = "black", 
                 # For overall: 
                 color = "grey60", 
                 stroke = 0.3, size = 2.2, alpha = 0.85) +
      scale_fill_manual(values = season_colors, limits = season_order, name = "Season") +
      scale_colour_manual(values = c(season_colors, policy_colors),
                          limits = col_limits, name = col_name) +
      scale_shape_manual(values = source_shapes, limits = all_sources, name = "Source") +
      scale_linetype_manual(values = source_lines, limits = all_sources, name = "Source") +
      scale_x_continuous(breaks = time_lookup$Time, labels = time_lookup$Year_Season) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.45))) +  # room for policy text
      coord_cartesian(xlim = time_xlim) +
      guides(fill  = guide_legend(override.aes = list(shape = 21, colour = "black",
                                                      alpha = 1, linetype = 0)),
             shape = guide_legend(override.aes = list(fill = "grey80"))) +
      labs(x = NULL, y = y_lab, title = title, subtitle = subtitle) +
      theme_bw(base_size = 11) +
      theme(
        # plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_markdown(size = 12, lineheight = 1.25),
        axis.title.y     = element_text(size = 12.5),   
        axis.text.y      = element_text(size = 12),   
        axis.text.x      = if (show_x) element_text(angle = 90, vjust = 0.5,
                                                    hjust = 1, size = 12)
        else element_blank(),
        panel.grid.minor = element_blank())
  }

###### Source & Policies: 1. The seasonal trend of each source ######

# ── Run ────────────────────────────────────
ec_yearseason_conc_mkts <-
  compute_seasonal_trend(ec_oc_iso_3mcmc_yearseason, "EC_conc_contri")
oc_yearseason_conc_mkts <-
  compute_seasonal_trend(ec_oc_iso_3mcmc_yearseason, "OC_conc_contri")

ec_season_conc_mkts_id <- 
  ec_yearseason_conc_mkts %>%
  dplyr::distinct(
    Source, Season, season_n, season_ts_slope, season_ts_slope_lwr,
    season_ts_slope_upr, season_ci_truncated, season_mk_tau, season_mk_p)
oc_season_conc_mkts_id <- 
  oc_yearseason_conc_mkts %>%
  dplyr::distinct(
    Source, Season, season_n, season_ts_slope, season_ts_slope_lwr,
    season_ts_slope_upr, season_ci_truncated, season_mk_tau, season_mk_p)
head(ec_season_conc_mkts_id)
head(oc_season_conc_mkts_id)

ec_overall_conc_mkts_id <- 
  ec_yearseason_conc_mkts %>%
  dplyr::distinct(
    Source, overall_smk_slope, overall_smk_tau,
    overall_smk_p, overall_smk_p_het)
oc_overall_conc_mkts_id <- 
  oc_yearseason_conc_mkts %>%
  dplyr::distinct(
    Source, overall_smk_slope, overall_smk_tau,
    overall_smk_p, overall_smk_p_het)
head(ec_overall_conc_mkts_id)
head(oc_overall_conc_mkts_id)

build_group_panel_mkts <-
  function(ec_data, oc_data, group_name, group_sources, policies) {
    ec_panel <- plot_trend_panel(ec_data, "EC_conc_contri", "EC contribution (µg/m³)",
                                 group_sources, policies, mode = "season",
                                 title = group_name, show_x = FALSE)
    oc_panel <- plot_trend_panel(oc_data, "OC_conc_contri", "OC contribution (µg/m³)",
                                 group_sources, policies, mode = "season")
    ec_panel / oc_panel
  }

# ── Build figures — for each season ─────────────────────
fig_biomass_mkts <- build_group_panel_mkts(
  ec_yearseason_conc_mkts, oc_yearseason_conc_mkts,
  group_name    = "Biomass",
  group_sources = source_groups$Biomass,
  policies      = policies_biomass
)

fig_traffic_mkts <- build_group_panel_mkts(
  ec_yearseason_conc_mkts, oc_yearseason_conc_mkts,
  group_name    = "Traffic",
  group_sources = source_groups$Traffic,
  policies      = policies_traffic
)

fig_coal_mkts <- build_group_panel_mkts(
  ec_yearseason_conc_mkts, oc_yearseason_conc_mkts,
  group_name    = "Coal",
  group_sources = source_groups$Coal,
  policies      = policies_coal
)

# fig_combined_mkts <- (fig_biomass_mkts | fig_traffic_mkts | fig_coal_mkts) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom", legend.box = "horizontal")

# # cairo_pdf so that µ, τ, ≤, × render in the PDF
# ggsave("temporal_trends_combined_mkts.pdf", fig_combined_mkts,
#        width = 20, height = 12, units = "in", device = cairo_pdf)
# ggsave("temporal_trends_combined_mkts.png", fig_combined_mkts,
#        width = 20, height = 12, units = "in", dpi = 300)


###### Source & Policies: 2. Year-Season trend of each source ######

# ── Run ────────────────────────────────────
ec_ys_conc_ts <-
  compute_overall_trend(ec_oc_iso_3mcmc_yearseason, "EC_conc_contri")
oc_ys_conc_ts <-
  compute_overall_trend(ec_oc_iso_3mcmc_yearseason, "OC_conc_contri")

ec_ys_conc_ts_id <-
  ec_ys_conc_ts %>%
  dplyr::distinct(
    Source, overall_ts_slope, overall_ts_slope_lwr, overall_ts_slope_upr,
    overall_mk_tau, overall_mk_p, overall_smk_p)
oc_ys_conc_ts_id <- 
  oc_ys_conc_ts %>%
  dplyr::distinct(
    Source, overall_ts_slope, overall_ts_slope_lwr, overall_ts_slope_upr,
    overall_mk_tau, overall_mk_p, overall_smk_p)

head(ec_ys_conc_ts_id)
head(oc_ys_conc_ts_id)

build_group_panel_mkts_overall <-
  function(ec_data, oc_data, group_name, group_sources, policies) {
    ec_panel <- 
      plot_trend_panel(ec_data, "EC_conc_contri", "EC contribution (µg/m³)",
                       group_sources, policies, mode = "overall",
                       title = group_name, show_x = FALSE)
    oc_panel <- 
      plot_trend_panel(oc_data, "OC_conc_contri", "OC contribution (µg/m³)",
                       group_sources, policies, mode = "overall")
    ec_panel / oc_panel
  }

# ── Build figures ─────────────────────
fig_biomass_mkts_overall <- build_group_panel_mkts_overall(
  ec_ys_conc_ts, oc_ys_conc_ts,
  group_name    = "Biomass",
  group_sources = source_groups$Biomass,
  policies      = policies_biomass
)

fig_traffic_mkts_overall <- build_group_panel_mkts_overall(
  ec_ys_conc_ts, oc_ys_conc_ts,
  group_name    = "Traffic",
  group_sources = source_groups$Traffic,
  policies      = policies_traffic
)

fig_coal_mkts_overall <- build_group_panel_mkts_overall(
  ec_ys_conc_ts, oc_ys_conc_ts,
  group_name    = "Coal",
  group_sources = source_groups$Coal,
  policies      = policies_coal
)

fig_combined_mkts_overall <-
  (fig_biomass_mkts_overall | fig_traffic_mkts_overall | fig_coal_mkts_overall) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal")

ggsave("temporal_trends_combined_mkts_overall.pdf", fig_combined_mkts_overall,
       width = 20, height = 11, units = "in", device = cairo_pdf)
ggsave("temporal_trends_combined_mkts_overall.png", fig_combined_mkts_overall,
       width = 20, height = 11, units = "in", dpi = 300)






















# # ── Theil-Sen function ────────────────────────────────────────────────────────
# compute_theil_sen <- 
#   function(data, y_var) {
#   results <- data.frame()
#   
#   for (src in unique(data$Source)) {
#     src_data <- data %>% dplyr::filter(Source == src)
#     
#     fit  <- mblm::mblm(as.formula(paste(y_var, "~ Time")),
#                        data     = src_data,
#                        repeated = FALSE)
#     pred <- predict(fit, interval = "confidence", level = 0.95)
#     mk   <- trend::mk.test(src_data[[y_var]])
#     
#     temp <- src_data %>%
#       dplyr::mutate(
#         fitted   = pred[, 1],
#         lwr      = pred[, 2],
#         upr      = pred[, 3],
#         label_ts = paste0(src, ": ", "slope = ", round(coef(fit)[2], 4),
#                           ", p = ", ifelse(mk$p.value < 0.001, "< 0.001",
#                                            round(mk$p.value, 3)))
#       )
#     results <- rbind(results, temp)
#   }
#   return(results)
# }

# ec_yearseason_conc_ts <- 
#   compute_theil_sen(ec_oc_iso_3mcmc_yearseason, "EC_conc_contri")
# oc_yearseason_conc_ts <- 
#   compute_theil_sen(ec_oc_iso_3mcmc_yearseason, "OC_conc_contri")
# head(ec_yearseason_conc_ts)


# ── Seasonal Mann-Kendall + Sen slope per source ───────────────────────────── 
# Replaces compute_theil_sen
# Uses Year as x-axis (not Time index) so incomplete years are handled correctly
# Each season is treated independently then combined

compute_seasonal_trend <- function(data, y_var) {
  results <- data.frame()
  season_order <- c("Spring", "Summer", "Autumn", "Winter")
  
  for (src in unique(data$Source)) {
    src_data <- data %>%
      dplyr::filter(Source == src) %>%
      dplyr::mutate(Season = factor(Season, levels = season_order)) %>%
      dplyr::arrange(Year, Season)
    
    # ── Overall Seasonal Mann-Kendall (Hirsch-Slack, EnvStats) ───────────
    # Returns: pooled Sen slope (per Year), pooled tau, trend p, heterogeneity p
    smk_result <- tryCatch(
      EnvStats::kendallSeasonalTrendTest(
        y      = src_data[[y_var]],
        season = src_data$Season,
        year   = src_data$Year
      ),
      error = function(e) NULL
    )
    
    if (!is.null(smk_result)) {
      smk_slope_year <- as.numeric(smk_result$estimate["slope"])
      smk_tau        <- as.numeric(smk_result$estimate["tau"])
      smk_p_trend    <- as.numeric(smk_result$p.value["z (Trend)"])
      smk_p_het      <- as.numeric(smk_result$p.value["Chi-Square (Het)"])
    } else {
      smk_slope_year <- smk_tau <- smk_p_trend <- smk_p_het <- NA_real_
    }
    
    # ── Per-season Theil-Sen (mblm) + Mann-Kendall (trend::mk.test) ──────
    ts_slopes_year <- c()
    season_fits    <- list()
    
    for (ssn in season_order) {
      ssn_data <- src_data %>% dplyr::filter(Season == ssn) %>% dplyr::arrange(Year)
      if (nrow(ssn_data) < 3) next
      
      # Classic Theil-Sen (repeated = FALSE) with Year as x → slope per Year
      ts_fit <- mblm::mblm(as.formula(paste(y_var, "~ Year")),
                           data = ssn_data, repeated = FALSE)
      ts_pred <- predict(ts_fit, newdata = ssn_data,
                         interval = "confidence", level = 0.95)
      ts_slope_year <- as.numeric(coef(ts_fit)[2])
      ts_p_wilcox   <- summary(ts_fit)$coefficients[2, 4]  # mblm's Wilcoxon p
      
      # Plain Mann-Kendall (trend package — no tauk2 warnings)
      mk_ssn      <- trend::mk.test(ssn_data[[y_var]])
      mk_tau      <- as.numeric(mk_ssn$estimates["tau"])
      mk_p        <- as.numeric(mk_ssn$p.value)
      
      ts_slopes_year <- c(ts_slopes_year, ts_slope_year)
      
      season_fits[[ssn]] <- ssn_data %>%
        dplyr::mutate(
          # Theil-Sen (per season, per Year)
          ts_fitted        = ts_pred[, 1],
          ts_lwr           = ts_pred[, 2],
          ts_upr           = ts_pred[, 3],
          season_ts_slope  = round(ts_slope_year, 4),  # y per Year
          season_ts_p      = round(ts_p_wilcox, 4),    # mblm Wilcoxon p
          # Mann-Kendall (per season)
          season_mk_tau    = round(mk_tau, 3),
          season_mk_p      = round(mk_p, 4),
          # Combined label for plots
          label_season = paste0(
            ssn,
            ": TS slope = ", round(ts_slope_year, 4),
            ", MK tau = ",   round(mk_tau, 3),
            ", MK p = ",     ifelse(mk_p < 0.001, "< 0.001", round(mk_p, 3))
          )
        )
    }
    
    src_fitted <- dplyr::bind_rows(season_fits)
    
    # Two aggregate slopes for cross-checking
    smk_slope_median <- round(median(ts_slopes_year, na.rm = TRUE), 4)  # median of per-season TS
    smk_slope_hs     <- round(smk_slope_year, 4)                        # EnvStats pooled
    
    src_fitted <- src_fitted %>%
      dplyr::mutate(
        Source              = src,
        # Overall SMK (Hirsch-Slack, EnvStats)
        overall_smk_slope   = smk_slope_hs,      # per Year, pooled H-S
        overall_smk_tau     = round(smk_tau, 3),
        overall_smk_p       = smk_p_trend,
        overall_smk_p_het   = smk_p_het,
        # Cross-check: median of per-season TS slopes
        overall_ts_slope_median = smk_slope_median,
        
        label_smk = paste0(
          src,
          ": TS slope = ", round(overall_ts_slope_median, 3),
          ", p = ",     ifelse(smk_p_trend < 0.001, "< 0.001",
                               round(smk_p_trend, 3))
        )
      )
    
    results <- rbind(results, src_fitted)
  }
  return(results)
}


# Use mblm for theil-sen slope and p-value
get_slope_pvalue_ts_mblm <- 
  function(df, x, y) {
    clean_df <- df[complete.cases(df[[x]], df[[y]]), ]
    
    if (nrow(clean_df) < 2) {
      return(data.frame(diff_slope = NA, p_value = NA, intercept = NA))
    }
    
    tryCatch({
      # Fit the Theil-Sen model using mblm
      formula <- as.formula(paste(y, "~", x))
      # mblm::mblm, Classic (unweighted) Theil-Sen estimator, takes simple median of all pairwise slopes, ives equal weight to all data pairs
      model <- mblm::mblm(formula, data = clean_df)
      
      # Extract coefficients and summary
      model_summary <- summary(model)
      diff_slope <- coef(model)[2] # The slope coefficient for x
      intercept <- coef(model)[1]
      # p-value for the slope is typically in the summary's coefficients table
      p_value <- model_summary$coefficients[2, 4]
      
      return(data.frame(diff_slope = diff_slope, p_value = p_value, intercept = intercept))
      
    }, error = function(e) {
      return(data.frame(diff_slope = NA, p_value = NA, intercept = NA))
    })
  }

###### Source & Policies: 1. The seasonal trend of each source ######

# ── Run ────────────────────────────────────
### Hirsch-Slack fix version of Mann-Kendall (MK) trends
# Trend of each season, and a combined trends for each source across years
ec_yearseason_conc_mkts <-
  compute_seasonal_trend(ec_oc_iso_3mcmc_yearseason, "EC_conc_contri")
oc_yearseason_conc_mkts <-
  compute_seasonal_trend(ec_oc_iso_3mcmc_yearseason, "OC_conc_contri")

ec_season_conc_mkts_id <-
  dplyr::select(ec_yearseason_conc_mkts, Source, Season, label_season)
ec_season_conc_mkts_id = 
  ec_season_conc_mkts_id[!duplicated(ec_season_conc_mkts_id), ]
oc_season_conc_mkts_id <-
  dplyr::select(oc_yearseason_conc_mkts, Source, Season, label_season)
oc_season_conc_mkts_id = 
  oc_season_conc_mkts_id[!duplicated(oc_season_conc_mkts_id), ]
ec_season_conc_mkts_id; oc_season_conc_mkts_id

ec_overall_conc_mkts_id <-
  dplyr::select(ec_yearseason_conc_mkts, Source, 
                overall_smk_slope, overall_smk_tau, overall_smk_p, 
                overall_smk_p_het, overall_ts_slope_median)
ec_overall_conc_mkts_id = 
  ec_overall_conc_mkts_id[!duplicated(ec_overall_conc_mkts_id), ]
oc_overall_conc_mkts_id <-
  dplyr::select(oc_yearseason_conc_mkts, Source,
                overall_smk_slope, overall_smk_tau, overall_smk_p, 
                overall_smk_p_het, overall_ts_slope_median)
oc_overall_conc_mkts_id = 
  oc_overall_conc_mkts_id[!duplicated(oc_overall_conc_mkts_id), ]
ec_overall_conc_mkts_id; oc_overall_conc_mkts_id


ec_yearseason_conc_ts <-
  ec_oc_iso_3mcmc_yearseason %>%
  dplyr::group_by(Source, Season) %>%
  reframe(
    get_slope_pvalue_ts_mblm(pick(everything()), "Year", "EC_conc_contri")
  ) %>%
  ungroup()

oc_yearseason_conc_ts <-
  ec_oc_iso_3mcmc_yearseason %>%
  dplyr::group_by(Source, Season) %>%
  reframe(
    get_slope_pvalue_ts_mblm(pick(everything()), "Year", "OC_conc_contri")
  ) %>%
  ungroup()

build_group_panel_mkts <- 
  function(ec_data, oc_data,
           group_name, group_sources,
           policies) {
    
    ec_grp <- ec_data %>% dplyr::filter(Source %in% group_sources)
    oc_grp <- oc_data %>% dplyr::filter(Source %in% group_sources)
    
    n_src     <- length(group_sources)
    src_lines <- c("solid", "dashed", "dotted")[1:n_src]
    names(src_lines) <- group_sources
    
    # Season point shapes
    season_shapes <- c("Spring" = 21, "Summer" = 22,
                       "Autumn"   = 23, "Winter" = 24)
    
    # Annotation at last available year per source
    ec_label <- ec_grp %>%
      dplyr::group_by(Source) %>%
      dplyr::slice_max(Year, n = 1) %>%
      dplyr::ungroup()
    
    oc_label <- oc_grp %>%
      dplyr::group_by(Source) %>%
      dplyr::slice_max(Year, n = 1) %>%
      dplyr::ungroup()
    
    ec_label$Year = oc_label$Year = 2017
    
    # ── EC panel ────────────────────────────────────────────────────────────────
    ec_panel <-
      ggplot(ec_grp, aes(x = Year, linetype = Source)) +
      # Points — shape encodes season
      geom_point(aes(y = EC_conc_contri, shape = Season, color = Season),
                 alpha = 0.6, size = 2) +
      # Per-season trend ribbon and line
      geom_ribbon(aes(ymin = lwr, ymax = upr,
                      group = interaction(Source, Season)),
                  fill = "#555555", alpha = 0.12, color = NA) +
      geom_line(aes(y = fitted,
                    group = interaction(Source, Season)),
                color = "#555555", linewidth = 0.8) +
      # Policy lines — now use Year_num directly, no Time_idx needed
      geom_vline(data = policies,
                 aes(xintercept = Year_num, color = level),
                 linetype = "dashed", linewidth = 0.6,
                 inherit.aes = FALSE) +
      geom_text(data = policies,
                aes(x = Year_num, label = label, color = level),
                y = Inf, vjust = 1.3, hjust = -0.05,
                size = 2.5, angle = 90,
                inherit.aes = FALSE) +
      # SMK annotation
      geom_text(data = ec_label,
                aes(x     = Year,
                    y     = EC_conc_contri + 0.1,
                    label = label_mkts),
                hjust = -0.1, vjust = 0.5,
                size = 4, color = "#333333",
                inherit.aes = FALSE) +
      scale_linetype_manual(values = src_lines,    name = "Source") +
      scale_shape_manual(values    = season_shapes, name = "Season") +
      scale_color_manual(values    = c(season_colors, policy_colors)) +
      scale_x_continuous(breaks = unique(ec_grp$Year)) +
      labs(x     = NULL,
           y     = "EC contribution (µg/m³)",
           title = group_name) +
      theme_bw(base_size = 12) +
      theme(axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
            panel.grid.minor = element_blank(),
            plot.title       = element_text(face = "bold", size = 11),
            plot.margin      = margin(5, 60, 5, 5),
            legend.position  = "bottom",
            legend.box       = "vertical")
    # ec_panel
    
    # ── OC panel ────────────────────────────────────────────────────────────────
    oc_panel <-
      ggplot(oc_grp, aes(x = Year, linetype = Source)) +
      geom_point(aes(y = OC_conc_contri, shape = Season, color = Season),
                 alpha = 0.6, size = 2) +
      geom_ribbon(aes(ymin = lwr, ymax = upr,
                      group = interaction(Source, Season)),
                  fill = "#555555", alpha = 0.12, color = NA) +
      geom_line(aes(y = fitted,
                    group = interaction(Source, Season)),
                color = "#555555", linewidth = 0.8) +
      geom_vline(data = policies,
                 aes(xintercept = Year_num, color = level),
                 linetype = "dashed", linewidth = 0.6,
                 inherit.aes = FALSE) +
      geom_text(data = policies,
                aes(x = Year_num, label = label, color = level),
                y = Inf, vjust = 1.3, hjust = -0.05,
                size = 2.5, angle = 90,
                inherit.aes = FALSE) +
      geom_text(data = oc_label,
                aes(x     = Year,
                    y     = OC_conc_contri + 0.5,
                    label = label_mkts),
                hjust = -0.1, vjust = 0.5,
                size = 4, color = "#333333",
                inherit.aes = FALSE) +
      scale_linetype_manual(values = src_lines,    name = "Source") +
      scale_shape_manual(values    = season_shapes, name = "Season") +
      scale_color_manual(values    = c(season_colors, policy_colors)) +
      scale_x_continuous(breaks = unique(oc_grp$Year)) +
      labs(x = NULL,
           y = "OC contribution (µg/m³)") +
      theme_bw(base_size = 12) +
      theme(axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
            panel.grid.minor = element_blank(),
            plot.margin      = margin(5, 60, 5, 5),
            legend.position  = "bottom",
            legend.box       = "vertical")
    
    ec_panel / oc_panel +
      plot_layout(guides = "collect")
  }

# ── Build figures — for each seaon ─────────────────────
fig_biomass_mkts <- build_group_panel_mkts(
  ec_yearseason_conc_mkts, oc_yearseason_conc_mkts,
  group_name    = "Biomass",
  group_sources = source_groups$Biomass,
  policies      = policies_biomass
)

fig_traffic_mkts <- build_group_panel_mkts(
  ec_yearseason_conc_mkts, oc_yearseason_conc_mkts,
  group_name    = "Traffic",
  group_sources = source_groups$Traffic,
  policies      = policies_traffic
)

fig_coal_mkts <- build_group_panel_mkts(
  ec_yearseason_conc_mkts, oc_yearseason_conc_mkts,
  group_name    = "Coal",
  group_sources = source_groups$Coal,
  policies      = policies_coal
)

fig_combined_mkts <- fig_biomass_mkts | fig_traffic_mkts | fig_coal_mkts

ggsave("temporal_trends_combined_mkts.pdf", fig_combined_mkts,
       width = 20, height = 8, units = "in")
ggsave("temporal_trends_combined_mkts.png", fig_combined_mkts,
       width  = 20, height = 8,
       units  = "in", dpi    = 300)


###### Source & Policies: 2. Year-Seaon trend of each source ######

# ── Run ────────────────────────────────────

ec_ys_conc_ts <-
  ec_oc_iso_3mcmc_yearseason %>%
  dplyr::group_by(Source) %>%
  reframe(
    get_slope_pvalue_ts_mblm(pick(everything()), "Time", "EC_conc_contri")
  ) %>%
  ungroup()

oc_ys_conc_ts <-
  ec_oc_iso_3mcmc_yearseason %>%
  dplyr::group_by(Source) %>%
  reframe(
    get_slope_pvalue_ts_mblm(pick(everything()), "Time", "OC_conc_contri")
  ) %>%
  ungroup()

build_group_panel_mkts_overall <- 
  function(ec_data, oc_data,
           group_name, group_sources,
           policies) {
    
    ec_grp <- ec_data %>% dplyr::filter(Source %in% group_sources)
    oc_grp <- oc_data %>% dplyr::filter(Source %in% group_sources)
    
    n_src     <- length(group_sources)
    src_lines <- c("solid", "dashed", "dotted")[1:n_src]
    names(src_lines) <- group_sources
    
    # Season point shapes
    season_shapes <- c("Spring" = 21, "Summer" = 22,
                       "Autumn"   = 23, "Winter" = 24)
    
    # Annotation at last available year per source
    ec_label <- ec_grp %>%
      dplyr::group_by(Source) %>%
      dplyr::slice_max(Year, n = 1) %>%
      dplyr::ungroup()
    
    oc_label <- oc_grp %>%
      dplyr::group_by(Source) %>%
      dplyr::slice_max(Year, n = 1) %>%
      dplyr::ungroup()
    
    ec_label$Year_Season = oc_label$Year_Season = "2017_Spring"
    
    # ── EC panel ────────────────────────────────────────────────────────────────
    ec_panel <-
      ggplot(ec_grp, aes(x = Year_Season, linetype = Source)) +
      # Points — shape encodes season
      geom_point(aes(y = EC_conc_contri, shape = Season, color = Season),
                 alpha = 0.6, size = 2) +
      # Per-season trend ribbon and line
      geom_ribbon(aes(ymin = ts_lwr, ymax = ts_upr,
                      group = Source),
                  fill = "#555555", alpha = 0.12, color = NA) +
      geom_line(aes(y = ts_fitted,
                    group = Source),
                color = "#555555", linewidth = 0.8) +
      # Policy lines — now use Year_num directly, no Time_idx needed
      geom_vline(data = policies,
                 aes(xintercept = Year_num, color = level),
                 linetype = "dashed", linewidth = 0.6,
                 inherit.aes = FALSE) +
      geom_text(data = policies,
                aes(x = Year_num, label = label, color = level),
                y = Inf, vjust = 1.3, hjust = -0.05,
                size = 2.5, angle = 90,
                inherit.aes = FALSE) +
      # SMK annotation
      geom_text(data = ec_label,
                aes(x     = Year_Season,
                    y     = EC_conc_contri + 0.1,
                    label = label_smk),
                hjust = -0.1, vjust = 0.5,
                size = 4, color = "#333333",
                inherit.aes = FALSE) +
      scale_linetype_manual(values = src_lines,    name = "Source") +
      scale_shape_manual(values    = season_shapes, name = "Season") +
      scale_color_manual(values    = c(season_colors, policy_colors)) +
      scale_x_continuous(breaks = unique(ec_grp$Year)) +
      labs(x     = NULL,
           y     = "EC contribution (µg/m³)",
           title = group_name) +
      theme_bw(base_size = 12) +
      theme(axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
            panel.grid.minor = element_blank(),
            plot.title       = element_text(face = "bold", size = 11),
            plot.margin      = margin(5, 60, 5, 5),
            legend.position  = "bottom",
            legend.box       = "vertical")
    # ec_panel
    
    # ── OC panel ────────────────────────────────────────────────────────────────
    oc_panel <-
      ggplot(oc_grp, aes(x = Year, linetype = Source)) +
      geom_point(aes(y = OC_conc_contri, shape = Season, color = Season),
                 alpha = 0.6, size = 2) +
      geom_ribbon(aes(ymin = ts_lwr, ymax = ts_upr,
                      group = Source),
                  fill = "#555555", alpha = 0.12, color = NA) +
      geom_line(aes(y = ts_fitted,
                    group = Source),
                color = "#555555", linewidth = 0.8) +
      geom_vline(data = policies,
                 aes(xintercept = Year_num, color = level),
                 linetype = "dashed", linewidth = 0.6,
                 inherit.aes = FALSE) +
      geom_text(data = policies,
                aes(x = Year_num, label = label, color = level),
                y = Inf, vjust = 1.3, hjust = -0.05,
                size = 2.5, angle = 90,
                inherit.aes = FALSE) +
      geom_text(data = oc_label,
                aes(x     = Year_Season,
                    y     = EC_conc_contri + 0.1,
                    label = label_smk),
                hjust = -0.1, vjust = 0.5,
                size = 4, color = "#333333",
                inherit.aes = FALSE) +
      scale_linetype_manual(values = src_lines,    name = "Source") +
      scale_shape_manual(values    = season_shapes, name = "Season") +
      scale_color_manual(values    = c(season_colors, policy_colors)) +
      scale_x_continuous(breaks = unique(oc_grp$Year)) +
      labs(x = NULL,
           y = "OC contribution (µg/m³)") +
      theme_bw(base_size = 12) +
      theme(axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
            panel.grid.minor = element_blank(),
            plot.margin      = margin(5, 60, 5, 5),
            legend.position  = "bottom",
            legend.box       = "vertical")
    
    ec_panel / oc_panel +
      plot_layout(guides = "collect")
  }

# ── Build figures — identical naming to your current code ─────────────────────
fig_biomass_mkts <- build_group_panel_mkts(
  ec_ys_conc_ts, oc_ys_conc_ts,
  group_name    = "Biomass",
  group_sources = source_groups$Biomass,
  policies      = policies_biomass
)

fig_traffic_mkts <- build_group_panel_mkts(
  ec_yearseason_conc_mkts, oc_yearseason_conc_mkts,
  group_name    = "Traffic",
  group_sources = source_groups$Traffic,
  policies      = policies_traffic
)

fig_coal_mkts <- build_group_panel_mkts(
  ec_yearseason_conc_mkts, oc_yearseason_conc_mkts,
  group_name    = "Coal",
  group_sources = source_groups$Coal,
  policies      = policies_coal
)

fig_combined_mkts <- fig_biomass_mkts | fig_traffic_mkts | fig_coal_mkts

ggsave("temporal_trends_combined_mkts.pdf", fig_combined_mkts,
       width = 20, height = 8, units = "in")
ggsave("temporal_trends_combined_mkts.png", fig_combined_mkts,
       width  = 20, height = 8,
       units  = "in", dpi    = 300)

##### Further spit traffic into Diesel & Gasoline ######

# Function to split traffic using one diagnostic ratio
split_traffic_by_ratio <- function(f_traffic_draws,   # posterior draws of f_traffic per sample
                                   R_obs,             # observed ratio for the sample
                                   R_gasoline_mean, R_gasoline_sd,
                                   R_diesel_mean,   R_diesel_sd,
                                   n_split = 100) {
  # For each posterior draw of f_traffic, sample endmembers and phi
  n_post <- length(f_traffic_draws)
  n_total <- n_post * n_split
  
  R_gas  <- rnorm(n_total, R_gasoline_mean, R_gasoline_sd)
  R_dsl  <- rnorm(n_total, R_diesel_mean,   R_diesel_sd)
  
  # Assume small analytical uncertainty on R_obs (~5% of value or absolute 0.02)
  R_meas <- rnorm(n_total, R_obs, 0.02)
  
  phi <- (R_dsl - R_meas) / (R_dsl - R_gas)
  phi <- pmax(0, pmin(1, phi))   # clamp
  
  # Recycle f_traffic draws
  f_traf <- rep(f_traffic_draws, each = n_split)
  
  data.frame(
    f_gasoline = phi * f_traf,
    f_diesel   = (1 - phi) * f_traf,
    phi        = phi
  )
}

yz_species_isotope = 
  fread("/Users/ztttttt/Documents/Beizhan PM2.5样品 江苏疾控/OC&EC_14C_test/for_R/YZ_isotope_species_meteorology_2026.08.csv")
head(yz_species_isotope)

library(dplyr)
library(stringr)
yz_species_isotope <- 
  yz_species_isotope %>%
  mutate(
    Year_Season = paste0(
      str_extract(Season, "^\\d{4}"),                       # year prefix
      "_",
      str_to_title(str_extract(Season, "[a-z]+$"))          # season, Title-cased
    )
  )
head(yz_species_isotope)

# Season names
group_names <- names(year_season_3mcmc) 
group_names


# ── Loop over year_season groups, apply both ratios ────────────────────
results_soot <- list()
results_InP  <- list()

for (g in group_names) {
  # g = group_names[1]
  posterior      <- year_season_3mcmc[[g]]$posterior
  f_traf_draws   <- posterior[, "Traffic"]     # vector, ~10,000 draws
  
  ratios <- 
    yz_species_isotope %>% dplyr::filter(Year_Season == g)
  
  # Soot-based split
  split_s <- split_traffic_by_ratio(
    f_traf_draws, ratios$soot_fraction,
    R_gasoline_mean = 0.20, R_gasoline_sd = 0.10,
    R_diesel_mean   = 0.85, R_diesel_sd   = 0.10
  )
  
  results_soot[[g]] <- data.frame(
    Year_Season   = g,
    f_gasoline    = mean(split_s$f_gasoline),
    f_gasoline_lo = quantile(split_s$f_gasoline, 0.025),
    f_gasoline_hi = quantile(split_s$f_gasoline, 0.975),
    f_diesel      = mean(split_s$f_diesel),
    f_diesel_lo   = quantile(split_s$f_diesel,   0.025),
    f_diesel_hi   = quantile(split_s$f_diesel,   0.975),
    method        = "soot_fractiontion"
  )
  
  # InP-based split
  split_i <- split_traffic_by_ratio(
    f_traf_draws, ratios$inp_bpe,
    R_gasoline_mean = 0.20, R_gasoline_sd = 0.05,
    R_diesel_mean   = 0.50, R_diesel_sd   = 0.10
  )
  results_InP[[g]] <- data.frame(
    Year_Season   = g,
    f_gasoline    = mean(split_i$f_gasoline),
    f_gasoline_lo = quantile(split_i$f_gasoline, 0.025),
    f_gasoline_hi = quantile(split_i$f_gasoline, 0.975),
    f_diesel      = mean(split_i$f_diesel),
    f_diesel_lo   = quantile(split_i$f_diesel,   0.025),
    f_diesel_hi   = quantile(split_i$f_diesel,   0.975),
    method        = "inp_bpe"
  )
}

results_soot <- dplyr::bind_rows(results_soot)
results_InP  <- dplyr::bind_rows(results_InP)

# Combine for side-by-side comparison
results_all <- rbind(results_soot, results_InP)

head(results_all)

results_all_sum <-
  results_all %>%
  dplyr::group_by(method) %>%
  dplyr::summarise(
    f_gasoline_mean = mean(f_gasoline),
    f_diesel_mean = mean(f_diesel),
    f_gasoline_median = median(f_gasoline),
    f_diesel_median = median(f_diesel)
  )
