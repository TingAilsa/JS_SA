library(data.table)
library(readxl)
library(lubridate)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(purrr)

# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test")
setwd("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test")
getwd()


# Colorblind-friendly palette (Okabe-Ito scheme)
source_colors <- c(
  "Coal"       = "#000000",   # black
  "C3_Plant"   = "#009E73",   # bluish green
  "C4_Plant"   = "#0072B2",   # blue
  "Traffic_G1" = "#D55E00",   # vermillion
  "Traffic_G2" = "#E69F00"    # orange
)

# year_colors <- c("2016" = "#4477AA",  # blue
#                  "2017" = "#EE6677",  # red
#                  "2018" = "#228833",  # green
#                  "2019" = "#CCBB44",  # yellow
#                  "2020" = "#AA3377")  # purple
# 
# year_colors <- c("2016" = "#E69F00",  # orange
#                  "2017" = "#56B4E9",  # sky blue
#                  "2018" = "#009E73",  # green
#                  "2019" = "#CC79A7",  # pink/purple
#                  "2020" = "#0072B2")  # dark blue

season_colors <- c(
  "Spring" = "#56B4E9",   # sky blue
  "Summer" = "#F0E442",   # yellow
  "Autumn" = "#CC79A7",   # pink/purple
  "Winter" = "#332288"    # indigo
)

# year_colors <- 
#   c("2016" = "#332288",  # indigo
#     "2017" = "#88CCEE",  # cyan
#     "2018" = "#44AA99",  # teal
#     "2019" = "#DDCC77",  # sand
#     "2020" = "#CC6677")  # rose

year_colors <- c(
  "2016" = "#000000",   # black
  "2017" = "#009E73",   # bluish green
  "2018" = "#0072B2",   # blue
  "2019" = "#D55E00",   # vermillion
  "2020" = "#E69F00"    # orange
)

# Matching shapes so color is not the only encoding
source_shapes <- c(
  "Coal"       = 16,   # filled circle
  "C3_Plant"   = 17,   # filled triangle up
  "C4_Plant"   = 15,   # filled square
  "Traffic_G1" = 18,   # filled diamond
  "Traffic_G2" = 8     # asterisk
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

####  Perform Thiel-Sen regression and return the slope #### 
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

####  Data preparation #### 
# Read the files
ec_iso <- read_excel("isotope/Final_data_yz_isotope_analyses.xlsx", sheet = 1)
oc_iso <- read_excel("isotope/Final_data_yz_isotope_analyses.xlsx", sheet = 2)

nj_iso <- read_excel("isotope/Nanjing_PhD_Shanli_13C_source.xlsx", sheet = 1)

# Renmae column
names(ec_iso)[c(1, 4, 5)]
names(oc_iso)[c(1, 4, 5)]
names(ec_iso)[c(1, 4, 5)] = c("Date", "EC_delta13C", "EC_f14c")
names(oc_iso)[c(1, 4, 5)] = c("Date", "TC_delta13C", "OC_delta13C")
sapply(ec_iso, class)
sapply(oc_iso, class)

# Remove NA row
ec_iso = na.omit(ec_iso)
oc_iso = subset(oc_iso, !(Name == "YZ-20190214-S-2"))
oc_iso$OC_delta13C = as.numeric(oc_iso$OC_delta13C)

# Date format
ec_iso$Date <- 
  as.Date(as.numeric(ec_iso$Date), 
          origin = "1899-12-30")
oc_iso$Date <- as.Date(oc_iso$Date)

head(ec_iso); head(oc_iso)

# Remove column not to use
ec_iso$Name = oc_iso$Name = NULL

# Average if there are duplicates
ec_iso = 
  ddply(ec_iso, .(Date, Season), summarise,
        EC_delta13C = mean(EC_delta13C),
        EC_f14c = mean(EC_f14c))
oc_iso = 
  ddply(oc_iso, .(Date, Season), summarise,
        TC_delta13C = mean(TC_delta13C),
        OC_delta13C = mean(OC_delta13C))

# Get year & month
ec_iso$Year = year(ec_iso$Date)
oc_iso$Year = year(oc_iso$Date)
ec_iso$Month = month(ec_iso$Date)
oc_iso$Month = month(oc_iso$Date)

# Dec of last year, change it to winter of the next year
ec_iso_newS = ec_iso
oc_iso_newS = oc_iso

ec_iso_newS$Year = 
  ifelse(ec_iso_newS$Month != 12, ec_iso_newS$Year, ec_iso_newS$Year + 1) 
oc_iso_newS$Year = 
  ifelse(oc_iso_newS$Month != 12, oc_iso_newS$Year, oc_iso_newS$Year + 1) 

# Add Year_Season column
ec_iso$Year_Season = 
  paste0(ec_iso$Year, "_", ec_iso$Season)
oc_iso$Year_Season = 
  paste0(oc_iso$Year, "_", ec_iso$Season)

ec_iso_newS$Year_Season = 
  paste0(ec_iso_newS$Year, "_", ec_iso_newS$Season)
oc_iso_newS$Year_Season = 
  paste0(oc_iso_newS$Year, "_", ec_iso_newS$Season)

# Define the correct order for Year_Season
ys_order <- c("2016_Autumn", "2016_Winter",
              "2017_Spring", "2017_Summer", "2017_Autumn", "2017_Winter",
              "2018_Spring", "2018_Autumn", "2018_Winter",
              "2019_Spring", "2019_Summer", "2019_Autumn", "2019_Winter",
              "2020_Spring", "2020_Summer", "2020_Autumn", "2020_Winter")  # Add more as needed

# Convert Year_Season to a factor with the correct order
ec_iso$Year_Season <-
  factor(ec_iso$Year_Season, levels = ys_order)
oc_iso$Year_Season <-
  factor(oc_iso$Year_Season, levels = ys_order)

ec_iso_newS$Year_Season <-
  factor(ec_iso_newS$Year_Season, levels = ys_order)
oc_iso_newS$Year_Season <-
  factor(oc_iso_newS$Year_Season, levels = ys_order)

head(ec_iso); head(oc_iso)
head(ec_iso_newS); head(oc_iso_newS)

# Change for plotting
ec_iso_long = 
  ec_iso %>%
  pivot_longer(
    cols = EC_delta13C:EC_f14c,
    values_to = "Values",
    names_to = "Isotope_metric"
  )

oc_iso_long = 
  oc_iso %>%
  pivot_longer(
    cols = TC_delta13C:OC_delta13C,
    values_to = "Values",
    names_to = "Isotope_metric"
  )

ec_iso_newS_long = 
  ec_iso_newS %>%
  pivot_longer(
    cols = EC_delta13C:EC_f14c,
    values_to = "Values",
    names_to = "Isotope_metric"
  )

oc_iso_newS_long = 
  oc_iso_newS %>%
  pivot_longer(
    cols = TC_delta13C:OC_delta13C,
    values_to = "Values",
    names_to = "Isotope_metric"
  )

head(ec_iso_long); head(oc_iso_long)
summary(ec_iso_long); summary(oc_iso_long)
sapply(ec_iso_long, class)
sapply(oc_iso_long, class)

# write.csv(ec_iso_long,
#           file.path("isotope", "EC_dual_Isotope_YZ.csv"))
# write.csv(oc_iso_long,
#           file.path("isotope", "OC_13C_Isotope_YZ.csv"))

season_order = c("Spring", "Summer", "Autumn", "Winter")


####  Figures, scatter #### 
head(ec_iso)
head(oc_iso)

ec_oc_iso = merge(ec_iso, oc_iso)

format_iso_var <- function(variable) {
  variable <- gsub("δ13", "δ\u00B9\u00B3", variable)
  variable <- gsub("F14", "F\u00B9\u00B4", variable)
  variable <- gsub("CEC", "C\u209E\u210C", variable) 
  variable <- gsub("CTC", "C\u208T\u208C", variable) 
  variable <- gsub("COC", "C\u208O\u208C", variable) 
  variable <- gsub("m3", "m\u00B3", variable) 
  
  return(variable)
}

format_iso_var <- function(variable) {
  # For δ13C and F14C, we'll use expression() instead of Unicode substitution
  if(variable == "δ13C") return(expression(delta^13*C))
  if(variable == "F14C") return(expression(F^14*C))
  
  # For CEC, CTC, COC, also use expression
  if(variable == "CEC") return(expression(C[EC]))
  if(variable == "CTC") return(expression(C[TC]))
  if(variable == "COC") return(expression(C[OC]))
  
  # For unit
  if(variable == "m3") return(expression(m^3))
  
  # If none of the above, return original
  return(variable)
}


ec_dual_iso_p <-
  ggplot(subset(ec_iso, Season == "Winter"), 
         aes(x = EC_delta13C, y = EC_f14c, 
             color = factor(Year), shape = factor(Year))) +
  geom_point(size = 3.5) +
  scale_shape_manual(values = c(6, 13, 5, 8)) +  
  scale_color_manual(values = c("#E7B800", "#FC4E07", "#00AFBB", "#4E84C4")) +  
  scale_x_continuous(breaks = function(x) pretty(x, n=3)) +
  # scale_y_continuous(breaks = function(x) pretty(x, n=2)) +
  labs(title = "Dual-Isotopic Signatures in Winter",
       x = expression(delta^13*C[EC]~" (‰)"), y = expression(F^14*C[EC]~" (%)")) +
  theme_bw(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        #panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 0),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  


ec_oc_iso_p <- 
  ggplot(subset(ec_oc_iso, Season == "Winter"), 
         aes(x = EC_delta13C, y = TC_delta13C, 
             color = factor(Year), shape = factor(Year))) +
  geom_point(size = 3.5) +
  scale_shape_manual(values = c(6, 13, 5, 8)) +  
  scale_color_manual(values = c("#E7B800", "#FC4E07", "#00AFBB", "#4E84C4")) +  
  scale_x_continuous(breaks = function(x) pretty(x, n=3)) +
  labs(title = expression(delta^13*C[EC] ~ " vs. " ~ delta^13*C[TC] ~ "in Winter"),
       x = expression(delta^13*C[EC]~" (‰)"), y = expression(delta^13*C[TC]~" (‰)")) +
  theme_bw(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        #panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  

iso_scatter_p = ec_dual_iso_p + ec_oc_iso_p
iso_scatter_p

# ggsave(plot = iso_scatter_p, 
#        file.path("isotope/figure", "13C_14C_scatter.pdf"), 
#        width = 14.5, height = 7.5)

######  Isotope biplot - source specific ###### 

# Nanjing Isotope info, by source
# nj_iso_use = 
#   subset(nj_iso, Source %in% c("Coal", "Traffic", "C3_Plant", "C4_Plant"))
nj_iso_use = 
  subset(nj_iso, Source %in% c("Coal", "Traffic_G1", "Traffic_G2", "C3_Plant", "C4_Plant"))
nj_iso_use$Use = nj_iso_use$Sample = NULL
names(nj_iso_use) = c("Data", "EC_delta13C", "EC_f14c")
head(nj_iso_use)

# Yangzhou isotope, Winter
ec_iso_winter <- subset(ec_iso, Season == "Winter")
ec_iso_winter <-
  dplyr::select(ec_iso_winter, 
                Year, EC_delta13C, EC_f14c)
names(ec_iso_winter)[1] = "Data"

# average
ec_iso_winter <-
  ddply(ec_iso_winter, .(Data), summarise,
        EC_delta13C = mean(EC_delta13C),
        EC_f14c = mean(EC_f14c))

# write.csv(ec_iso_winter, "isotope/YZ_EC_13C_14C_with_source_Winter.csv")

# plot
ec_iso_njSource_yz_winter <-
  ggplot() + 
  geom_point(data = ec_iso_winter,
             aes(x = EC_delta13C, y = EC_f14c, color = factor(Data)),
             size = 4, alpha = 0.9) +
  scale_color_manual(values = c("#E7B800", "#FC4E07", "#00AFBB", "#4E84C4")) +  
  geom_point(data = nj_iso_use,
             aes(x = EC_delta13C, y = EC_f14c, shape = factor(Data)),
             size = 3, stroke = 1, color = "darkorchid4") +
  scale_shape_manual(values = c(5, 6, 0, 13, 8)) +
  scale_x_continuous(breaks = function(x) pretty(x, n=3)) +
  # scale_y_continuous(breaks = function(x) pretty(x, n=2)) +
  labs(title = "Dual-Isotopic Signatures in Winter",
       x = expression(delta^13*C[EC]~" (‰)"), y = expression(F^14*C[EC]~" (%)")) +
  theme_bw(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        #panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 0),
        # legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  
ec_iso_njSource_yz

# Make sure ec_iso$Season and nj_iso_use$Data are of factor class
ec_iso$Season <- factor(ec_iso$Season, levels = c("Spring","Summer","Autumn","Winter"))
nj_iso_use$Data <- factor(nj_iso_use$Data, 
                          levels = c("Coal","C3_Plant","C4_Plant","Traffic_G1","Traffic_G2"))



iso_njSource_yz_year_season <-
  ggplot() + 
  # Dataset 1: ec_iso — color = Year, shape = Season
  # geom_point(data = ec_iso,
  #            aes(x = EC_delta13C, y = EC_f14c, 
  #                color = factor(Year), shape = Season),
  #            size = 4, alpha = 0.9) +
  geom_point(
    data = ec_iso,
    aes(x = EC_delta13C, y = EC_f14c, 
        color = factor(Year), shape = Season),
    size = 3.5,
    alpha = 0.85,
    position = position_jitter(width = 0.008, height = 0.008)
  ) +
  # Dataset 2: nj_iso_use — fixed color, shape = Data type
  geom_point(data = nj_iso_use,
             aes(x = EC_delta13C, y = EC_f14c, shape = Data),
             size = 3, stroke = 1.2, color = "darkorchid4") +
  
  # Unified shape scale covering ALL levels from both datasets
  scale_shape_manual(
    values = c(
      # ec_iso Season levels (e.g. Spring/Summer/Autumn/Winter)
      "Spring"  = 16,
      "Summer"  = 17,
      "Autumn"  = 15,
      "Winter"  = 18,
      # nj_iso_use Data levels — use open/distinct shapes to visually separate
      "Coal" = 5,
      "C3_Plant" = 6,
      "C4_Plant" = 0,
      "Traffic_G1" = 13,
      "Traffic_G2" = 8
    )
  ) +
  
  scale_color_manual(
    values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#CC79A7"),
    name   = "Year"
  ) +
  
  scale_x_continuous(breaks = function(x) pretty(x, n = 3)) +
  
  labs(
    title = "Dual-Isotopic Signatures: by Season by Year",
    x     = expression(delta^13*C[EC]~" (‰)"),
    y     = expression(F^14*C[EC]~" (%)")
  ) +
  
  theme_bw(base_size = 20) +
  theme(
    plot.background  = element_rect(color = NA),
    strip.background = element_rect(fill = "grey85"),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(size = 20),
    legend.title     = element_text(size = 0),
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1)
  )
iso_njSource_yz_year_season
  

nj_iso_use_1 = nj_iso_use
names(nj_iso_use_1)[1] = "Source"
nj_iso_use_1

# Get annual & seasonal averages
ec_iso_year = 
  ec_iso %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    EC_delta13C = mean(EC_delta13C),
    EC_f14c = mean(EC_f14c),
    .groups = "drop"
  )
ec_iso_season = 
  ec_iso %>%
  dplyr::group_by(Season) %>%
  dplyr::summarise(
    EC_delta13C = mean(EC_delta13C),
    EC_f14c = mean(EC_f14c),
    .groups = "drop"
  )

# Summarise ec_iso by Year + Season
ec_iso_year_sum <- 
  ec_iso %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    x_mean = mean(EC_delta13C, na.rm = TRUE),
    y_mean = mean(EC_f14c,     na.rm = TRUE),
    x_min  = min(EC_delta13C,  na.rm = TRUE),
    x_max  = max(EC_delta13C,  na.rm = TRUE),
    y_min  = min(EC_f14c,      na.rm = TRUE),
    y_max  = max(EC_f14c,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Year   = factor(Year)
  )
head(ec_iso_year_sum)

ec_iso_season_sum <- 
  ec_iso %>%
  dplyr::group_by(Season) %>%
  dplyr::summarise(
    x_mean = mean(EC_delta13C, na.rm = TRUE),
    y_mean = mean(EC_f14c,     na.rm = TRUE),
    x_min  = min(EC_delta13C,  na.rm = TRUE),
    x_max  = max(EC_delta13C,  na.rm = TRUE),
    y_min  = min(EC_f14c,      na.rm = TRUE),
    y_max  = max(EC_f14c,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))
  )
head(ec_iso_season_sum)


iso_njSource_yz_year <-
  ggplot() +
  ## Annual mean + range
  # vertical error bar (F14C range)
  geom_errorbar(
    data = ec_iso_year_sum,
    aes(x    = x_mean,
        ymin = y_min,
        ymax = y_max,
        color = factor(Year)),
    width = 0.1, linewidth = 0.6, alpha = 0.7
  ) +
  # horizontal error bar (delta13C range)
  geom_errorbarh(
    data = ec_iso_year_sum,
    aes(y     = y_mean,
        xmin  = x_min,
        xmax  = x_max,
        color = factor(Year)),
    width = 0.1, linewidth = 0.6, alpha = 0.7
  ) +
  # mean point
  geom_point(
    data = ec_iso_year,
    aes(x     = EC_delta13C,
        y     = EC_f14c,
        color = factor(Year)),
    size = 4, alpha = 0.85
  ) +
  scale_color_manual(
    values = year_colors, 
    name   = "Year"
  ) +
  
  ### Dataset 2: nj_iso_use, source endmember
  geom_point(
    data = nj_iso_use_1,
    aes(x = EC_delta13C, y = EC_f14c, shape = Source),
    size = 3, stroke = 1.2, color = "darkorchid4"
  ) +
  scale_shape_manual(
    name = "Source",
    values = source_shapes
  ) +
  
  scale_x_continuous(breaks = function(x) pretty(x, n = 3)) +
  labs(
    title = "Dual-Isotopic Signatures: by Year",
    x     = expression(delta^13*C[EC]~" (‰)"),
    y     = expression(F^14*C[EC]~" (%)")
  ) +
  theme_bw(base_size = 24) +
  theme(
    plot.background  = element_rect(color = NA),
    strip.background = element_rect(fill = "grey85"),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(size = 20),
    legend.title     = element_text(size = 12),
    axis.text.x      = element_text(angle = 0, hjust = 1, vjust = 1)
  )
iso_njSource_yz_year

iso_njSource_yz_season <-
  ggplot() +
  ## Annual mean + range
  # vertical error bar (F14C range)
  geom_errorbar(
    data = ec_iso_season_sum,
    aes(x    = x_mean,
        ymin = y_min,
        ymax = y_max,
        color = factor(Season)),
    width = 0.1, linewidth = 0.6, alpha = 0.7
  ) +
  # horizontal error bar (delta13C range)
  geom_errorbarh(
    data = ec_iso_season_sum,
    aes(y     = y_mean,
        xmin  = x_min,
        xmax  = x_max,
        color = factor(Season)),
    width = 0.1, linewidth = 0.6, alpha = 0.7
  ) +
  # mean point
  geom_point(
    data = ec_iso_season,
    aes(x     = EC_delta13C,
        y     = EC_f14c,
        color = factor(Season)),
    size = 4, alpha = 0.85
  ) +
  scale_color_manual(
    values = season_colors, 
    name   = "season"
  ) +
  
  ### Dataset 2: nj_iso_use, source endmember
  geom_point(
    data = nj_iso_use_1,
    aes(x = EC_delta13C, y = EC_f14c, shape = Source),
    size = 3, stroke = 1.2, color = "darkorchid4"
  ) +
  scale_shape_manual(
    name = "Source",
    values = source_shapes
  ) +
  
  scale_x_continuous(breaks = function(x) pretty(x, n = 3)) +
  labs(
    title = "Dual-Isotopic Signatures: by season",
    x     = expression(delta^13*C[EC]~" (‰)"),
    y     = expression(F^14*C[EC]~" (%)")
  ) +
  theme_bw(base_size = 24) +
  theme(
    plot.background  = element_rect(color = NA),
    strip.background = element_rect(fill = "grey85"),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(size = 20),
    legend.title     = element_text(size = 12),
    # legend.position = "bottom",
    axis.text.x      = element_text(angle = 0, hjust = 0.5, vjust = 1)
  )
iso_njSource_yz_season

# Get annual & seasonal averages
ec_iso_year = 
  ec_iso %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    EC_delta13C = mean(EC_delta13C),
    EC_f14c = mean(EC_f14c),
    .groups = "drop"
  )
ec_iso_season = 
  ec_iso %>%
  dplyr::group_by(Season) %>%
  dplyr::summarise(
    EC_delta13C = mean(EC_delta13C),
    EC_f14c = mean(EC_f14c),
    .groups = "drop"
  )

# ggsave(plot = ec_iso_njSource_yz, 
#        file.path("isotope", "YZ_EC_13C_14C_with_source.pdf"), 
#        width = 7.5, height = 6.2)

ggsave(plot = ec_iso_njSource_yz, 
       file.path("isotope", "YZ_EC_13C_14C_with_source_mark.pdf"), 
       width = 7.5, height = 6.2)

iso_njSource_yz_year_season
iso_njSource_yz_year
iso_njSource_yz_season

######  Figures, PM2.5, OC, & EC ###### 

# Yangzhou OC EC data
pm_c_yz <- read.csv("YZ_element_ion_pah_updateC_2024.08.csv")
pm_c_yz <- select(pm_c_yz, Date, PM25, OC, EC)
pm_c_yz$Date = as.Date(pm_c_yz$Date)
head(pm_c_yz)

# Get annual average
pm_c_yz$Year = year(pm_c_yz$Date)
names(pm_c_yz)[2] = "PM2.5"
pm_c_yz <-
  pm_c_yz %>%
  pivot_longer(
    cols = PM2.5:EC,
    values_to = "Concentration",
    names_to = "Pollutant"
  )
head(pm_c_yz)

pm_c_yz_annual =
  ddply(pm_c_yz, .(Year, Pollutant), summarise,
        conc_mean = mean(Concentration),
        conc_lower = quantile(Concentration, 0.05),
        conc_upper = quantile(Concentration, 0.95))

pm_c_yz_annual_plot <- 
  ggplot(pm_c_yz_annual, aes(x = Year)) +
  geom_line(aes(y = conc_mean), col = "grey45") +
  geom_errorbar(aes(ymin = conc_lower, ymax = conc_upper),  # , color = Pollutant
                width = 0.1, linewidth = 0.5, alpha = 0.8, color = "#0072B2") +
  geom_point(aes(y = conc_mean),  # , color = Pollutant
             size = 2.5, stroke = 1, alpha = 0.8, color = "#0072B2") +
  facet_grid(Pollutant ~., scales = "free_y") +
  # scale_x_continuous(breaks = function(x) pretty(x, n=3)) +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n=2)) +
  labs(title = "Annual PM2.5, OC, and EC Conc",
       x = "Year", y = expression("Concentration,  µg /"~m^3)) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 0),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  
pm_c_yz_annual_plot

ggsave(plot = pm_c_yz_annual_plot, 
       file.path("isotope", "YZ_PM_EC_OC_annual.pdf"), 
       width = 5.5, height = 6.2)

write.csv(pm_c_yz_annual, "isotope/figure/YZ_PM_EC_OC_annual.csv")

####  Figures for original season classification #### 
#### Changes in EC, 1
ggplot(ec_iso_long,
       aes(x = Year_Season, 
           y = Values,
           color = Season)) +
  geom_boxplot() +
  facet_grid(Isotope_metric ~ ., scales = "free_y") +
  scale_color_manual(values = season_colors) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text( size = 20),
        axis.text.x = element_text( angle = 45, hjust = 1, vjust = 1))

#### Changes in EC, 2
ec_iso_long_avg = 
  ddply(ec_iso_long, .(Year, Season, Isotope_metric), summarise, 
        Values = mean(Values))
oc_iso_long_avg = 
  ddply(oc_iso_long, .(Year, Season, Isotope_metric), summarise, 
        Values = mean(Values))

ec_iso_long_avg$Season <-
  factor(ec_iso_long_avg$Season, levels = season_order)
oc_iso_long_avg$Season <-
  factor(oc_iso_long_avg$Season, levels = season_order)

# ec_iso_boxplot <-
ggplot(ec_iso_long_avg,
       aes(x = Season, 
           y = Values,
           group = Year)) + 
  geom_line(linetype = "dashed", color = "gray", size = 0.65) + 
  geom_point(aes(color = Season, size = Year), alpha = 0.55) +  
  facet_grid(Isotope_metric ~ ., scales = "free_y") +
  scale_color_manual(values = season_colors) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  

#### Figures for Updated season classification #### 

#### Changes in EC, 1

ggplot(ec_iso_newS_long,
       aes(x = Year_Season, 
           y = Values,
           color = Season)) +
  geom_boxplot() +
  facet_grid(Isotope_metric ~ ., scales = "free_y") +
  scale_color_manual(values = season_colors) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text( size = 20),
        axis.text.x = element_text( angle = 45, hjust = 1, vjust = 1))

#### Changes in EC, 2
ec_iso_newS_long_avg = 
  ddply(ec_iso_newS_long, .(Year, Season, Isotope_metric), summarise, 
        Values = mean(Values))
oc_iso_newS_long_avg = 
  ddply(oc_iso_newS_long, .(Year, Season, Isotope_metric), summarise, 
        Values = mean(Values))

ec_iso_newS_long_avg$Season <-
  factor(ec_iso_newS_long_avg$Season, levels = season_order)
oc_iso_newS_long_avg$Season <-
  factor(oc_iso_newS_long_avg$Season, levels = season_order)

# ec_iso_newS_boxplot <-
ggplot(ec_iso_newS_long_avg,
       aes(x = Season, 
           y = Values,
           group = Year)) + 
  geom_line(linetype = "dashed", color = "gray", size = 0.65) + 
  geom_point(aes(color = Season, size = Year), alpha = 0.55) +  
  facet_grid(Isotope_metric ~ ., scales = "free_y") +
  scale_color_manual(values = season_colors) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  


#### Changes in OC, 1
oc_iso_newS_long_avg = 
  ddply(oc_iso_newS_long, .(Year, Season, Isotope_metric), summarise, 
        Values = mean(Values))
oc_iso_newS_long_avg = 
  ddply(oc_iso_newS_long, .(Year, Season, Isotope_metric), summarise, 
        Values = mean(Values))

oc_iso_newS_long_avg$Season <-
  factor(oc_iso_newS_long_avg$Season, levels = season_order)
oc_iso_newS_long_avg$Season <-
  factor(oc_iso_newS_long_avg$Season, levels = season_order)

ggplot(oc_iso_newS_long_avg,
       aes(x = Season, 
           y = Values,
           group = Year)) + 
  geom_line(linetype = "dashed", color = "gray", size = 0.65) + 
  geom_point(aes(color = Season, size = Year), alpha = 0.55) +  
  facet_grid(Isotope_metric ~ ., scales = "free_y") +
  scale_color_manual(values = season_colors) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  


#### YZ, EC biomass, EC fossil ####
###### EC & OC, bb & ff contribution estimation ######

# Read original F14C records and extract YZ data
ec_oc_f14c <- read_excel("isotope/AMS data Beizhan.xlsx", sheet = 1)
ec_oc_f14c <- na.omit(ec_oc_f14c)
names(ec_oc_f14c)[1:3] = c("Sample_Name", "Fraction_modern", "Total_uncertainty")
head(ec_oc_f14c)

# Get f14C
# There is the nuclear explosion in 1952, which influence the f14C
ec_oc_f14c$f14c = ec_oc_f14c$Fraction_modern/1.1
ec_oc_f14c_yz = 
  subset(ec_oc_f14c, City == "YZ") %>%
  dplyr::select(Year, Season, EC_OC, f14c)
head(ec_oc_f14c_yz); sapply(ec_oc_f14c_yz, class)

# Get EC_14c, and OC_14c
ec_oc_f14c_yz <-
  ec_oc_f14c_yz %>%
  pivot_wider(
    names_from = "EC_OC",
    values_from = "f14c"
  )
names(ec_oc_f14c_yz)[3:4] = c("EC_f14c", "OC_f14c")
head(ec_oc_f14c_yz)

# Read original OC EC conc records and extract YZ data
ec_oc_season <- 
  # read_excel("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/NJ_YZ_OC_EC_with_14C.xlsx", 
  read_excel("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/NJ_YZ_OC&EC_14C_test/NJ_YZ_OC_EC_with_14C.xlsx", 
             sheet = "YZ")
names(ec_oc_season)[10:16]

# Get seasonal conc
ec_oc_season = ec_oc_season[, 12:16]
names(ec_oc_season) = c("Year", "Season", "PM25", "OC", "EC")
ec_oc_season = na.omit(ec_oc_season)
head(ec_oc_season)

# Merge conc & f14c
ec_oc_conc_f14c =
  merge(ec_oc_season, ec_oc_f14c_yz)
ec_oc_conc_f14c$Season[ec_oc_conc_f14c$Season == "Fall"] = "Autumn"
ec_oc_conc_f14c_2 = ec_oc_conc_f14c


# Get Biomass burning (BB), and fossil fuel contributed EC & OC
ec_oc_conc_f14c$EC_Biomass = ec_oc_conc_f14c$EC * ec_oc_conc_f14c$EC_f14c / 100
ec_oc_conc_f14c$OC_Biomass = ec_oc_conc_f14c$OC * ec_oc_conc_f14c$OC_f14c / 100

ec_oc_conc_f14c$EC_Fossil = ec_oc_conc_f14c$EC - ec_oc_conc_f14c$EC_Biomass
ec_oc_conc_f14c$OC_Fossil = ec_oc_conc_f14c$OC - ec_oc_conc_f14c$OC_Biomass


ec_oc_conc_f14c_2$ECbb = ec_oc_conc_f14c_2$EC * ec_oc_conc_f14c_2$EC_f14c / 100
ec_oc_conc_f14c_2$OCbb = ec_oc_conc_f14c_2$OC * ec_oc_conc_f14c_2$OC_f14c / 100

ec_oc_conc_f14c_2$ECff = ec_oc_conc_f14c_2$EC - ec_oc_conc_f14c_2$ECbb
ec_oc_conc_f14c_2$OCff = ec_oc_conc_f14c_2$OC - ec_oc_conc_f14c_2$OCbb

head(ec_oc_conc_f14c)
head(ec_oc_conc_f14c_2)

# Data for plotting

ec_oc_conc_f14c_plot =
  dplyr::select(ec_oc_conc_f14c, Year, Season, 
                EC_Biomass, OC_Biomass, EC_Fossil, OC_Fossil) %>%
  pivot_longer(
    cols = EC_Biomass:OC_Fossil,
    names_to = "EC_OC",
    values_to = "Concentration"
  ) %>%
  separate(EC_OC, into = c("EC_OC", "Source"), sep = "_")
head(ec_oc_conc_f14c_plot)

ec_oc_conc_f14c_plot = 
  ec_oc_conc_f14c_plot %>%
  pivot_wider(
    names_from = "EC_OC",
    values_from = "Concentration"
  )
names(ec_oc_conc_f14c_plot)[4:5]
head(ec_oc_conc_f14c_plot)
# Plot

ec_oc_conc_f14c_box_plot =
  dplyr::select(ec_oc_conc_f14c_2, Year, Season, 
                ECbb, OCbb, ECff, OCff) %>%
  pivot_longer(
    cols = ECbb:OCff,
    names_to = "C_Source",
    values_to = "Concentration"
  ) 
head(ec_oc_conc_f14c_box_plot)

## Theil-Sen temporal trend



# write.csv(ec_oc_conc_f14c_plot, "isotope/EC_OC_14C_BB_Fossil.csv")

###### EC & OC, bb & ff contribution: plotting ######

ec_oc_conc_fossil_bb_box <-
  ggplot(subset(ec_oc_conc_f14c_box_plot, Season == "Winter"), 
         aes(x = Year)) +
  geom_line(aes(y = Concentration), col = "grey45") +
  geom_point(aes(y = Concentration),  # , color = Pollutant
             size = 2.5, stroke = 1, alpha = 0.8, color = "#0072B2") +
  facet_grid(C_Source ~.) + # , scales = "free_y"
  # scale_x_continuous(breaks = function(x) pretty(x, n=3)) +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n=3)) +
  labs(title = "Annual OC & EC from FF & BB",
       x = "Year", y = expression("Concentration,  µg /"~m^3)) +
  theme_base(base_size = 20) + 
  theme(plot.background = element_rect(color = NA),
        panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 0),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  
ec_oc_conc_fossil_bb_box

# ggsave(plot = ec_oc_conc_fossil_bb_box, 
#        file.path("isotope", "EC_OC_14C_BB_FF_boxplot_winter.pdf"), 
#        width = 5.5, height = 7.5)


ec_oc_conc_fossil_bb <-
  ggplot(subset(ec_oc_conc_f14c_plot, Season == "Winter"), 
         aes(x = EC, y = OC, 
             color = factor(Year), shape = Source)) +
  geom_point(size = 3.5, stroke = 1) +
  scale_shape_manual(values = c(8, 13)) + # 6, 13, 5, 8
  scale_color_manual(values = c("#E7B800", "#FC4E07", "#00AFBB", "#4E84C4")) +  # , "darkorchid3"
  scale_x_continuous(breaks = function(x) pretty(x, n=3)) +
  # scale_y_continuous(breaks = function(x) pretty(x, n=2)) +
  labs(title = "OC and EC from FF and BB",
       x = expression("EC,  µg /"~m^3), y = expression("OC,  µg /"~m^3)) +
  theme_bw(base_size = 22) + 
  theme(plot.background = element_rect(color = NA),
        #panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 0),
        # legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1))  
ec_oc_conc_fossil_bb


ec_oc_conc_bb <-
  ggplot(subset(ec_oc_conc_f14c_plot, 
                Season == "Winter" & Source == "Biomass"), 
         aes(x = EC, y = OC, 
             color = factor(Year))) +
  geom_point(size = 3.5, stroke = 1, shape = 8) +
  # scale_shape_manual(values = c(13)) + # 6, 13, 5, 8
  scale_color_manual(values = c("#E7B800", "#FC4E07", "#00AFBB", "#4E84C4")) +  # , "darkorchid3"
  scale_x_continuous(breaks = function(x) pretty(x, n=3)) +
  scale_y_continuous(breaks = function(x) pretty(x, n=3)) +
  # scale_y_continuous(breaks = function(x) pretty(x, n=2)) +
  labs(title = "OC and EC from Biomass Burning",
       x = expression("EC,  µg /"~m^3), y = expression("OC,  µg /"~m^3)) +
  theme_bw(base_size = 22) + 
  theme(plot.background = element_rect(color = NA),
        #panel.spacing = unit(5, "mm"),
        strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 0),
        # legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1))  
ec_oc_conc_bb

# ggsave(plot = ec_oc_conc_fossil_bb, 
#        file.path("isotope", "EC_OC_14C_BB_Fossil_winter.pdf"), 
#        width = 7.5, height = 6.2)

ggsave(plot = ec_oc_conc_bb, 
       file.path("isotope", "EC_OC_14C_BB_only_winter.pdf"), 
       width = 7.5, height = 6.2)

###### EC & OC, bb & ff contribution: theil-sen ######
library(dplyr)
library(purrr)

ec_oc_conc_f14c_box_plot <-
  ec_oc_conc_f14c_box_plot %>%
  mutate(
    Season     = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter")),
    Year       = factor(Year),
    YearSeason = factor(paste(Year, Season, sep = " "),  # space instead of \n
                        levels = yearseason_levels)
  )

# ── 1. Seasonal trend (numeric season index 1–4) ──────────────────────────────
ec_oc_conc_f14c_box_plot$Season_num = as.numeric(ec_oc_conc_f14c_box_plot$Season)

seasonal_trend <- 
  ec_oc_conc_f14c_box_plot %>%
  dplyr::mutate(Season_num = as.numeric(Season)) %>%
  dplyr::group_by(C_Source, Season, Season_num) %>%
  dplyr::summarise(Concentration = mean(Concentration, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(C_Source) %>%
  dplyr::group_modify(~ get_slope_pvalue_ts_mblm(.x, x = "Season_num", y = "Concentration")) %>%
  ungroup()

ec_oc_conc_f14c_season_plot <-
  ec_oc_conc_f14c_box_plot %>%
  dplyr::group_by(Season, C_Source) %>%
  dplyr::summarise(
    Concentration = mean(Concentration),
    .groups = "drop"
  ) %>%
  dplyr::group_by(C_Source) %>%
  dplyr::mutate(
    # recode x as sequential integer based on observed seasons only
    x = as.numeric(factor(Season,
                          levels = c("Spring","Summer","Autumn","Winter")))
  ) %>%
  dplyr::ungroup()

ts_season_line <-
  ec_oc_conc_f14c_season_plot %>%
  dplyr::group_by(C_Source) %>%
  dplyr::summarise(
    x             = list(x),
    Concentration = list(Concentration),
    n_seasons     = dplyr::n(),  # actual number of seasons per source
    .groups       = "drop"
  ) %>%
  dplyr::mutate(
    model = purrr::map2(x, Concentration,
                        ~ mblm::mblm(y ~ x,
                                     data = data.frame(x = .x, y = .y))),
    slope     = purrr::map_dbl(model, ~ coef(.x)[2]),
    intercept = purrr::map_dbl(model, ~ coef(.x)[1]),
    ci        = purrr::map(model, confint),
    slope_lo  = purrr::map_dbl(ci, ~ .x[2, 1]),
    slope_hi  = purrr::map_dbl(ci, ~ .x[2, 2]),
    int_lo    = purrr::map_dbl(ci, ~ .x[1, 1]),
    int_hi    = purrr::map_dbl(ci, ~ .x[1, 2]),
    p_value   = purrr::map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    # use dynamic x_max based on actual seasons present
    x_min      = 1,
    x_max      = n_seasons,
    y_start    = intercept + slope    * x_min,
    y_end      = intercept + slope    * x_max,
    y_lo_start = int_lo    + slope_lo * x_min,
    y_lo_end   = int_lo    + slope_lo * x_max,
    y_hi_start = int_hi    + slope_hi * x_min,
    y_hi_end   = int_hi    + slope_hi * x_max,
    label      = sprintf("%s: β = %.3f, p = %.3f",
                         C_Source, slope, p_value)
  )

# Build ribbon as polygon data
ts_season_ribbon <- 
  ts_season_line %>%
  dplyr::select(C_Source, x_min, x_max,
                y_lo_start, y_lo_end,
                y_hi_start, y_hi_end) %>%
  dplyr::mutate(
    ribbon = pmap(list(x_min, x_max,
                       y_lo_start, y_lo_end,
                       y_hi_start, y_hi_end),
                  ~ data.frame(
                    x    = c(..1, ..2, ..2, ..1),
                    ymin = c(..3, ..4, ..4, ..3),
                    ymax = c(..5, ..6, ..6, ..5)
                  ))
  ) %>%
  dplyr::select(C_Source, ribbon) %>%
  unnest(ribbon)

# ── 2. Annual trend (numeric year) ───────────────────────────────────────────
year_levels <- c("2016", "2017", "2018", "2019", "2020")

year_trend <- 
  ec_oc_conc_f14c_box_plot %>%
  dplyr::mutate(Year_num = as.numeric(as.character(Year))) %>%
  dplyr::group_by(C_Source, Year, Year_num) %>%
  dplyr::summarise(Concentration = mean(Concentration, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(C_Source) %>%
  dplyr::group_modify(~ get_slope_pvalue_ts_mblm(.x, x = "Year_num", y = "Concentration")) %>%
  ungroup()

ec_oc_conc_f14c_year_plot <-
  ec_oc_conc_f14c_box_plot %>%
  dplyr::group_by(Year, C_Source) %>%
  dplyr::summarise(
    Concentration = mean(Concentration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    x    = as.numeric(as.character(Year)),  # numeric for mblm fitting
    Year = factor(Year, levels = year_levels)  # factor for plotting
  )

ts_year_line <-
  ec_oc_conc_f14c_year_plot %>%
  dplyr::group_by(C_Source) %>%
  dplyr::summarise(
    x             = list(x),
    Concentration = list(Concentration),
    n_years     = dplyr::n(),  # actual number of years per source
    .groups       = "drop"
  ) %>%
  dplyr::mutate(
    model = purrr::map2(x, Concentration,
                        ~ mblm::mblm(y ~ x,
                                     data = data.frame(x = .x, y = .y))),
    slope     = purrr::map_dbl(model, ~ coef(.x)[2]),
    intercept = purrr::map_dbl(model, ~ coef(.x)[1]),
    ci        = purrr::map(model, confint),
    slope_lo  = purrr::map_dbl(ci, ~ .x[2, 1]),
    slope_hi  = purrr::map_dbl(ci, ~ .x[2, 2]),
    int_lo    = purrr::map_dbl(ci, ~ .x[1, 1]),
    int_hi    = purrr::map_dbl(ci, ~ .x[1, 2]),
    p_value   = purrr::map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    # use dynamic x_max based on actual years present
    x_min      = 2016,
    x_max      = 2020,
    y_start    = intercept + slope    * x_min,
    y_end      = intercept + slope    * x_max,
    y_lo_start = int_lo    + slope_lo * x_min,
    y_lo_end   = int_lo    + slope_lo * x_max,
    y_hi_start = int_hi    + slope_hi * x_min,
    y_hi_end   = int_hi    + slope_hi * x_max,
    label      = sprintf("%s: β = %.3f, p = %.3f",
                         C_Source, slope, p_value)
  )

# Build ribbon as polygon data
ts_year_ribbon <- 
  ts_year_line %>%
  dplyr::select(C_Source, x_min, x_max,
                y_lo_start, y_lo_end,
                y_hi_start, y_hi_end) %>%
  dplyr::mutate(
    ribbon = pmap(list(x_min, x_max,
                       y_lo_start, y_lo_end,
                       y_hi_start, y_hi_end),
                  ~ data.frame(
                    x    = c(..1, ..2, ..2, ..1),
                    ymin = c(..3, ..4, ..4, ..3),
                    ymax = c(..5, ..6, ..6, ..5)
                  ))
  ) %>%
  dplyr::select(C_Source, ribbon) %>%
  unnest(ribbon)

# ── 3. Year-Season trend (chronological numeric index) ───────────────────────
yearseason_trend <- 
  ec_oc_conc_f14c_box_plot %>%
  dplyr::mutate(YearSeason_num = as.numeric(YearSeason)) %>%
  dplyr::group_by(C_Source, YearSeason, YearSeason_num) %>%
  dplyr::summarise(Concentration = mean(Concentration, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(C_Source) %>%
  dplyr::group_modify(~ get_slope_pvalue_ts_mblm(.x, x = "YearSeason_num", y = "Concentration")) %>%
  dplyr::ungroup()

ts_yearseason_line <- 
  ec_oc_conc_f14c_box_plot %>%
  dplyr::mutate(x = as.numeric(YearSeason)) %>%
  dplyr::group_by(C_Source) %>%
  dplyr::summarise(
    x             = list(x),
    Concentration = list(Concentration),
    .groups       = "drop"
  ) %>%
  dplyr::mutate(
    # fit mblm once per source — pass as data.frame
    model = purrr::map2(x, Concentration,
                        ~ mblm::mblm(y ~ x,
                                     data = data.frame(x = .x,
                                                       y = .y))),
    # slope + intercept
    slope     = purrr::map_dbl(model, ~ coef(.x)[2]),
    intercept = purrr::map_dbl(model, ~ coef(.x)[1]),
    # 95% CI
    ci        = purrr::map(model, confint),
    slope_lo  = purrr::map_dbl(ci, ~ .x[2, 1]),
    slope_hi  = purrr::map_dbl(ci, ~ .x[2, 2]),
    int_lo    = purrr::map_dbl(ci, ~ .x[1, 1]),
    int_hi    = purrr::map_dbl(ci, ~ .x[1, 2]),
    # p-value
    p_value   = purrr::map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    # line endpoints
    x_min      = 1,
    x_max      = 17,
    y_start    = intercept + slope   * x_min,
    y_end      = intercept + slope   * x_max,
    y_lo_start = int_lo    + slope_lo * x_min,
    y_lo_end   = int_lo    + slope_lo * x_max,
    y_hi_start = int_hi    + slope_hi * x_min,
    y_hi_end   = int_hi    + slope_hi * x_max,
    # annotation label
    label      = sprintf("%s: β = %.3f, p = %.3f",
                         C_Source, slope, p_value)
  )

# Build ribbon as polygon data
ts_yearseason_ribbon <- 
  ts_yearseason_line %>%
  dplyr::select(C_Source, x_min, x_max,
                y_lo_start, y_lo_end,
                y_hi_start, y_hi_end) %>%
  dplyr::mutate(
    ribbon = pmap(list(x_min, x_max,
                       y_lo_start, y_lo_end,
                       y_hi_start, y_hi_end),
                  ~ data.frame(
                    x    = c(..1, ..2, ..2, ..1),
                    ymin = c(..3, ..4, ..4, ..3),
                    ymax = c(..5, ..6, ..6, ..5)
                  ))
  ) %>%
  dplyr::select(C_Source, ribbon) %>%
  unnest(ribbon)

# ── 4.1 Plot Year-Season trend with Theil-Sen range ───────────────────────

yearseason_levels <- c(
  "2016 Autumn", "2016 Winter",
  "2017 Spring", "2017 Summer", "2017 Autumn", "2017 Winter",
  "2018 Spring", "2018 Summer", "2018 Autumn", "2018 Winter",
  "2019 Spring", "2019 Summer", "2019 Autumn", "2019 Winter",
  "2020 Spring", "2020 Summer", "2020 Autumn"
)

ts_yearseason_line <- 
  ts_yearseason_line %>%
  dplyr::mutate(
    x_min = yearseason_levels[x_min],  # 1 → "2016 Autumn"
    x_max = yearseason_levels[x_max]   # 17 → "2020 Autumn"
  )
ts_yearseason_ribbon <- 
  ts_yearseason_ribbon %>%
  dplyr::mutate(
    x = yearseason_levels[x]  # maps 1→"2016 Autumn", 17→"2020 Autumn"
  )
head(ts_yearseason_line); head(ts_yearseason_ribbon)

yearseason_labels <- 
  data.frame(
  x     = c(1.5, 4.5, 8.5, 12.5, 16),
  label = c("2016", "2017", "2018", "2019", "2020"),
  C_Source = unique(ec_oc_conc_f14c_box_plot$C_Source)[1]  # top panel only
)

# Temporal, by year by season
ec_oc_bb_ff_yearseason_plot <- 
  ggplot(ec_oc_conc_f14c_box_plot,
         aes(x = YearSeason, y = Concentration,
             group = C_Source)) +
  # year boundaries
  geom_vline(xintercept = c(2.5, 6.5, 10.5, 14.5),
             color = "grey88", linewidth = 0.4) +
  
  # Theil-Sen 95% CI ribbon
  geom_ribbon(data = ts_yearseason_ribbon,
              aes(x = x, ymin = ymin, ymax = ymax,
                  group = C_Source),
              fill = "steelblue", alpha = 0.12,
              inherit.aes = FALSE) +
  # Theil-Sen trend line
  geom_segment(data = ts_yearseason_line,
               aes(x = x_min, xend = x_max,
                   y = y_start, yend = y_end,
                   group = C_Source),
               color = "steelblue", linewidth = 0.9,
               linetype = "solid",
               inherit.aes = FALSE) +
  
  # data points on top
  geom_point(size = 2.5, alpha = 0.9, color = "grey25") +
  
  # annotation
  geom_text(data = ts_yearseason_line,
            aes(x = Inf, y = Inf, label = label,
                group = C_Source),
            hjust = 1.05, vjust = 4.5,
            size = 4.2, color = "steelblue4",
            inherit.aes = FALSE) +
  
  # year labels
  geom_text(data = yearseason_labels,
            aes(x = x, y = Inf, label = label),
            vjust = 1.5, size = 3.8,
            color = "grey55",
            inherit.aes = FALSE) +
  
  facet_wrap(~ C_Source, ncol = 1, strip.position = "right") +
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x),
    expand = expansion(mult = c(0.05, 0.12))  # extra top space for annotation
  ) +
  labs(
    title = "Concentration trends by source, YZ 2016-2020",
    x     = NULL,
    y     = expression("Concentration ("*mu*g~m^-3*")")
  ) +
  theme_bw(base_size = 17) +
  theme(
    plot.background  = element_rect(color = NA),
    panel.spacing    = unit(4, "mm"),
    panel.grid       = element_blank(),
    panel.grid.major.y = element_line(color = "grey94", linewidth = 0.3),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 13, hjust = 0.5),
    strip.placement  = "outside",
    plot.title       = element_text(size = 14),
    legend.position  = "none",           # no legend needed now
    axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )
ec_oc_bb_ff_yearseason_plot

# ── 4.2 Plot Season trend with Theil-Sen range ───────────────────────

season_levels <- 
  c("Spring", "Summer", "Autumn", "Winter")

ts_season_line <- 
  ts_season_line %>%
  dplyr::mutate(
    x_min = season_levels[x_min],  
    x_max = season_levels[x_max]   
  )
ts_season_ribbon <- 
  ts_season_ribbon %>%
  dplyr::mutate(
    x = season_levels[x]  
  )
head(ts_season_line); head(ts_season_ribbon)

# Temporal, by year by season
ec_oc_bb_ff_season_plot <-
  ggplot(ec_oc_conc_f14c_season_plot,
         aes(x = Season, y = Concentration,
             group = C_Source)) +
  # Theil-Sen 95% CI ribbon
  geom_ribbon(data = ts_season_ribbon,
              aes(x = x, ymin = ymin, ymax = ymax,
                  group = C_Source),
              fill = "steelblue", alpha = 0.12,
              inherit.aes = FALSE) +
  # Theil-Sen trend line
  geom_segment(data = ts_season_line,
               aes(x = x_min, xend = x_max,
                   y = y_start, yend = y_end,
                   group = C_Source),
               color = "steelblue", linewidth = 0.9,
               linetype = "solid",
               inherit.aes = FALSE) +
  
  # data points on top
  geom_point(size = 2.5, alpha = 0.9, color = "grey25") +
  
  # annotation
  geom_text(data = ts_season_line,
            aes(x = Inf, y = Inf, label = label,
                group = C_Source),
            hjust = 1.05, vjust = 3.5,
            size = 4.2, color = "steelblue4",
            inherit.aes = FALSE) +
  
  facet_wrap(~ C_Source, ncol = 1, strip.position = "right") +
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x),
    expand = expansion(mult = c(0.05, 0.12))  # extra top space for annotation
  ) +
  labs(
    title = "Concentration trends by source by Season, YZ 2016-2020",
    x     = NULL,
    y     = expression("Concentration ("*mu*g~m^-3*")")
  ) +
  theme_bw(base_size = 17) +
  theme(
    plot.background  = element_rect(color = NA),
    panel.spacing    = unit(4, "mm"),
    panel.grid       = element_blank(),
    panel.grid.major.y = element_line(color = "grey94", linewidth = 0.3),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 13, hjust = 0.5),
    strip.placement  = "outside",
    plot.title       = element_text(size = 14),
    legend.position  = "none",           # no legend needed now
    axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )
ec_oc_bb_ff_season_plot

# ── 4.3 Plot Year trend with Theil-Sen range ───────────────────────

ts_year_line <- 
  ts_year_line %>%
  dplyr::mutate(
    x_min = year_levels[x_min - 2015],  
    x_max = year_levels[x_max - 2015]   
  )
ts_year_ribbon <- 
  ts_year_ribbon %>%
  dplyr::mutate(
    x = year_levels[x - 2015]  
  )
head(ts_year_line); head(ts_year_ribbon)

# Temporal, by year by year
ec_oc_bb_ff_year_plot <-
  ggplot(ec_oc_conc_f14c_year_plot,
         aes(x = Year, y = Concentration,
             group = C_Source)) +
  # Theil-Sen 95% CI ribbon
  geom_ribbon(data = ts_year_ribbon,
              aes(x = x, ymin = ymin, ymax = ymax,
                  group = C_Source),
              fill = "steelblue", alpha = 0.12,
              inherit.aes = FALSE) +
  # Theil-Sen trend line
  geom_segment(data = ts_year_line,
               aes(x = x_min, xend = x_max,
                   y = y_start, yend = y_end,
                   group = C_Source),
               color = "steelblue", linewidth = 0.9,
               linetype = "solid",
               inherit.aes = FALSE) +
  
  # data points on top
  geom_point(size = 2.5, alpha = 0.9, color = "grey25") +
  
  # annotation
  geom_text(data = ts_year_line,
            aes(x = Inf, y = Inf, label = label,
                group = C_Source),
            hjust = 1.05, vjust = 3.5,
            size = 4.2, color = "steelblue4",
            inherit.aes = FALSE) +
  
  facet_wrap(~ C_Source, ncol = 1, strip.position = "right") +
  scale_y_continuous(
    labels = function(x) sprintf("%.1f", x),
    expand = expansion(mult = c(0.05, 0.12))  # extra top space for annotation
  ) +
  labs(
    title = "Concentration trends by source by year, YZ 2016-2020",
    x     = NULL,
    y     = expression("Concentration ("*mu*g~m^-3*")")
  ) +
  theme_bw(base_size = 17) +
  theme(
    plot.background  = element_rect(color = NA),
    panel.spacing    = unit(4, "mm"),
    panel.grid       = element_blank(),
    panel.grid.major.y = element_line(color = "grey94", linewidth = 0.3),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 13, hjust = 0.5),
    strip.placement  = "outside",
    plot.title       = element_text(size = 14),
    legend.position  = "none",           # no legend needed now
    axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )
ec_oc_bb_ff_year_plot

