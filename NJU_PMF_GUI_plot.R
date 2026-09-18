
setwd("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/NJ_NJUdata_PMF/results")
getwd()
data.dir <- "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/NJ_NJUdata_PMF/results"

#### Libraries to use #### 
library(tidyr)
library(dplyr)
library(plyr)
library(base)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggsci)
library(ggpattern)
library(ggthemes)
library(scales)

#### Functions used ####

#### Transfer txt lines into dataframe  ####
line_to_df <- function(lines_txt, nrow, ncol){
  # split the lines into individual elemenconc
  lines_txt = unlist(strsplit(lines_txt, "\\s+"))
  
  # create a matrix to store the individual elemenconc and convert to data.frame
  lines_df = data.frame(
    matrix(lines_txt, 
           nrow = nrow, 
           ncol = ncol, 
           byrow = T))
  
  return(lines_df)
}

#### Identify segments that are not spaces
non_space_segments <- function(line_to_replace) {
  # Split the line into segments (numbers and spaces)
  # (?<=...): a lookbehind assertion; (?=...): a lookahead assertion.
  # a space character (\\s), a non-space character (\\S)
  # together, splits the string exactly at the positions where a space transitions into a non-space or a non-space transitions into a space
  segments <- unlist(strsplit(line_to_replace, 
                              "(?<=\\s)(?=\\S)|(?<=\\S)(?=\\s)", 
                              perl = TRUE))
  
  # Identify segments that are not spaces
  non_space_segments <- sapply(segments, 
                               function(x) 
                                 !grepl("^\\s+$", x))
  
  return(non_space_segments)
}

disp_analysis = function(disp_output){
  # Extract lines with values
  disp_output_noNull = disp_output[! grepl('^\\s*$', disp_output)]
  
  # Get the second line in DISPres1.txt file to get the two numbers evaluating DISP performance
  DISPres1_sum = disp_output_noNull[1]
  
  # Split the string and remove empty elements
  disp.numbers <- strsplit(DISPres1_sum, "\\s+")
  disp.numbers <- disp.numbers[[1]][disp.numbers[[1]] != ""]
  
  # Convert to numeric
  disp.numbers <- as.numeric(disp.numbers)
  
  # Extract the two numbers
  disp.error.code <- disp.numbers[1]
  disp.qdrop <- disp.numbers[2]
  
  # Extract lines with values
  disp_output_matrix = disp_output_noNull[6:length(disp_output_noNull)]
  matrix_line = length(disp_output_matrix)/4
  
  # Extract up and down CI from DISP
  disp_down = disp_output_matrix[1:(matrix_line-1)]
  disp_up = disp_output_matrix[(matrix_line+1):(matrix_line*2-1)]
  
  # Identify segments that are not spaces
  info_segments = non_space_segments(disp_down[1])
  
  # Estimate the factor number
  factor.No <- sum(info_segments)
  
  Factor.serial = paste0("Factor", 1:factor.No)
  
  # lines to dataframe
  disp_down_df = line_to_df(disp_down, 
                            matrix_line - 1, 
                            factor.No + 1)
  disp_down_df = mutate_all(disp_down_df, as.numeric)
  colnames(disp_down_df)[1] = "Species"
  colnames(disp_down_df)[2:(factor.No+1)] = Factor.serial
  
  disp_up_df = line_to_df(disp_up, 
                          matrix_line - 1, 
                          factor.No + 1)
  disp_up_df = mutate_all(disp_up_df, as.numeric)
  colnames(disp_up_df)[1] = "Species"
  colnames(disp_up_df)[2:(factor.No+1)] = Factor.serial
  
  return(list(disp.error.code, disp.qdrop,
              disp_down = disp_down_df, 
              disp_up = disp_up_df))
}

#### Convert concentration contribution to fraction/percent contribution

conc_percent_contri = function(conc_contribution){
  
  # get the percent contribution
  all_species = data.frame(Species = conc_contribution$Species)
  percent_value = 
    signif(
      conc_contribution[, -1] *100 / 
        rowSums(conc_contribution[, -1]), 
      2)
  percent_contribution = cbind(all_species, 
                               percent_value)
  
  return(percent_contribution)
}

#### Source profile figure, Transfer data (percent contribution) to logged value, and match the scale used in logged normalized contribution
# assuming the value is in percentage (0-100) and needs to be scaled between log_min (log(1e-05)) to log_max (e.g., log(1e-01))
trans_log <- function(value, log_max, log_min) {
  log_trans = log(value + 1) / 
    log(101) * 
    (log_max - log_min) + 
    log_min
  
  return(log_trans)
}

#### Define the function to map percent (0-100) to exponent values of a given range
map_percent_to_exponent <- 
  function(percent_values, exponent_values, percent_to_convert) {
    # Interpolate in log10 scale for precise control over exponential mapping
    log_exponent <- 
      approx(percent_values, 
             log10(exponent_values), 
             xout = percent_to_convert, 
             rule = 2)$y
    convert_percent <- 10^log_exponent
    
    return(convert_percent)
  }
#### for bar start from a value less than 0
# https://stackoverflow.com/questions/22295253/force-bars-to-start-from-a-lower-value-than-0-in-ggplot-geom-bar-in-r
mylog_trans <- function(base=exp(1), from=0) {
  trans <- function(x) log(x, base)-from
  inv <- function(x) base^(x+from)
  trans_new("mylog", trans, inv, log_breaks(base=base), 
            domain = c(base^from, Inf))
}

# pairs plot 1, correlation calculation
panel.corr <- function(x, y, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  # Calculate correlation and p-value
  test <- cor.test(x, y)
  r <- round(test$estimate, digits=3)
  p_value <- round(test$p.value, digits=5)
  
  # Create correlation text and calculate size
  txt <- paste0("R = ", r)
  cex.cor <- 2.5/strwidth(txt)
  
  # Determine color based on significance of p-value
  if (p_value < 0.05) {
    col <- "red"
  } else {
    col <- "black"
  }
  
  # Place the correlation text on the plot
  text(0.5, 0.7, txt, cex = cex.cor * abs(r), col = col)
  
  # Create p-value text and calculate size
  txt_p <- paste0("p = ", round(p_value, 3))
  cex.p <- 1
  
  # Place the p-value text on the plot below the correlation
  text(0.5, 0.3, txt_p, cex = cex.p, col = col)
  
  # Return correlation and p-value
  return(list(correlation = r, p_value = p_value))
}

# pairs plot 2, upper or lower panel
panel.scatter <- function(x, y, ...){
  points(x, y, 
         pch=point_char, 
         size=point_size,
         col=adjustcolor(point_col, 
                         alpha.f=point_alpha))
}

# !!! Important for pairs plot 1&2 is, pairs function provides only the x and y arguments to the upper.panel and lower.panel functions
# !!! For other setting such as pch and col, take ... (ellipsis), and explicitly state them outside the function 


#### superscript & subscript setting
format_variable <- function(variable) {
  variable <- gsub("ClIon", "Cl\u207B", variable)
  variable <- gsub("NO3Ion", "NO\u2083\u207B", variable)
  variable <- gsub("SO4Ion", "SO\u2084\u00B2\u207B", variable)
  variable <- gsub("CO3Ion", "CO\u2083\u00B2\u207B", variable)
  variable <- gsub("NH4Ion", "NH\u2084\u207A", variable)
  variable <- gsub("NaIon", "Na\u207A", variable)
  variable <- gsub("KIon", "K\u207A", variable)
  variable <- gsub("PM25", "PM\u2082.\u2085", variable)
  variable <- gsub("PM2.5", "PM\u2082.\u2085", variable)
  variable <- gsub("m3", "m\u00B3", variable)
  return(variable)
}

#### themes used for plotting
theme_text_speciesName = theme(axis.title.x = element_text(color="grey25", size = 12,
                                                           family = "Arial Unicode MS", 
                                                           vjust=0), 
                               axis.title.y = element_text(color="grey25", size = 12,
                                                           family = "Arial Unicode MS", 
                                                           vjust=1),
                               axis.text.x = element_text(color="grey25", size = 10.5,
                                                          family = "Arial Unicode MS",
                                                          angle = 90, hjust = 0, vjust = 0.3),
                               axis.text.y = element_text(color="grey25", size = 10.5,
                                                          family = "Arial Unicode MS",
                                                          angle = 0, hjust = 0.5))

theme_ts = theme(axis.title.x = element_text(color="grey25", size = 12,
                                             family = "Arial Unicode MS", 
                                             vjust=0), 
                 axis.title.y = element_text(color="grey25", size = 12,
                                             family = "Arial Unicode MS", 
                                             vjust=1),
                 axis.text.x = element_text(color="grey25", size = 10.5,
                                            family = "Arial Unicode MS",
                                            angle = 0, hjust = 0, vjust = 0.3),
                 axis.text.y = element_text(color="grey25", size = 10.5,
                                            family = "Arial Unicode MS",
                                            angle = 0, hjust = 0.5))

#### Base model data ####

# original concentration
org_file_path = "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/NJ_NJUdata_PMF"
org_conc = 
  read.csv(
    file.path(org_file_path, 
            
           "NJ_NJUdata_no_extremes_4.5unc_intp_DN_conc.csv")) # NJUdnWeakUnc5F
           # "NJ_NJUdata_no_extremes_4.5unc_DN_manualReplaceAllExe_conc.csv")) # NJUdnAllRepUnc5F
           # "NJ_NJUdata_no_extremes_4.5unc_intp_DN_manualExeReplace_conc.csv")) # NJUdnZnMWeakUnc5, NJUdnZnExeUnc5F
          # "NJ_NJUdata_no_extremes_4.5unc_DN_ReplaceAllExe&lessExe_conc.csv")) # NJUdnLessRepWeakPUnc5F
org_conc$X = NULL
org_conc$Date = as.Date(org_conc$Date) #, format = "%m/%d/%y"
head(org_conc)

# data.pre = "NJUdnLessRepWeakPUnc5F"
# data.pre = "NJUdnZnMWeakUnc5"
# data.pre = "NJUdnZnExeUnc5F"
# data.pre = "NJUdnAllRepUnc5F"

data.pre = "NJUdnWeakUnc5F"
# data.pre = "NJnoExeUncPweak05F"

species.no = 27
factor.No = 7 # 6, 7, 8
date.no = 260
Factor.serial = paste0("Factor", 1:factor.No)
  
base.name = paste0(data.pre, factor.No, "_base.xlsx")
disp.name = paste0(data.pre, factor.No, "_DISPres1.txt")

# extract overall concentration contribution & %ofSpecies from base result
base_profile = read_excel(base.name, sheet = "Profiles")
base_conc = base_profile[3:(2+species.no), 2:(2+factor.No)]
base_frac = base_profile[(7+species.no):(6+2*species.no), 2:(2+factor.No)]
names(base_conc) = names(base_frac) = 
  append("Species", paste0("Factor", 1:factor.No))

base_conc$Species <- gsub('""\\"|""|"', "", base_conc$Species)
base_frac$Species = base_conc$Species

# extract daily concentration contribution from base result
base_daily = read_excel(base.name, sheet = "Contributions")
base_daily_conc = 
  base_daily[(7+date.no):(6+2*date.no), 2:(2+factor.No)] #2:(2+factor.No), 3:(3+factor.No)
names(base_daily_conc) = 
  append("Date", Factor.serial)

# convert classes of columns
base_daily_conc$Date = 
  as.Date(base_daily_conc$Date, format = "%m/%d/%y") #"%Y/%m/%d"
base_daily_conc[, 2:ncol(base_daily_conc)] =
  mutate_all(base_daily_conc[, 2:ncol(base_daily_conc)], as.numeric)

# check the basic info
sapply(base_daily_conc, class)
head(base_daily_conc)
dim(base_daily_conc)

# in case of any removed dates, subset the date org_conc
org_conc = subset(org_conc, 
                  Date %in% base_daily_conc$Date)

# in case of any unused species, subset the date org_conc
cols_use_pmf = append("Date", base_conc$Species)
org_conc <- org_conc[, cols_use_pmf]

# extract daily normalized contribution from base result
base_nm_contri = 
  base_daily[3:(2+date.no), 2:(2+factor.No)] #2:(2+factor.No), 3:(3+factor.No)
names(base_nm_contri) = 
  append("Date", Factor.serial)

# convert classes of columns
base_nm_contri$Date = 
  as.Date(base_nm_contri$Date, format = "%m/%d/%y") #"%Y/%m/%d"
base_nm_contri[, 2:ncol(base_nm_contri)] =
  mutate_all(base_nm_contri[, 2:ncol(base_nm_contri)], as.numeric)

# check the basic info
sapply(base_nm_contri, class)
head(base_nm_contri)
dim(base_nm_contri)

# data for plotting, concentration, fraction, and time-series daily from base
base_conc_plot = gather(base_conc,
                        "Factor", 
                        "Concentration", 
                        -Species)

base_percent_plot = gather(base_frac,
                           "Factor", 
                           "Percent", 
                           -Species)

base_ts_conc_long <-
  base_daily_conc %>% 
  pivot_longer(
    cols = Factor.serial[1]:Factor.serial[factor.No],
    names_to = c("Factor"),
    # names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "Concentration"
  )

#### DISP data ####

# extract overall %ofSpecies from DISP result
disp_file = readLines(disp.name)
disp_down_conc = disp_analysis(disp_file)$disp_down
disp_up_conc = disp_analysis(disp_file)$disp_up
disp_down_conc$Species = disp_up_conc$Species = base_conc$Species

# mean concentration from DISP
disp_mean_conc = (disp_down_conc[, 2:(factor.No+1)] + disp_up_conc[, 2:(factor.No+1)])/2
disp_mean_conc$Species = disp_down_conc$Species

# relocate the column
disp_mean_conc = relocate(disp_mean_conc, Species, .before = Factor1)

# convert the df for plotting
disp_down_conc_plot = gather(disp_down_conc,
                             "Factor", 
                             "disp_conc_down", 
                             -Species)
disp_up_conc_plot = gather(disp_up_conc,
                           "Factor", 
                           "disp_conc_up", 
                           -Species)
disp_mean_conc_plot = gather(disp_mean_conc,
                             "Factor", 
                             "disp_conc_mean", 
                             -Species)

# DISP results - overall validation of results
disp.error.code = disp_analysis(disp_file)[[1]]
disp.qdrop = disp_analysis(disp_file)[[2]]  

#### Prepare data for plotting ####

species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/Progress data/CSN_Species_class_sub.csv")
species_class$Species[nrow(species_class)] = "PM2.5"

# merge datasets for source profile plotting
base_disp_conc_to_merge <- 
  list(base_conc_plot, base_percent_plot, 
       disp_down_conc_plot, disp_mean_conc_plot, disp_up_conc_plot, 
       species_class, main_source)

conc_percent_bsDisp <- 
  Reduce(function(x, y) 
    merge(x, y), 
    base_disp_conc_to_merge)

# Convert 0 to 1e-10 for columns to be used for y-axis, there is log transfer later
conc_percent_bsDisp <- 
  conc_percent_bsDisp %>%
  mutate(across(Concentration:Percent.up, 
                ~replace(., . == 0, 1e-10)))

# Change data class to numeric
sapply(conc_percent_bsDisp, class)
columns_to_convert = 
  c("Concentration", "Percent")
conc_percent_bsDisp[columns_to_convert] =
  lapply(conc_percent_bsDisp[columns_to_convert], as.numeric)

##### Convert percent values to make the scale pattern similar to log concentration
# set the Percent == 0 to a low value before log
conc_percent_bsDisp$Percent[conc_percent_bsDisp$Percent == 0] = 1e-05

# define the breakpoints and corresponding values in percent and exponent values
percent_values <- c(100, 80, 60, 40, 20, 0)
exponent_values <- c(1e-00, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05)
# exponent_values <- c(1e+01, 1e-00, 1e-01, 1e-02, 1e-03, 1e-04)

# get the mapped percent
conc_percent_bsDisp$MappedPercent  = 
  map_percent_to_exponent(percent_values, 
                          exponent_values, 
                          conc_percent_bsDisp$Percent)

#### Plotting: source profile #### 

# source_profile
source_profile <-
  ggplot(conc_percent_bsDisp,
         aes(x = reorder(Species, sequence), 
             group = Factor_source)) +
  # Bar plot for Concentration
  geom_bar(aes(y = Concentration, fill = Factor_source), 
           stat = "identity", width = 0.6, alpha = 0.8) +
  # Point plot for transformed Percent
  geom_point(aes(y = MappedPercent, group =1), 
             color = "grey25", shape = 15, size = 1.5) +
  geom_point(aes(y = disp_conc_mean), 
             color = "grey25", shape = 1, size = 2) +
  geom_errorbar(aes(ymin = disp_conc_down, 
                    ymax = disp_conc_up), 
                width = 0.3, color = "grey25") +
  facet_grid(Factor ~ ., switch = "y") +
  scale_y_continuous(
    name = format_variable("Concentration µg/m3"),
    breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e-00),
    labels = c(expression(10^"-5"), expression(10^"-4"), 
               expression(10^"-3"), expression(10^"-2"), 
               expression(10^"-1"), expression(10^"0")),
    sec.axis = sec_axis(
      # set the trans the same as the main y axis
      trans = ~., 
      breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e+00),
      name = "Explained Variance %",
      labels = c(0, 20, 40, 60, 80, 100)
    ),
    trans = mylog_trans(base=10, from=-5),
    limits = c(1E-5, max(conc_percent_bsDisp$Concentration))) +
  ggtitle(paste0(paste0(data.pre, factor.No), # "\n",
                 ", Error.Code = ", disp.error.code, 
                 ", DISP.Qdrop = ", disp.qdrop)) + 
  scale_fill_npg() +
  xlab(format_variable("PM25 Species")) +
  ylab(format_variable("Concentration µg/m3")) +
  scale_x_discrete(labels = function(x) format_variable(x)) +
  theme_bw() +
  theme_text_speciesName +
  theme(
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
    strip.background = element_blank(), strip.text = element_blank(),
    legend.position = "none"
  )

source_profile

#### Plotting: time-series Concentration #### 

####  Time-series, daily
base_ts_conc_long$Concentration = as.numeric(base_ts_conc_long$Concentration)

# set gap in figure if no data for >15 days
base_ts_conc_long <- 
  base_ts_conc_long %>%
  arrange(Date) %>%
  mutate(gap = c(0, diff(as.numeric(Date))) > 15, # calculate gaps, convert Date to numeric for diff
         group = cumsum(gap)) # Cumulative sum to create a new group after each gap

daily_conc_plot <-
  ggplot(base_ts_conc_long, 
         aes(x = Date, y = Concentration, 
             group = Factor, color = Factor)) + # group = 1
  geom_line(aes(group = group), linewidth = 0.3, alpha = 0.8) +
  geom_point(size = 1.2) +
  labs(x = "Date", y = format_variable("Concentration µg/m3")) +
  facet_grid(Factor ~., scales = "free_y") +
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n = 3)) + # y start from 0, and number of breaks, pretty function
  scale_color_npg() +
  theme_base() +
  theme(
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
    strip.background = element_blank(), strip.text = element_blank(),
    legend.position = "none"
  )

# daily_conc_plot

####### annual & monthly
base_ts_conc_ym = base_ts_conc_long

base_ts_conc_ym$year = year(base_ts_conc_ym$Date)
base_ts_conc_ym$month = month(base_ts_conc_ym$Date)
summary(base_ts_conc_ym)

#### annlual
base_ts_conc_year =
  base_ts_conc_ym %>%
  dplyr::group_by(Factor, year) %>%
  dplyr::summarise(
    conc_med = quantile(Concentration, 0.5),  
    conc_up = quantile(Concentration, 0.95), 
    conc_down = quantile(Concentration, 0.05),  
    .groups = 'drop'  # drops the grouping so the result is ungrouped
  )

head(base_ts_conc_year)

year_conc_plot <-
  ggplot(base_ts_conc_year, 
         aes(x = year, y = conc_med, 
             group = Factor, color = Factor)) + # group = 1
  geom_line(aes(group = Factor), linewidth = 0.3, alpha = 0.8) +
  geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                width = 0.15) +
  geom_point(size = 1.2) +
  # the preset format_variable function, for sub-/super- scripts
  labs(x = "Year", y = format_variable("Concentration µg/m3")) +
  facet_grid(Factor ~., scales = "free_y") +
  # y start from 0, and number of breaks, pretty function
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n = 3)) + 
  scale_color_npg() + # ggsci
  theme_base() +
  theme(
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
    strip.background = element_blank(), strip.text = element_blank(),
    legend.position = "none"
  )

#### monthly
# group_by in dplyr could be more robust than ddply in plyr
base_ts_conc_month =
  base_ts_conc_ym %>%
  dplyr::group_by(Factor, month) %>%
  dplyr::summarise(
    conc_med = quantile(Concentration, 0.5),  
    conc_up = quantile(Concentration, 0.95), 
    conc_down = quantile(Concentration, 0.05),  
    # drops the grouping so the result is ungrouped
    .groups = 'drop'  
  )

# set gap in figure if no data for a given month
base_ts_conc_month = 
  base_ts_conc_month %>%
  arrange(month) %>%
  mutate(gap = c(0, diff(as.numeric(month))) > 1, # calculate gaps
         group = cumsum(gap)) # cumulative sum to create a new group after each gap

head(base_ts_conc_month)

month_conc_plot <-
  ggplot(base_ts_conc_month, 
         aes(x = month, y = conc_med, 
             group = Factor, color = Factor)) + # group = 1
  geom_line(aes(group = group), linewidth = 0.3, alpha = 0.8) +
  geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                width = 0.15) +
  geom_point(size = 1.2) +
  # the preset format_variable function, for sub-/super- scripts
  labs(x = "Month", y = format_variable("Concentration µg/m3")) +
  facet_grid(Factor ~., scales = "free_y") +
  # pretty function, y start from 0, and 3 breaks
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n = 3)) + 
  # pretty function, x start from 1, and 12 breaks
  scale_x_continuous(limits = c(1, 12), 
                     breaks = function(x) pretty(x, n = 12)) + 
  scale_color_npg() + # ggsci
  theme_base() +
  theme(
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
    strip.background = element_blank(), strip.text = element_blank(),
    legend.position = "none"
  )
# month_conc_plot


##### year_month

# estimate the concentration by year-month group
base_ts_conc_ym_conc = 
  base_ts_conc_ym %>%
  dplyr::group_by(Factor, year, month) %>%
  dplyr::summarise(
    conc_med = quantile(Concentration, 0.5),  
    conc_up = quantile(Concentration, 0.95), 
    conc_down = quantile(Concentration, 0.05), 
    # drops the grouping so the result is ungrouped
    .groups = 'drop')

# get the year_month date
base_ts_conc_ym_conc = 
  base_ts_conc_ym_conc %>%
  mutate(year_month = 
           as.Date(
             paste(year, month, "01", sep = "-")))
head(base_ts_conc_ym_conc)

# plot the change
ym_conc_plot <-
  ggplot(base_ts_conc_ym_conc, 
         aes(x = year_month, 
             y = conc_med, 
             group = Factor, color = Factor)) + # group = 1
  geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                width = 0.15) +
  geom_point(size = 1.2) +
  # the preset format_variable function, for sub-/super- scripts
  labs(x = "Date", y = format_variable("Concentration µg/m3")) +
  facet_grid(Factor ~., scales = "free_y") +
  # pretty function, y start from 0, and 3 breaks
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n = 3)) + 
  # Setting breaks every month  
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 month") + 
  scale_color_npg() + # ggsci
  theme_base() +
  theme(
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
    strip.background = element_blank(), strip.text = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  )

ym_conc_plot

#### Plotting: time-series Fraction #### 

# mean estimate year-month factor specific concentration contribution
base_ts_conc_ym_sum = 
  base_ts_conc_ym_conc %>%
  dplyr::group_by(year_month) %>%
  dplyr::summarise(
    conc_sum = sum(conc_med),  
    # drops the grouping so the result is ungrouped
    .groups = 'drop')
head(base_ts_conc_ym_sum)
summary(base_ts_conc_ym_sum)

# get the fraction of factor specific  contribution
base_ts_conc_ym_frac = merge(base_ts_conc_ym_conc, base_ts_conc_ym_sum)
base_ts_conc_ym_frac$frac_med = 
  base_ts_conc_ym_frac$conc_med/base_ts_conc_ym_frac$conc_sum * 100
base_ts_conc_ym_frac$frac_up = 
  base_ts_conc_ym_frac$conc_up/base_ts_conc_ym_frac$conc_sum * 100
base_ts_conc_ym_frac$frac_down = 
  base_ts_conc_ym_frac$conc_down/base_ts_conc_ym_frac$conc_sum * 100
head(base_ts_conc_ym_frac)

# plot the change
ym_frac_plot <-
  ggplot(base_ts_conc_ym_frac, 
         aes(x = year_month, 
             y = frac_med, 
             group = Factor, color = Factor)) + # group = 1
  geom_errorbar(aes(ymin = frac_down, ymax = frac_up),
                width = 0.15) +
  geom_point(size = 1.2) +
  # the preset format_variable function, for sub-/super- scripts
  labs(x = "Date", y = format_variable("Fraction %")) +
  facet_grid(Factor ~., scales = "free_y") +
  # pretty function, y start from 0, and 3 breaks
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n = 3)) + 
  # Setting breaks every month  
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 month") + 
  scale_color_npg() + # ggsci
  theme_base() +
  theme(
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
    strip.background = element_blank(), strip.text = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  )

ym_frac_plot

#### Plotting: G-space plots #### 

# Use captured pch and col outside the function 
point_char = 19
point_size = 1
point_alpha = 0.3
point_col = "skyblue"
base_contribute_pair = base_nm_contri[, 2:(factor.No+1)]

# Pair plotting
pdf(paste0(data.pre, factor.No, "_G-space.pdf"))

# par(oma = c(4, 4, 6, 10), mar = c(4, 4, 2, 2)) 
pairs(base_contribute_pair, 
      upper.panel = panel.scatter,
      lower.panel = panel.corr)

dev.off()


#### Plotting: predictions vs. observations #### 

# daily species concentration observations
obs_daily_species_long <-
  org_conc %>% 
  pivot_longer(
    cols = Al:PM2.5,
    names_to = c("Species"),
    # names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "Concentration"
  )
head(obs_daily_species_long)

# initialize an empty matrix to store the species-specific daily concentraion contributions 
daily_contribution_matrix <- 
  matrix(0, nrow = date.no, 
         ncol = species.no )

# column names for the matrix are species names
colnames(daily_contribution_matrix) <- base_conc$Species # to be replaced in later process

# Loop through each species to calculate daily contributions
# multiply and sum the contributions
base_ts_factors <- base_daily_conc[-1]

for(i in 1:nrow(base_conc)) {
  factor_species_conc <- as.numeric(base_conc[i, -1])
  
  daily_contribution_matrix[, i] <- 
    rowSums(t(t(base_ts_factors) * 
                factor_species_conc))
}

predict_daily_species_conc <- 
  data.frame(Date = base_daily_conc$Date, 
             daily_contribution_matrix)

# relocate dataset, Al first, PM last
predict_daily_species_conc = 
  relocate(predict_daily_species_conc, Al, .after = Date)

# use pivot_longer to transform dataset
predict_daily_species_long = 
  predict_daily_species_conc %>%
  pivot_longer(
    cols = Al:PM2.5,
    names_to = c("Species"),
    values_to = "Concentration"
  )

# in case some species in original data are not used
obs_daily_species_long = 
  subset(obs_daily_species_long, 
         Species %in% predict_daily_species_long$Species)

# reorder dataset for matching
predict_daily_species_long =
  predict_daily_species_long[with(
    predict_daily_species_long, order(Date, Species)), ]
obs_daily_species_long =
  obs_daily_species_long[with(
    obs_daily_species_long, order(Date, Species)), ]

# assign name of datasets and combine
predict_daily_species_long$Data = "Predictions"
obs_daily_species_long$Data = "Observations"

rmse <- function(actual, predicted) {
  sqrt(mean((predicted - actual) ^ 2))
}

pred_obs_species_wide =
  cbind(select(predict_daily_species_long, -Data), 
        select(obs_daily_species_long, -c("Data", "Date", "Species")))
names(pred_obs_species_wide)[3:4] = 
  c("conc_predict", "conc_obs")

pred_obs_compare  =
  pred_obs_species_wide %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    RMSE = rmse(conc_predict, conc_obs),
    R2 = 1 - sum((conc_obs - conc_predict)^2) / 
      sum((conc_obs - mean(conc_obs))^2),
    cor_pearson = cor(conc_obs, conc_predict, method = "pearson"),
    cor_spearman = cor(conc_obs, conc_predict, method = "spearman"),
    mean_obs = mean(conc_obs), 
    sd_obs = sd(conc_obs), 
    mean_pmf = mean(conc_predict), 
    sd_pmf = sd(conc_predict))

### combine data for time-series plotting
pred_obs_species_conc = 
  rbind(predict_daily_species_long, obs_daily_species_long)

### time-series
# set gap in figure if no data for >7 days
pred_obs_species_conc <- 
  pred_obs_species_conc %>%
  arrange(Date) %>%
  mutate(gap = c(0, diff(as.numeric(Date))) > 7, # calculate gaps, convert Date to numeric for diff
         group = cumsum(gap)) # Cumulative sum to create a new group after each gap

# positions of ggtext
pred_obs_species_position <-
  ddply(pred_obs_species_conc, .(Species),
        summarise,
        y_position = quantile(Concentration, 0.99),
        x_position = median(Date))

# info to list in ggtext
pred_obs_species_med <-
  ddply(subset(pred_obs_species_conc,
               Data == "Observations"), 
        .(Species),
        summarise,
        median_conc = signif(median(Concentration), 2),
        mean_conc = signif(mean(Concentration), 2))
pred_obs_species_med = 
  merge(pred_obs_species_med, pred_obs_compare)
pred_obs_species_med$Species_conc =
  paste0(pred_obs_species_med$Species,
         ": Obs-median=", pred_obs_species_med$median_conc,
         ", RMSE=", signif(pred_obs_species_med$RMSE, 2),
         ", R^2=", signif(pred_obs_species_med$R2, 2))

# combine ggtext position and information data
pred_obs_species_position = 
  join(pred_obs_species_position, pred_obs_species_med)

# plotting
pred_obs_species_conc_plot <-
  ggplot(pred_obs_species_conc, 
         aes(x = Date, y = Concentration, 
             group = group, color = Data)) + # group = 1
  geom_line(linewidth = 0.15, alpha = 0.4) +
  geom_point(size = 0.2, alpha = 0.7) +
  labs(x = "Date", y = format_variable("Concentration µg/m3")) +
  facet_wrap(Species ~., scales = "free_y", ncol = 4) + # ncol, only for facet_wrap, not facet_grid
  scale_y_continuous(limits = c(0, NA), 
                     breaks = function(x) pretty(x, n = 3)) +  # reduce the number of breaks
  scale_color_manual(values = c("#ff7f00", "#377eb8")) + # c("steelblue", "brown2"), c("#377eb8", "#ff7f00")
  geom_text(data = pred_obs_species_position, size = 3,
            aes(x = x_position, y = y_position, label = Species_conc), 
            inherit.aes = FALSE) + 
  theme_base() +
  theme(
    plot.margin = margin(r = 5, l = 3, t = 3, unit = "mm"), # more space on right & top
    panel.grid = element_line(colour = "white"),
    plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
    strip.background = element_blank(), strip.text = element_blank(),
    legend.position = "bottom", legend.background = element_blank(),
    legend.title = element_text(size = 0),
    axis.text = element_text(size = 11, color = "grey25")
  )
# pred_obs_species_conc_plot

##### predictions vs. observations, scatter
species_conc_pred_vs_obs = 
  pred_obs_species_conc %>%
  pivot_wider(
    names_from = Data, 
    values_from = Concentration)

species_pred_vs_obs_position =
  ddply(species_conc_pred_vs_obs,
        .(Species),
        summarise,
        x_position = max(`Observations`)*0.85,
        y_position = max(`Predictions`)*0.9)

species_pred_vs_obs_plot <-
  ggplot(species_conc_pred_vs_obs,
         aes(x = `Observations`,
             y = `Predictions`)) +
  geom_point(size = 1, color = "grey35", alpha = 0.8) +
  geom_abline(slope=1, intercept=0, color = "red") +
  facet_wrap(~ Species, scales = "free", ncol = 5) +
  geom_text(data = species_pred_vs_obs_position, size = 4,
            aes(x = x_position, y = y_position, label = Species), 
            inherit.aes = FALSE) + 
  labs(x = format_variable("Observations µg/m3"), 
       y = format_variable("PMF_Predictions µg/m3")) +
  theme_base() +
  theme(
    plot.margin = margin(r = 5, l = 3, t = 3, b = 3, unit = "mm"), # more space on figure edges
    strip.background = element_blank(), strip.text = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12))
# species_pred_vs_obs_plot

# check PM2.5 comparison
PM_conc_pred_vs_obs = subset(species_conc_pred_vs_obs, Species == "PM2.5")
lm_pred_obs = lm(PM_conc_pred_vs_obs$Observations~ PM_conc_pred_vs_obs$Predictions)
summary(lm_pred_obs)

plot(PM_conc_pred_vs_obs$Observations, PM_conc_pred_vs_obs$Predictions)

####  output figures ####

ggsave(paste0(data.pre, factor.No, "_daily.pdf"), plot = daily_conc_plot, width = 6, height = 7.5)
ggsave(paste0(data.pre, factor.No, "_year.pdf"), plot = year_conc_plot, width = 6, height = 7.5)
ggsave(paste0(data.pre, factor.No, "_month.pdf"), plot = month_conc_plot, width = 6, height = 7.5)
ggsave(paste0(data.pre, factor.No, "_year_month_conc.pdf"), plot = ym_conc_plot, width = 6, height = 7.5)
ggsave(paste0(data.pre, factor.No, "_year_month_frac.pdf"), plot = ym_frac_plot, width = 6, height = 7.5)
# ggsave(paste0(data.pre, "overall.pdf"), plot = overall_contri, width = 9, height = 5)
ggsave(paste0(data.pre, factor.No, "_source_profile.pdf"), plot = source_profile, width = 7, height = 7.5, 
       device = cairo_pdf) # device = cairo_pdf, save the special fonts (super-/sub- script) in pdf
ggsave(paste0(data.pre, factor.No, "_obs_pred_ts.pdf"), plot = pred_obs_species_conc_plot, width = 14.5, height = 9)
ggsave(paste0(data.pre, factor.No, "_obs_pred_sct.pdf"), plot = species_pred_vs_obs_plot, width = 10.5, height = 9)


