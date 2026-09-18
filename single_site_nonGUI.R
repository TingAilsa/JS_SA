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
library(deming) # Thiel-Sen
library(gridExtra)
library(data.table)

#### Functions used ####

### extract the lowest Qm and corresponding task
lowest_Qm_task <- function(base_report_file) {
  
  ## Determine the number of lines to read
  # correlations was quoted with "correlations", use slash "\" to write a double quote character
  "factor \"correlations\" with Best-fit factors"
  line.start = line_number(base_report_file,
                           "Qmtrue, gradnorm, #posoutl, #negoutl, endcode")
  
  end.line = line.start + 21
  start.line = line.start + 2
  
  # Extract the lines including Q values and task numbers
  Q_lines <- base_report_file[start.line:end.line]
  
  # Convert the selected lines (with Q values & task number) into a data frame
  Q_task <- read.table(text = Q_lines,
                       header = F,
                       quote = "\"'")
  # colnames(Q_task) = c("TaskNum", "Qmain", "Qaux", "Q.XX")
  colnames(Q_task) = c("TaskNum", "iterNum", "Q", "Qmain", "Qaux", "Qmtrue",
                       "gradnorm", "posoutlNum", "negoutlNum", "endcode")
  
  Q_task_converge = subset(Q_task, endcode == 4)
  
  # Find the number of task when the value of Qm is the lowest
  lowest_Qm_task <- Q_task_converge$TaskNum[which.min(Q_task_converge$Qmain)]
  
  lowest_Qm = Q_task_converge$Qmain[Q_task_converge$TaskNum == lowest_Qm_task]
  converge_percent = nrow(Q_task_converge)/nrow(Q_task)
  
  return(list(lowest_Qm_task = lowest_Qm_task, 
              lowest_Qm = lowest_Qm,
              converge_percent = converge_percent))
}

#### Extract data in Base Model results for selected task number ####
# time series info from base output
base_results <- function(base_output, task, input.row) {
  # Extract Factor matrix AA & BB from base result
  # the regular expression pattern
  pattern_start <- paste0("Results written by postprocessing for task #\\s+", task, "\\s+---------------------------")
  pattern_end <- paste0("Results written by postprocessing for task #\\s+", task+1, "\\s+---------------------------")
  pattern_end_20 = "task#\\s+Qmain\\s+Qaux\\s+Q\\(XX\\)\\s+dQ<0"
  pattern_end = ifelse(task < 20, pattern_end, pattern_end_20)
  
  # detect the line number where the pattern appears
  base.start.line = grep(pattern_start, base_output)
  base.end.line = grep(pattern_end, base_output)
  
  # data for the selected task number
  # the base run results for the selected task
  base_selectQtask = 
    base_output[
      (base.start.line+3):
        (base.end.line-4)]
  
  #### Extract time series normalized contributions
  ts.start = grep("Factor matrix AA", 
                  base_selectQtask) + 1
  
  # Identify segments that are not spaces
  info_segments = non_space_segments(base_selectQtask[ts.start])
  
  # Estimate the factor number
  factor.No <- sum(info_segments) - 1
  
  base_ts_txt = 
    base_selectQtask[
      ts.start : 
        (ts.start + input.row - 1)]
  
  # lines to dataframe
  base_ts = line_to_df(base_ts_txt, input.row, factor.No+2)
  base_ts$X1 = NULL
  base_ts = mutate_all(base_ts, as.numeric)
  
  # rename the columns and replace the first column values
  colnames(base_ts)[1] = "Serial.No"
  colnames(base_ts)[2:(factor.No+1)] = Factor.serial
  
  #### Extract & estimate base concentration & percent 
  conc.start = grep("Factor matrix BB", 
                    base_selectQtask) + 1
  conc.end = grep("Factor matrix CC", 
                  base_selectQtask) - 4
  
  base_conc_txt = 
    base_selectQtask[
      conc.start:conc.end]
  
  # lines to dataframe
  base_conc = line_to_df(base_conc_txt, 
                         conc.end - conc.start + 1, 
                         factor.No+2)
  sapply(base_conc, class)
  base_conc = mutate_all(base_conc, as.numeric)
  base_conc$X1 = NULL
  
  # rename the columns and replace the first column values
  colnames(base_conc)[1] = "Species"
  colnames(base_conc)[2:(factor.No+1)] = Factor.serial
  
  ##### Extract factor-specific contribution
  
  ## overall fraction contribution
  factor_concen_contri = base_conc[nrow(base_conc), ]
  factor_concen_contri_sum = sum(factor_concen_contri[1, -1])
  factor_fraction_contri = factor_concen_contri/factor_concen_contri_sum
  factor_fraction_contri$Species = factor_concen_contri$Species
  
  ##  factor-specific overall concentration
  factor_concen <- as.numeric(factor_concen_contri[1, -1])
  
  # apply the multiply to base_ts, excluding the first column
  base_ts_conc <- base_ts
  base_ts_conc[,-1] <- sweep(base_ts[,-1], 2, factor_concen, "*")
  
  # estimate overall fraction contribution based on base_conc
  base_fraction = factor_concen_contri[1, -1] / sum(factor_concen)
  
  # overall factor concentration contribution
  base_factor_concen = factor_concen_contri[1, -1]
  
  #### Estimate the predicted daily contribution of each species
  
  # initialize an empty matrix to store the species-specific daily concentraion contributions 
  daily_contribution_matrix <- 
    matrix(0, nrow = nrow(base_ts), 
           ncol = nrow(base_conc))
  
  # column names for the matrix will be species names from base_conc
  colnames(daily_contribution_matrix) <- base_conc$Species # to be replaced in later process
  
  # Loop through each species to calculate daily contributions
  # multiply and sum the contributions
  base_ts_factors <- base_ts[-1]
  
  for(i in 1:nrow(base_conc)) {
    factor_species_conc <- as.numeric(base_conc[i, -1])
    
    daily_contribution_matrix[, i] <- 
      rowSums(t(t(base_ts_factors) * 
                  factor_species_conc))
  }
  
  predict_daily_species_conc <- 
    data.frame(Serial.No = base_ts$Serial.No, 
               daily_contribution_matrix)
  
  #### Extract fitted G vs. reference G Regression matrix
  # fitted G vs. reference G, regression
  #G.correl.start = grep("Regression matrix T1 of fitted G vs. reference G:", 
  #                      base_selectQtask) + 1
  
  # fitted G vs. reference G regression matrix
  #base_G_correl_txt = 
  #  base_selectQtask[
  #    G.correl.start : 
  #      (G.correl.start + factor.No - 1)]
  
  # lines to dataframe
  #base_G_cor = line_to_df(base_G_correl_txt, factor.No, factor.No+2)
  #base_G_cor$X1 = NULL
  #base_G_cor = mutate_all(base_G_cor, as.numeric)
  
  # rename the columns and replace the first column values
  #colnames(base_G_cor)[1] = "Factors"
  #Factor.serial = paste0("Factor", 1:factor.No)
  #colnames(base_G_cor)[2:(factor.No+1)] = Factor.serial
  #base_G_cor$Factors = Factor.serial
  
  
  # Return a list containing both data frames
  return(list(#base_G_cor = base_G_cor, 
    base_ts = base_ts, 
    base_contri = base_conc,
    base_ts_conc = base_ts_conc, 
    base_fraction = base_fraction,
    base_factor_concen = base_factor_concen,
    predict_daily_species_conc = predict_daily_species_conc))
}

#### Match site & date, preparing for base model result plotting  #### 

time_series = function(base_ts, site_date, factor.No){
  base_ts_all = base_ts_date = base_ts
  
  # match "Date", "SiteCode", "PM2.5", "State" info
  # base_ts_all[c("Date", "SiteCode", "PM2.5", "State")] <- 
  #   site_date[c("Date", "SiteCode", "PM2.5", "State")]
  base_ts_all[c("Date", "PM2.5")] <- 
    site_date[c("Date", "PM2.5")]
  base_ts_all$species.sum = rowSums(base_ts_all[, 2:(factor.No+1)])
  sapply(base_ts_all, class)
  cor_PM_sumSpecies = cor(base_ts_all$PM2.5, 
                          base_ts_all$species.sum)
  
  # base_ts_date[c("Date", "SiteCode")] <- 
  #   site_date[c("Date", "SiteCode")]
  base_ts_date$Date = site_date$Date
  base_ts_date$Serial.No = NULL
  
  #### 1. Gather the data for time series plotting
  base_ts_gather = gather(base_ts_date, 
                          "Factor", 
                          "Normalize_contri", 
                          -Date) # , -SiteCode
  
  #### 2. Linear regression resutls - contributions 
  # OLS multiple regression model without an intercept using all other columns
  ts_PM_lm = lm(PM2.5 ~ . - 1, 
                data = base_ts_all[, 
                                   c("PM2.5", 
                                     paste0("Factor", 1:factor.No))])
  ts_PM_lm_beta = summary(ts_PM_lm)$coefficients[, 1]
  ts_PM_lm_beta = data.frame(ts_PM_lm_beta)
  colnames(ts_PM_lm_beta) = "lm.beta.site"
  ts_PM_lm_beta$Factor = rownames(ts_PM_lm_beta)
  ts_PM_lm_beta$Factor.contribution = (ts_PM_lm_beta$lm.beta.site/
                                         sum(ts_PM_lm_beta$lm.beta.site))*100
  # keep three significant digits
  ts_PM_lm_beta$Factor.contr = paste0(
    signif(ts_PM_lm_beta$Factor.contribution, 
           3),
    "%")
  
  base_ts_gather$Date = as.Date(base_ts_gather$Date)
  
  # Return a list containing both data frames
  return(list(base_ts_nmContri_spread = base_ts_date,
              base_ts_nmContri_gather = base_ts_gather, 
              ts_PM_lm_beta = ts_PM_lm_beta,
              cor_PM_sumSpecies = cor_PM_sumSpecies,
              base_ts_nmContri_all = base_ts_all))
}

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
  variable <- gsub("r2", "r\u00B2", variable)
  variable <- gsub("R2", "R\u00B2", variable)
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


####### 1. Read & process other files to use ####### 

#### 1.1 NJUdata & YZ-CDC #### 

##set working directory
setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/YZ_data/base_DISPres1")
getwd()
data.dir <- "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/YZ_data/base_DISPres1"

species_class = read.csv("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/CSN_IMPROVE_ownPC/Element_ion_pha_Cgroup_sequence.csv")


#### 1.N shared process #### 

site_serial_Nos = c("NJ_NJUdata_mannual_rmv_extremes_4.5unc_intp",
                    "NJ_NJUdata_mannual_rmv_extremes_4.5unc_intp_DN", 
                    "YZ_Csub", "YZ_OCEC", "YZ_OCEC_pah")

### 11 color panel based on ggsic_npg
col_npg = append(pal_npg()(10), "#8A4198FF")


########################################################
######## 2. START the LOOP! Process the PMF non-GUI outputs & Plot ####### 
########################################################

correl_r_p_summary = NULL
summary_base = NULL
pred_obs_compare_summary = NULL

for (site.serial in site_serial_Nos) { # no SWB for now
  
  ## read site related files
  site_folder = site.serial
  site_date_PM_species = read.csv(file.path(site.serial, paste0(site.serial, "_CMD.csv")))
  site_info = read.csv(file.path(site.serial, paste0(site.serial, "_SWB.csv")))
  site_date_PM_species$X = site_info$X = NULL
  
  # change colnames of site_info
  site_info = plyr::rename(
    site_info, 
    c("PM25" = "PM2.5"))
  
  # Extract site & date info
  site_date_PM_species$Date = as.Date(site_date_PM_species$Date)
  
  start.date = site_date_PM_species$Date[1]
  end.date = site_date_PM_species$Date[nrow(site_date_PM_species)]
  
  ## extract concentrations & uncertainties
  site_date_PM_species_conc <- 
    site_date_PM_species %>%
    # remove columns with "unc_" in their names
    dplyr::select(-contains("unc_")) %>%
    # rename columns that start with "conc_"
    rename_with(~gsub("^conc_", "", .x), starts_with("conc_"))
  
  site_date_PM_species_unc <- 
    site_date_PM_species %>%
    # remove columns with "conc_" in their names
    dplyr::select(-contains("conc_")) %>%
    # rename columns that start with "unc_"
    rename_with(~gsub("^unc_", "", .x), starts_with("unc_"))
  
  ## rename the columns
  site_date_PM_species_conc = plyr::rename(
    site_date_PM_species_conc, 
    c("PM25" = "PM2.5"))
  
  site_date_PM_species_unc = plyr::rename(
    site_date_PM_species_unc, 
    c("PM25" = "PM2.5"))
  
  #### species strong, weak, bad
  site.data.row = site_info$site.row
  strong.species.count = length(strong_species(site_info, "Al", "PM2.5"))

  ## Select weak & strong variables by the value
  strong_weak_assign = strong_weak(site_info, "Al", "PM2.5")
  strong_weak_df = strong_weak_assign$strong_weak_col
  site.weak.strong = strong_weak_assign$strong_weak_species
  species.weak.strong.count = length(site.weak.strong) - 1
  
  for (factor.No in 5:11) { # 5:11, # factor.No = 8

    # factor.No = 10
    
    site.serial.factor.pre = paste0(site.serial, "_F_", factor.No, "_")
    # name.prefix = paste0(data.prefix, site.serial.factor.pre)
    
    tryCatch({
      ####### Read the results from bash script ####### 
      # Access the input file name passed as an argument
      folder_path <- paste0(site.serial, "/Factor_", factor.No, "/") 
      base_output = readLines(paste0(folder_path, 
                                     site.serial.factor.pre,
                                     "base.txt")) # "_base.txt"
      disp_output = readLines(paste0(folder_path, 
                                     site.serial.factor.pre,
                                     "DISPres1.txt"))
      bs_output = readLines(paste0(folder_path, 
                                   site.serial.factor.pre,
                                   "BS_.txt"))
      base_report = readLines(paste0(folder_path, 
                                     site.serial.factor.pre,
                                     "base_PMFreport.txt"))
      
      # Bootstrap mapping rate
      ## some sites, the BS results are not complete, thus, use the tryCatch
      # Try to run the function and catch errors
      bs_map_result <- tryCatch({
        bs_map_fra <- bs_map(bs_output, 50, factor.No, 0.6) # 0.6 is the threshold or r
        bs_map_fraction <- bs_map_fra$percent_less_than_0_6
        bs_overall_map <- bs_map_fra$BS_overall
        list(bs_map_fra = bs_map_fra, bs_map_fraction = bs_map_fraction, bs_overall_map = bs_overall_map)
      }, error = function(e) {
        list(bs_map_fra = NULL, bs_map_fraction = NULL, bs_overall_map = "error_in_BS")
      })
      
      # extract values from the bs_map_result
      bs_map_fra <- bs_map_result$bs_map_fra
      bs_map_fraction <- bs_map_result$bs_map_fraction
      bs_overall_map <- bs_map_result$bs_overall_map
      
      # Find the number of task when the value of Qm is the lowest
      lowest_Qm_taskNo = lowest_Qm_task(base_report)$lowest_Qm_task
      lowest_Qm = round(lowest_Qm_task(base_report)$lowest_Qm, 0)
      # converge condition in the 20 base runs
      converge_percent = lowest_Qm_task(base_report)$converge_percent
      
      Factor.serial = paste0("Factor", 1:factor.No)
      
      Q.exp = 
        site.data.row*strong.species.count - 
        ((factor.No*site.data.row) + 
           (factor.No*strong.species.count))
      
      ####### Extract base info from PMF CMD outputs & match with date, PM #######
      
      # extract df from base outputs
      base_info = base_results(base_output, 
                               lowest_Qm_taskNo, 
                               site.data.row)
      
      # base_G_cor = base_info$base_G_cor
      base_ts = base_info$base_ts
      base_contri = base_info$base_contri # species overall contribution
      base_ts_conc = base_info$base_ts_conc 
      base_conc_fraction = base_info$base_fraction
      base_conc_fraction = as.data.frame(t(base_conc_fraction))
      base_factor_concen = base_info$base_factor_concen
      base_factor_concen = as.data.frame(t(base_factor_concen))
      predict_daily_species_conc = base_info$predict_daily_species_conc
      
      # rename the overall fraction contribution (based on daily concentration) of each factor
      colnames(base_conc_fraction)[1] = "Fractrion_conc_based"
      base_conc_fraction$Faraction_conc_contri = 
        paste0(round(base_conc_fraction$Fractrion_conc_based*100, 1),
               "%")
      base_conc_fraction$Factor = rownames(base_conc_fraction)
      
      # rename the overall concentration contribution of each factor
      names(base_factor_concen)[1] = "Factor_concentration"
      base_factor_concen$Factor = rownames(base_factor_concen)
      
      # assign species names 
      base_contri$Species = site.weak.strong
      colnames(predict_daily_species_conc)[-1] = 
        site.weak.strong
      
      # get concentration fraction contribution of each species
      base_percent = conc_percent_contri(base_contri)
      names(base_percent)[1] = "Species"
      
      base_conc_plot = gather(base_contri,
                              "Factor", 
                              "Concentration", 
                              -Species)
      
      base_percent_plot = gather(base_percent,
                                 "Factor", 
                                 "Percent", 
                                 -Species)
      
      #### estimate the uncertainty scaled residual
      daily_species_scale_residual = 
        (site_date_PM_species_conc[, site.weak.strong] - 
           predict_daily_species_conc[, site.weak.strong]) /
        site_date_PM_species_unc[, site.weak.strong]
      
      # remove PM2.5 data
      daily_species_scale_residual_noPM = 
        daily_species_scale_residual[-ncol(daily_species_scale_residual)]
      
      # Q_true & Q_robust
      daily_species_scale_residual_noPM_robust = daily_species_scale_residual_noPM
      daily_species_scale_residual_noPM_robust[abs(daily_species_scale_residual_noPM_robust) > 4] <- 0
      
      Q.true = round(sum(daily_species_scale_residual_noPM^2), 0)
      Q.robust = round(sum(daily_species_scale_residual_noPM_robust^2), 0)
      
      # Daily Q/Qexp ratio
      daily_Q_Qexp = 
        data.frame(site_date_PM_species_conc$Date, 
                   rowSums(daily_species_scale_residual_noPM^2)/species.weak.strong.count)
      colnames(daily_Q_Qexp) = c("Date", "Q_Qexp_ratio")
      daily_Q_Qexp$higher.3.scalRes.fraction = 
        rowSums(abs(daily_species_scale_residual_noPM) > 3) / species.weak.strong.count
      daily_Q_Qexp$higher.3.scalRes.per = 
        paste0(round(daily_Q_Qexp$higher.3.scalRes.fraction * 100, 2), "%")
      
      # Species-specific Q/Qexp ratio
      species_Q_Qexp = data.frame(colSums(daily_species_scale_residual^2)/(Q.exp/strong.species.count))
      species_Q_Qexp$Species = row.names(species_Q_Qexp)
      colnames(species_Q_Qexp)[1] = "Q_Qexp_ratio"
      species_Q_Qexp$higher.3.scalRes.fraction = 
        colSums(abs(daily_species_scale_residual) > 3) / site.data.row
      species_Q_Qexp$higher.3.scalRes.per = 
        paste0(round(species_Q_Qexp$higher.3.scalRes.fraction * 100, 2), "%")
      
      # normalized and overall contributions based on OLS MLR
      site_date_PM = dplyr::select(site_date_PM_species_conc,
                            Date, PM2.5) # SiteCode, State, 
      time_series_analyses = time_series(base_ts, site_date_PM, factor.No)
      # base_ts_nmContri_gather = time_series_analyses$base_ts_nmContri_gather
      base_ts_nmContri_spread = time_series_analyses$base_ts_nmContri_spread
      ts_PM_lm_beta = time_series_analyses$ts_PM_lm_beta
      cor_PM_sumSpecies = time_series_analyses$cor_PM_sumSpecies
      # base_ts_nmContri_all = time_series_analyses$base_ts_nmContri_all
      
      base_ts_conc_all = cbind(base_ts_conc, site_date_PM)
      base_ts_conc_all$Date = as.Date(base_ts_conc_all$Date)
      base_ts_conc_all$SerialNumber = NULL
      # colnames(base_ts_conc_all)[ncol(base_ts_conc_all)] = "PM2.5"
      
      # estimate the PMF explained PM2.5 and the observations
      ts_conc_2PM = base_ts_conc_all
      ts_conc_2PM$PMF_PM2.5 = rowSums(ts_conc_2PM[, Factor.serial], na.rm = TRUE)
      median_PMF_PM2.5 = median(ts_conc_2PM$PMF_PM2.5)
      median_obs_PM2.5 = median(ts_conc_2PM$PM2.5)
      cor.PMF.obs.PM = cor(ts_conc_2PM$PMF_PM2.5, ts_conc_2PM$PM2.5)
      
      #### time-series concentration contribution estimation
      
      # pivot data from wide to long
      base_ts_conc_long <-
        base_ts_conc_all %>% 
        pivot_longer(
          cols = Factor.serial[1]:Factor.serial[factor.No],
          names_to = c("Factor"),
          # names_pattern = "new_?(.*)_(.)(.*)",
          values_to = "Concentration"
        )
      
      # Get annual and monthly contributions
      base_ts_conc_long$Year = year(base_ts_conc_long$Date)
      base_ts_conc_long$Month = month(base_ts_conc_long$Date)
      
      # get the Year_Month date
      base_ts_conc_long = 
        base_ts_conc_long %>%
        mutate(Year_Month = 
                 as.Date(
                   paste(Year, Month, "01", sep = "-")))
      head(base_ts_conc_long)
      
      
      #### time-series Fraction contribution estimation
      base_day_conc = base_ts_conc_all
      base_day_conc$Serial.No = base_day_conc$PM2.5 = NULL
      base_day_conc = relocate(base_day_conc, Date, .before = Factor1)
      
      # get daily fraction contribution
      base_day_percent = conc_percent_contri(base_day_conc)
      names(base_day_percent)[1] = "Date"
      
      # get year & month
      base_day_percent$Year = year(base_day_percent$Date)
      base_day_percent$Month = month(base_day_percent$Date)
      
      # get the Year_Month date
      base_day_percent = 
        base_day_percent %>%
        mutate(Year_Month = 
                 as.Date(
                   paste(Year, Month, "01", sep = "-")))
      # head(base_day_percent)
      
      # convert the df for plotting
      base_day_percent_plot = 
        base_day_percent %>%
        pivot_longer(
          cols = Factor1:Factor.serial[factor.No],
          names_to = c("Factor"),
          values_to = "Percent"
        )
      head(base_day_percent_plot)
      
      
      ####### Extract DISP info from PMF CMD, and validation results #######
      
      # DISP results - Credible intervals
      disp_down_conc = disp_analysis(disp_output)$disp_down
      disp_up_conc = disp_analysis(disp_output)$disp_up
      disp_down_conc$Species = disp_up_conc$Species = site.weak.strong
      
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
      
      
      # # DISP fraction contribution
      # disp_down_percent = conc_percent_contri(disp_down_conc)
      # names(disp_down_percent)[1] = "Species"
      # disp_down_percent_plot = gather(disp_down_percent,
      #                                 "Factor", 
      #                                 "Percent.down", 
      #                                 -Species)
      # 
      # disp_up_percent = conc_percent_contri(disp_up_conc)
      # names(disp_up_percent)[1] = "Species"
      # disp_up_percent_plot = gather(disp_up_percent,
      #                               "Factor", 
      #                               "Percent.up", 
      #                               -Species)
      
      
      # DISP results - overall validation of results
      disp.error.code = disp_analysis(disp_output)[[1]]
      disp.qdrop = disp_analysis(disp_output)[[2]]
      
      ####### Source Assignment & Match #######
      #main3_species = Nmain_Species(base_percent, 3)
      main5_species = Nmain_Species(base_percent, 5)
      # 
      # # Source detection according to info in recent publications
      # main_source = source_ref(base_percent, 5)
      # main_source$Source.No = sapply(strsplit(main_source$Source_reference, "-"), "[", 1)
      # main_source$Factor_source = main_source$Factor
      # main_source$Factor_source[main_source$Source_reference != "F-"] = 
      #   main_source$Source_reference[main_source$Source_reference != "F-"]
      
      # # replace colnames in base_ts by the exact source
      # colnames(base_ts_nmContri_spread)[1:factor.No] = main_source$Factor_source
      # base_ts_nmContri_spread$Date = NULL
      # base_ts_nmContri_spread$SiteCode = site_code
      # base_ts_nmContri_spread = site_color(base_ts_nmContri_spread)
      
      # merge datasets for source profile plotting
      base_disp_conc_to_merge <- 
        list(base_conc_plot, base_percent_plot, 
             disp_down_conc_plot, disp_mean_conc_plot, disp_up_conc_plot, 
             species_class, main5_species)
      
      conc_percent_bsDisp <- 
        Reduce(function(x, y) 
          merge(x, y), 
          base_disp_conc_to_merge)
      
      # match time-series contribution data with source info
      ts_conc_plot = merge(base_ts_conc_long, main5_species)
      base_day_percent_plot = merge(base_day_percent_plot, main5_species)
      
      lm_beta_plot = merge(ts_PM_lm_beta, main5_species)
      base_ts_conc_long_plot = merge(base_ts_conc_long, main5_species)
      
      # reorder the file 
      ts_conc_plot = 
        ts_conc_plot[with(ts_conc_plot,
                          order(Date, Factor)), ]
      base_day_percent_plot = 
        base_day_percent_plot[with(base_day_percent_plot,
                                   order(Date, Factor)), ]
      
      # match overall contribution based on normalized data with that from mass concentration
      lm_beta_plot = merge(lm_beta_plot, base_conc_fraction)
      lm_beta_plot$Fractrion_conc_based = 100 * lm_beta_plot$Fractrion_conc_based
      
      lm_beta_plot = merge(lm_beta_plot, base_factor_concen)
      
      cor.two.fraction.contri = cor.test(lm_beta_plot$Factor.contribution,
                                         lm_beta_plot$Fractrion_conc_based,
                                         method = "pearson")$estimate
      
      ####### Base, concentration & fraction contributions at different time scales #######
      
      ### Concentration contributions
      
      ## somehow quantile estimate via data.frame not work!!! change to data.table to estimate
      # Convert to data.table
      base_ts_conc_long_dt <- as.data.table(ts_conc_plot)
      
      # Summarize using data.table, getting concentrations contribution at different time scales
      ts_annual_conc <- 
        base_ts_conc_long_dt[, .(
          Concentration = quantile(Concentration, 0.5),
          conc_up = quantile(Concentration, 0.95),
          conc_down = quantile(Concentration, 0.05)
        ), 
        by = .(Factor, Year, Main_Species)]
      
      ts_month_conc <- 
        base_ts_conc_long_dt[, .(
          Concentration = quantile(Concentration, 0.5),
          conc_up = quantile(Concentration, 0.95),
          conc_down = quantile(Concentration, 0.05)
        ), 
        by = .(Factor, Month, Main_Species)]
      
      ts_year_month_conc <- 
        base_ts_conc_long_dt[, .(
          Concentration = quantile(Concentration, 0.5),
          conc_up = quantile(Concentration, 0.95),
          conc_down = quantile(Concentration, 0.05)
        ), 
        by = .(Factor, Year_Month, Main_Species)]
      
      ### Percent
      base_day_percent_dt = as.data.table(base_day_percent_plot)
      
      # annual percent & CI
      ts_annual_fraction = 
        base_day_percent_dt[, .(
          Percent = quantile(Percent, 0.5),
          percent_up = quantile(Percent, 0.95),
          percent_down = quantile(Percent, 0.05)
        ), 
        by = .(Factor, Year, Main_Species)]
      head(ts_annual_fraction)
      
      # monthly percent & CI
      ts_month_fraction = 
        base_day_percent_dt[, .(
          Percent = quantile(Percent, 0.5),
          percent_up = quantile(Percent, 0.95),
          percent_down = quantile(Percent, 0.05)
        ), 
        by = .(Factor, Month, Main_Species)]
      
      # year_month percent & CI
      ts_year_month_fraction = 
        base_day_percent_dt[, .(
          Percent = quantile(Percent, 0.5),
          percent_up = quantile(Percent, 0.95),
          percent_down = quantile(Percent, 0.05)
        ), 
        by = .(Factor, Year_Month, Main_Species)]
      

      ##########################################
      ####### PLOT #######
      ##########################################
      
      #### Source Profile - Concentration & percent Normalize_contri #### 
      conc_percent_bsDisp_use = conc_percent_bsDisp
      conc_percent_bsDisp_use = merge(conc_percent_bsDisp_use, lm_beta_plot)
      # conc_percent_bsDisp_use = subset(conc_percent_bsDisp, Source.No != "F")
      
      # Convert 0 to 1e-10 for columns to be used for y-axis, there is log transfer later
      conc_percent_bsDisp_use <- 
        conc_percent_bsDisp_use %>%
        mutate(across(Concentration:disp_conc_up, 
                      ~replace(., . <= 0, 1e-10)))
      
      ##### Convert percent values to make the scale pattern similar to log concentration
      # set the Percent == 0 to a low value before log
      conc_percent_bsDisp_use$Percent[conc_percent_bsDisp_use$Percent <= 0] = 1e-05

      conc_percent_bsDisp_use$Concentration[conc_percent_bsDisp_use$Concentration <= 0] = 1e-05
      conc_percent_bsDisp_use$disp_conc_down[conc_percent_bsDisp_use$disp_conc_down <= 0] = 1e-05
      conc_percent_bsDisp_use$disp_conc_up[conc_percent_bsDisp_use$disp_conc_up <= 0] = 1e-05
      
      # define the breakpoints and corresponding values in percent and exponent values
      percent_values <- c(100, 80, 60, 40, 20, 0)
      exponent_values <- c(1e-00, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05)
      # exponent_values <- c(1e+01, 1e-00, 1e-01, 1e-02, 1e-03, 1e-04)
      
      # get the mapped percent
      conc_percent_bsDisp_use$MappedPercent  = 
        map_percent_to_exponent(percent_values, 
                                exponent_values, 
                                conc_percent_bsDisp_use$Percent)
      
      # check the value range
      # summary(
      #   select(conc_percent_bsDisp_use, 
      #          MappedPercent, Percent, Concentration, 
      #          disp_conc_mean, disp_conc_down, disp_conc_up))
      
      #### Step trying to identify the trans for sec.axis
      
      # find the middle position of the factor/source names on x-axis
      middle_positions <- 
        conc_percent_bsDisp_use %>%
        dplyr::group_by(Factor) %>%
        dplyr::summarize(Factor = Factor[1],
                         middle = custom_median(as.numeric(sequence)),
                         Factor_conc = paste(round(unique(Factor_concentration), 3), 
                                             collapse = ", "),
                         Factor_conc_fr = paste(unique(Faraction_conc_contri), 
                                                collapse = ", ")) %>%
        dplyr::arrange(Factor)
      
      middle_species = 
        conc_percent_bsDisp_use$Species[
          conc_percent_bsDisp_use$sequence == 
            middle_positions$middle[1]]
      
      middle_positions$Species = middle_species[1]
      
      ### add BS results
      # bs_map_fraction_source = 
      #   merge(bs_map_fraction, 
      #         select(lm_beta_plot, 
      #                Factor, Factor))
      # middle_positions = merge(middle_positions, bs_map_fraction_source)
      
      ### Mark on figure, ggtext
      middle_positions$Factor_contr = 
        paste0(middle_positions$Factor, ", ", 
               "frac ", middle_positions$Factor_conc_fr,
               ", conc ", middle_positions$Factor_conc)
      
      
      ### line position
      species_count = length(unique(conc_percent_bsDisp_use$Species))
      species_x_num = get_divisible_numbers(species_count, 4)
      profile_line_position =
        data.frame(
          Factor = rep(paste0("Factor", 1:factor.No), each = length(species_x_num)),
          species_x_num = rep(species_x_num, factor.No))
      
      # Create the plot
      source_profile <- 
        ggplot(conc_percent_bsDisp_use,
               aes(x = reorder(Species, sequence), 
                   group = Factor)) +
        # Bar plot for Concentration
        geom_bar(aes(y = Concentration), 
                 stat = "identity", fill = "lightblue", width = 0.6, alpha = 0.8) +
        # lines for easier species position determination
        geom_vline(data = profile_line_position, 
                   aes(xintercept = species_x_num), 
                   linetype = "dashed", color = "gray", linewidth = 0.35) + 
        # Point plot for transformed Percent
        geom_point(aes(y = MappedPercent, group =1), 
                   color = "grey25", shape = 15, size = 1.5) +
        geom_point(aes(y = disp_conc_mean), 
                   color = "grey25", shape = 1, size = 1.5) +
        geom_errorbar(aes(ymin = disp_conc_down, 
                          ymax = disp_conc_up), 
                      width = 0.3, color = "grey35", linewidth = 0.35) +
        facet_grid(Factor ~ ., switch = "y") +
        scale_y_continuous(
          name = format_variable("Concentration µg/m3"),
          breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e-00, 1e+01),
          # "", only show the break but not text on y axis
          labels = c(expression(10^"-5"), expression(""), 
                     expression(10^"-3"), expression(""), 
                     expression(10^"-1"), expression(""), expression(10^"1")),
          sec.axis = sec_axis(
            # set the trans the same as the main y axis
            transform = ~., 
            breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e+00),
            name = "Explained Variance %",
            labels = c(0, "", 40, "", 80, "")
          ),
          transform = mylog_trans(base=10, from=-5),
          limits = c(1E-5, max(conc_percent_bsDisp$Concentration))) +
        ggtitle(paste0(site.serial.factor.pre, # "\n",
                       ", DISP.Error = ", disp.error.code, 
                       ", DISP.Qdrop = ", paste0(round(disp.qdrop/lowest_Qm*1000, 1), "‰,"), "\n",
                       "BS.map = ", bs_overall_map, 
                       ", Qmin = ", lowest_Qm, 
                       ", PMF.PM25 = ", round(median_PMF_PM2.5, 2),
                       ", obs.PM25 = ", round(median_obs_PM2.5, 2),
                       ", conc-µg/m3")) + 
        # scale_fill_manual(values = col_npg) +
        xlab(format_variable("PM25 Species")) +
        ylab(format_variable("Concentration µg/m3")) +
        scale_x_discrete(labels = function(x) format_variable(x)) +
        geom_text(data = middle_positions, size = 4,
                  aes(x = Species, y = 7e-02, label = Factor_contr),
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        theme_bw() +
        theme_text_speciesName +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )
      
      # source_profile
      
      conc_percent_bsDisp_sub = 
        subset(conc_percent_bsDisp_use, Fractrion_conc_based >= 5)
      profile_line_position_sub = 
        subset(profile_line_position, 
               Factor %in% conc_percent_bsDisp_sub$Factor)
      middle_positions_sub = 
        subset(middle_positions,
               Factor %in% conc_percent_bsDisp_sub$Factor)
      
      ggplot(conc_percent_bsDisp_sub,
               aes(x = reorder(Species, sequence), 
                   group = Factor)) +
        # Bar plot for Concentration
        geom_bar(aes(y = Concentration), 
                 stat = "identity", fill = "lightblue", width = 0.6, alpha = 0.8) +
        # lines for easier species position determination
        geom_vline(data = profile_line_position_sub, 
                   aes(xintercept = species_x_num), 
                   linetype = "dashed", color = "gray", linewidth = 0.35) + 
        # Point plot for transformed Percent
        geom_point(aes(y = MappedPercent, group =1), 
                   color = "grey25", shape = 15, size = 1.5) +
        geom_point(aes(y = disp_conc_mean), 
                   color = "grey25", shape = 1, size = 1.5) +
        geom_errorbar(aes(ymin = disp_conc_down, 
                          ymax = disp_conc_up), 
                      width = 0.3, color = "grey35", linewidth = 0.35) +
        facet_grid(Factor ~ ., switch = "y") +
        scale_y_continuous(
          name = format_variable("Concentration µg/m3"),
          breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e-00, 1e+01),
          # "", only show the break but not text on y axis
          labels = c(expression(10^"-5"), expression(""), 
                     expression(10^"-3"), expression(""), 
                     expression(10^"-1"), expression(""), expression(10^"1")),
          sec.axis = sec_axis(
            # set the trans the same as the main y axis
            transform = ~., 
            breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1e+00),
            name = "Explained Variance %",
            labels = c(0, "", 40, "", 80, "")
          ),
          transform = mylog_trans(base=10, from=-5),
          limits = c(1E-5, max(conc_percent_bsDisp_sub$Concentration))) +
        ggtitle(paste0(site.serial.factor.pre, # "\n",
                       ", DISP.Error = ", disp.error.code, 
                       ", DISP.Qdrop = ", paste0(round(disp.qdrop/lowest_Qm*1000, 1), "‰,"), "\n",
                       "BS.map = ", bs_overall_map, 
                       ", Qmin = ", lowest_Qm, 
                       ", PMF.PM25 = ", round(median_PMF_PM2.5, 2),
                       ", obs.PM25 = ", round(median_obs_PM2.5, 2),
                       ", conc-µg/m3")) + 
        # scale_fill_manual(values = col_npg) +
        xlab(format_variable("PM25 Species")) +
        ylab(format_variable("Concentration µg/m3")) +
        scale_x_discrete(labels = function(x) format_variable(x)) +
        # geom_text(data = middle_positions_sub, size = 4,
        #           aes(x = Species, y = 7e-02, label = Factor_contr),
        #           inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        theme_bw() +
        theme_text_speciesName +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )
      
      
      
      
      #### Time-series concentration contribution #### 
      
      #### Daily
      # daily_plot_use = subset(ts_conc_plot, Source.No != "F")
      daily_plot_use = ts_conc_plot
      
      # set date break for daily & year_month figure
      start_year <- as.Date(paste0(year(start.date), "-01-01"))  
      end_year <- as.Date(paste0(year(end.date), "-12-31"))  
      
      # set gap in figure if no data for >15 days
      daily_plot_use <- 
        daily_plot_use %>%
        arrange(Date) %>%
        mutate(gap = c(0, diff(as.numeric(Date))) > 15, # calculate gaps, convert Date to numeric for diff
               group = cumsum(gap)) # Cumulative sum to create a new group after each gap
      
      # define the x-y position of ggtext
      middle_positions$middle_day = median(daily_plot_use$Date)
      middle_positions$middle_month = month(median(daily_plot_use$Date))
      middle_positions$middle_year = year(median(daily_plot_use$Date))
      
      text_y_position =
        ddply(daily_plot_use, 
              .(Factor),
              summarise,
              text_y_day = quantile(Concentration, 0.997),
              text_y_mon = quantile(Concentration, 0.55),
              text_y_yr = quantile(Concentration, 0.55),
              text_y_yr_mon = quantile(Concentration, 0.95))
      
      middle_positions_ts = 
        merge(middle_positions, text_y_position)
      
      daily_conc_plot <- 
        ggplot(daily_plot_use, 
               aes(x = Date, y = Concentration, 
                   group = Factor, color = Factor)) + # group = 1
        geom_line(aes(group = group), linewidth = 0.3, alpha = 0.6) +
        geom_point(size = 0.7) +
        labs(x = "Date", y = format_variable("Concentration µg/m3")) +
        facet_grid(Factor ~., scales = "free_y") +
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + # y start from 0, and number of breaks, pretty function
        scale_x_date(
          breaks = seq(from = start_year, to = end_year, by = "6 month"),
          date_labels = ("%Y-%m")) +
        scale_color_manual(values = col_npg) +
        geom_text(data = middle_positions_ts, size = 4,
                  aes(x = middle_day, y = text_y_day, label = Factor_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        theme_base() +
        theme_text_speciesName +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "none"
        )
      
      # daily_conc_plot
      
      ## add PM2.5 & EC 
      site_date_PM_ec = 
        dplyr::select(site_date_PM_species_conc, "Date", "EC", "PM2.5")
      site_date_PM_ec_spread = 
        site_date_PM_ec %>%
        pivot_longer(
          cols = where(is.numeric), #Al:PM2.5, 
          names_to = c("Factor"),
          values_to = "Concentration"
        )
      site_date_PM_ec_spread$Type = "Species"
      
      daily_plot_species = 
        subset(
          dplyr::select(
            daily_plot_use, Date, Factor, Concentration),
          Factor %in% conc_percent_bsDisp_sub$Factor)
      daily_plot_species$Type = "Sources"
      
      site_date_PM_EC_factor =
        rbind(site_date_PM_ec_spread, daily_plot_species)
      setDT(site_date_PM_EC_factor)
      
      # Sort the data by date (if not already sorted)
      setorder(site_date_PM_EC_factor, Date)
      
      # Create a grouping variable where dates within 7 days are grouped together
      site_date_PM_EC_factor[, date_group := cumsum(c(0, diff(Date)) >= 7)]
      site_date_PM_EC_factor[, date_group := as.Date(paste0(format(min(Date), "%Y-%m"), "-01")), by = date_group]
      site_date_PM_EC_factor$date_group = 
        as.Date(site_date_PM_EC_factor$date_group, 
                origin = "1970-01-01")
      
      # round to "mapped" monthly average
      month_PM_EC_factor =
        site_date_PM_EC_factor[, .(
          Concentration = quantile(Concentration, 0.5),
          Concentration_up = quantile(Concentration, 0.95),
          Concentration_down = quantile(Concentration, 0.05)
        ), 
        by = .(Factor, Type, date_group)]
      
      # plot the "mapped year-month" trends
      # vlines at Jan and July
      vline_years = unique(year(month_PM_EC_factor$date_group))
      vline_yms = as.Date(
        append(paste0(vline_years, "-07-01"), 
               paste0(vline_years, "-01-01")))
      ym_line_position = 
        data.frame(
          Factor = rep(unique(month_PM_EC_factor$Factor), each = length(vline_yms)),
          vline_yms = rep(vline_yms, length(unique(month_PM_EC_factor$Factor))))
      unique(ym_line_position$vline_yms)
      
      # ym_line_position = subset(ym_line_position, vline_yms > min(month_PM_EC_factor$date_group))
      
      # plot the change
      ggplot(month_PM_EC_factor, 
               aes(x = date_group, 
                   y = Concentration, 
                   group = Type, color = Type)) + # group = 1
        # lines for easier species position determination
        geom_vline(data = ym_line_position, 
                   aes(xintercept = vline_yms), 
                   linetype = "dashed", color = "gray") + 
        geom_errorbar(aes(ymin = Concentration_down, ymax = Concentration_up),
                      width = 0.15, alpha = 0.8) +
        geom_point(size = 1.2) +
        # the preset format_variable function, for sub-/super- scripts
        labs(x = "Date", y = format_variable("Concentration µg/m3")) +
        facet_grid(Factor ~., scales = "free_y") +
        # pretty function, y start from 0, and 3 breaks
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 2)) + 
        # Setting breaks every X months  
        scale_x_date(
          breaks = seq(from = start_year, to = end_year, by = "6 month"),
          date_labels = ("%Y-%m")) +
        # geom_text(data = middle_positions_ts, size = 4,
        #           aes(x = middle_day, y = text_y_yr_mon, label = Factor_contr),
        #           inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        scale_color_manual(values = c("#377eb8", "#ff7f00")) +
        theme_base() +
        theme_ts +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
          legend.position = "none"
        )
      
      #### Annual
      # annual_plot_use = subset(ts_annual_conc, Source.No != "F")
      annual_plot_use = ts_annual_conc
      
      year_conc_plot <- 
        #  ggplot(subset(annual_plot_use,
        #                SiteCode = unique(annual_plot_use$SiteCode)[2]), 
        ggplot(annual_plot_use,
               aes(x = Year, y = Concentration, 
                   group = Factor, color = Factor)) +
        labs(x = "Year", y = format_variable("Concentration µg/m3")) +
        facet_grid(Factor ~., scales = "free_y") +
        geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                      width = 0.15) +
        geom_point(size = 1.5, alpha = 0.8) +
        scale_x_continuous(breaks = 
                             pretty(annual_plot_use$Year, # pretty{base}
                                    n = length(unique(annual_plot_use$Year)), 
                                    min.n = 1)) +
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + 
        geom_text(data = middle_positions_ts, size = 4,
                  aes(x = middle_year, y = text_y_yr, label = Factor_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        scale_color_manual(values = col_npg) +
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )
      
      # year_conc_plot
      
      #### Month
      # month_plot_use = subset(ts_month_conc, Source.No != "F")
      month_plot_use = ts_month_conc
      
      month_conc_plot <- 
        #  ggplot(subset(month_plot_use,
        #                SiteCode = unique(month_plot_use$SiteCode)[2]), 
        ggplot(month_plot_use,
               aes(x = Month, y = Concentration, 
                   group = Factor, color = Factor)) +
        labs(x = "Month", y = format_variable("Concentration µg/m3")) +
        facet_grid(Factor ~., scales = "free_y") +
        geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                      width = 0.15) +
        geom_point(size = 1.5, alpha = 0.8) +
        scale_x_continuous(breaks = 
                             pretty(month_plot_use$Month, # pretty{base}
                                    n = length(unique(month_plot_use$Month)), 
                                    min.n = 1)) +
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + 
        geom_text(data = middle_positions_ts, size = 4,
                  aes(x = 6, y = text_y_mon, label = Factor_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        scale_color_manual(values = col_npg) +
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )
      
      # month_conc_plot
      
      #### year_month
      base_ts_conc_ym_conc = ts_year_month_conc
      
      # vlines at Jan and July
      vline_years = unique(year(base_ts_conc_ym_conc$Year_Month))
      vline_yms = as.Date(
        append(paste0(vline_years, "-07-01"), 
              paste0(vline_years, "-01-01")))
      ym_line_position = 
        data.frame(
          Factor = rep(paste0("Factor", 1:factor.No), each = length(vline_yms)),
          vline_yms = rep(vline_yms, factor.No))
      unique(ym_line_position$vline_yms)
      # ym_line_position = subset(ym_line_position, vline_yms > min(base_ts_conc_ym_conc$Year_Month))
      
      # plot the change
      ym_conc_plot <-
        ggplot(base_ts_conc_ym_conc, 
               aes(x = Year_Month, 
                   y = Concentration, 
                   group = Factor)) + # group = 1
        # lines for easier species position determination
        geom_vline(data = ym_line_position, 
                   aes(xintercept = vline_yms), 
                   linetype = "dashed", color = "gray") + 
        geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                      width = 0.15, color = "#377eb8", alpha = 0.8) +
        geom_point(size = 1.2, color = "#377eb8") +
        # the preset format_variable function, for sub-/super- scripts
        labs(x = "Date", y = format_variable("Concentration µg/m3")) +
        facet_grid(Factor ~., scales = "free_y") +
        # pretty function, y start from 0, and 3 breaks
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 2)) + 
        # Setting breaks every X months  
        scale_x_date(
          breaks = seq(from = start_year, to = end_year, by = "6 month"),
          date_labels = ("%Y-%m")) +
        # geom_text(data = middle_positions_ts, size = 4,
        #           aes(x = middle_day, y = text_y_yr_mon, label = Factor_contr), 
        #           inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        # scale_color_manual(values = col_npg) +
        theme_base() +
        theme_ts +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
          legend.position = "none"
        )
      
      ym_conc_plot
      
      #### Time-series Fraction contribution #### 
      
      # define the y position of ggtext
      text_y_frac_position =
        ddply(base_day_percent_plot, 
              .(Factor),
              summarise,
              text_y_frac_day = quantile(Percent, 0.995),
              text_y_frac_mon = quantile(Percent, 0.75),
              text_y_frac_yr = quantile(Percent, 0.55),
              text_y_frac_yr_mon = quantile(Percent, 0.95))
      
      middle_positions_frac_ts = 
        merge(middle_positions_ts, text_y_frac_position)
      
      ## fraction time-series
      # Daily
      # set gap in figure if no data for >15 days
      base_day_percent_plot <- 
        base_day_percent_plot %>%
        arrange(Date) %>%
        mutate(gap = c(0, diff(as.numeric(Date))) > 15, # calculate gaps, convert Date to numeric for diff
               group = cumsum(gap)) # Cumulative sum to create a new group after each gap
      
      # plot
      daily_percent_plot <-
        ggplot(base_day_percent_plot,
               aes(x = Date, y = Percent,
                   group = Factor, color = Factor)) + # group = 1
        geom_line(aes(group = group), linewidth = 0.3, alpha = 0.6) +
        geom_point(size = 0.7) +
        # the preset format_variable function, for sub-/super- scripts
        labs(x = "Date", y = format_variable("Fraction contribution %")) +
        facet_grid(Factor ~., scales = "free_y") +
        # pretty function, y start from 0, and 3 breaks
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + 
        # set breaks every X months from the start month to the end one
        scale_x_date(
          breaks = seq(from = start_year, to = end_year, by = "6 month"),
          date_labels = ("%Y-%m")) +
        geom_text(data = middle_positions_frac_ts, size = 4,
                  aes(x = middle_day, y = text_y_frac_day, label = Factor_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        scale_color_manual(values = col_npg) +
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "none"
        )
      # daily_percent_plot
      
      
      ## overall contribution with CI
      setDT(ts_conc_plot)
      overall_conc_ci =
        ts_conc_plot[, .(
          Concentration_med = quantile(Concentration, 0.5),
          Concentration_up = quantile(Concentration, 0.9),
          Concentration_down = quantile(Concentration, 0.1),
          Concentration_mean = mean(Concentration),
          Concentration_sd = sd(Concentration)
        ), 
        by = .(Factor)]
      
      overall_frac_ci =
        ddply(base_day_percent_plot, 
              .(Factor),
              summarise,
              Fraction_med = quantile(Percent, 0.5),
              Fraction_up = quantile(Percent, 0.9),
              Fraction_down = quantile(Percent, 0.1),
              Fraction_mean = mean(Percent),
              Fraction_sd = sd(Percent))
      
      overall_conc_frac_ci =
        merge(overall_conc_ci, setDT(overall_frac_ci))
      View(overall_conc_frac_ci)
      
      overall_conc_frac_ci$Concentration = 
        paste(round(overall_conc_frac_ci$Concentration_mean, 1), "±",
              round(overall_conc_frac_ci$Concentration_sd, 1))
      
      overall_conc_frac_ci$Fraction = 
        paste(round(overall_conc_frac_ci$Fraction_mean, 1), "±",
              round(overall_conc_frac_ci$Fraction_sd, 1))
      
      # annual
      year_percent_plot <-
        ggplot(ts_annual_fraction,
               aes(x = Year, y = Percent,
                   group = Factor, color = Factor)) + # group = 1
        geom_errorbar(aes(ymin = percent_down, ymax = percent_up),
                      width = 0.15) +
        geom_point(size = 1.2) +
        # the preset format_variable function, for sub-/super- scripts
        labs(x = "Date", y = format_variable("Fraction contribution %")) +
        facet_grid(Factor ~., scales = "free_y") +
        # pretty function, y start from 0, and 3 breaks
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + 
        scale_x_continuous(breaks = 
                             pretty(ts_annual_fraction$Year, # pretty{base}
                                    n = length(unique(ts_annual_fraction$Year)), 
                                    min.n = 1)) +
        geom_text(data = middle_positions_frac_ts, size = 4,
                  aes(x = middle_year, y = text_y_frac_yr, label = Factor_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        scale_color_manual(values = col_npg) +
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "none"
        )
      # year_percent_plot
      
      # monthly
      month_percent_plot <-
        ggplot(ts_month_fraction,
               aes(x = Month, y = Percent,
                   group = Factor, color = Factor)) + # group = 1
        geom_errorbar(aes(ymin = percent_down, ymax = percent_up),
                      width = 0.15) +
        geom_point(size = 1.2) +
        # the preset format_variable function, for sub-/super- scripts
        labs(x = "Date", y = format_variable("Fraction contribution %")) +
        facet_grid(Factor ~., scales = "free_y") +
        # pretty function, y start from 0, and 3 breaks
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + 
        scale_x_continuous(breaks = 
                             pretty(ts_month_fraction$Month, # pretty{base}
                                    n = length(unique(ts_month_fraction$Month)), 
                                    min.n = 1)) +
        geom_text(data = middle_positions_frac_ts, size = 4,
                  aes(x = 6.5, y = text_y_frac_mon, label = Factor_contr),
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        scale_color_manual(values = col_npg) +
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "none"
        )
      # month_percent_plot
      
      # Year_Month
      ym_percent_plot <-
        ggplot(ts_year_month_fraction,
               aes(x = Year_Month, y = Percent,
                   group = Factor, color = Factor)) + # group = 1
        geom_errorbar(aes(ymin = percent_down, ymax = percent_up),
                      width = 0.15) +
        geom_point(size = 1.2) +
        # the preset format_variable function, for sub-/super- scripts
        labs(x = "Date", y = format_variable("Fraction contribution %")) +
        facet_grid(Factor ~., scales = "free_y") +
        # pretty function, y start from 0, and 3 breaks
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) + 
        # set breaks every X months from the start month to the end one
        scale_x_date(
          breaks = seq(from = start_year, to = end_year, by = "6 month"),
          date_labels = ("%Y-%m")) +
        geom_text(data = middle_positions_frac_ts, size = 4,
                  aes(x = middle_day, y = text_y_frac_yr_mon, label = Factor_contr), 
                  inherit.aes = FALSE, vjust = -0.2, hjust = 0.5) + # , fontface = "bold"
        scale_color_manual(values = col_npg) +# ggsci
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "none"
        )
      # ym_percent_plot
      
      #### Theil-Sen analyses ####
      
      ### estimate the slope to identify annual change 
      # concentration-based slope
      slope_annual_diff_conc_ts <- 
        ts_annual_conc %>%
        group_by(Factor) %>% 
        dplyr::summarize(
          diff_slope = get_slope_ts(cur_data(), "Year", "Concentration"),
          .groups = "drop"
        ) %>%
        ungroup()
      names(slope_annual_diff_conc_ts)[2] = paste0(names(slope_annual_diff_conc_ts)[2], "_conc")
      
      get_slope_ts <- function(df, x, y) {
        formula <- as.formula(paste(y, "~", x))
        model_ts <- theilsen(formula, data = df)
        slope_ts <- coef(model_ts)[x]
        return(slope_ts)
      }
      
      model_ts <- theilsen(Concentration ~ Year, 
                           data = subset(ts_annual_conc, Factor == "Factor1"))
      ggplot(subset(ts_annual_conc, Factor == "Factor1"), aes(x = Year, y = Concentration)) +
        geom_point(color = "blue") +  # Scatter plot points
        geom_errorbar(aes(ymin = conc_down, ymax = conc_up),
                      width = 0.15, alpha = 0.8) + 
        geom_abline(intercept = model_ts$coefficients[1], slope = model_ts$coefficients[2], color = "red") +  # Theil-Sen trend line
        labs(title = "Concentration vs Year with Theil-Sen Trend Line",
             x = "Year",
             y = "Concentration") +
        theme_minimal(base_size = 16)
      
      
      # percent-based slope
      slope_annual_diff_perc_ts <- 
        ts_annual_fraction %>%
        group_by(Factor) %>% 
        dplyr::summarize(
          diff_slope = get_slope_ts(cur_data(), "Year", "Percent"),
          .groups = "drop"
        ) %>%
        ungroup()
      names(slope_annual_diff_perc_ts)[2] = paste0(names(slope_annual_diff_perc_ts)[2], "_perc")
      
      slope_annual_diff_ts = merge(slope_annual_diff_conc_ts, slope_annual_diff_perc_ts)

      ### estimate the slope to identify month change 
      # concentration-based slope
      slope_month_diff_conc_ts <- 
        ts_month_conc %>%
        group_by(Factor) %>% 
        dplyr::summarize(
          diff_slope = get_slope_ts(cur_data(), "Month", "Concentration"),
          .groups = "drop"
        ) %>%
        ungroup()
      names(slope_month_diff_conc_ts)[2] = paste0(names(slope_month_diff_conc_ts)[2], "_conc")
      
      # percent-based slope
      slope_month_diff_perc_ts <- 
        ts_month_fraction %>%
        group_by(Factor) %>% 
        dplyr::summarize(
          diff_slope = get_slope_ts(cur_data(), "Month", "Percent"),
          .groups = "drop"
        ) %>%
        ungroup()
      names(slope_month_diff_perc_ts)[2] = paste0(names(slope_month_diff_perc_ts)[2], "_perc")
      
      slope_month_diff_ts = merge(slope_month_diff_conc_ts, slope_month_diff_perc_ts)
      
      
      #### combine concentration & fraction plot ####
      # combine two figures into one with grid.arrange{gridExtra}
      
      daily_ts_plot <- 
        grid.arrange(daily_conc_plot, daily_percent_plot, ncol = 2)
      
      month_ts_plot <- 
        grid.arrange(month_conc_plot, month_percent_plot, ncol = 2)
      
      year_ts_plot <- 
        grid.arrange(year_conc_plot, year_percent_plot, ncol = 2)
      
      ym_ts_plot <- 
        grid.arrange(ym_conc_plot, ym_percent_plot, ncol = 2)
      
      #### Overall factor percent Normalize_contri #### 
      # lm_beta_plot_use = subset(lm_beta_plot, Source.No != "F")
      lm_beta_plot_use = lm_beta_plot
      
      lm_beta_nm_contri = select(lm_beta_plot, 
                                 Factor, Factor, 
                                 Factor.contribution, Factor.contr)
      lm_beta_nm_contri$Fraction_data = "NM_contribution"
      
      lm_beta_conc_fr = select(lm_beta_plot, 
                               Factor, Factor, 
                               Fractrion_conc_based, Faraction_conc_contri)
      lm_beta_conc_fr$Fraction_data = "Concentration_base"
      
      colnames(lm_beta_conc_fr)[3:4] = colnames(lm_beta_nm_contri)[3:4]
      # contri_fraction = rbind(lm_beta_conc_fr, lm_beta_nm_contri)
      
      ### fraction contribution estimated based on concentration contribution
      overall_contri <-
        ggplot(lm_beta_plot_use, 
               aes(x = Factor, y = Fractrion_conc_based)) +
        geom_bar_pattern(aes(fill = Factor),
                         stat = "identity",
                         pattern_color = "white",
                         pattern_fill = "white",
                         # alpha = 0.8,
                         width = 0.3)  +
        geom_text(aes(label = paste(Factor, Faraction_conc_contri)), 
                  size = 6, angle = 90, hjust = 0, vjust = -3,
                  position = position_stack(vjust = 0)) + # start from same bottom level
        scale_fill_manual(values = col_npg) +
        scale_y_continuous(position = "right") +
        theme_base() +
        theme(panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              # remove the backgroup line
              plot.background = element_rect(color = NA),
              legend.position = "none",
              axis.text.x = element_text(color="grey25", size = 16, angle = 90, hjust = 0.5, vjust = 0.5), 
              axis.text.y = element_text(color="grey25", size = 16, angle = 90, hjust = 5, vjust = -0.5),
              axis.title.x = element_text(color="grey25", size = 22, angle = 180, hjust = 0.5),
              axis.title.y = element_text(color="grey25", size = 0, angle = -90, vjust = -1))
      # overall_contri
      
      ####### G-space plot, pairs: between-factor scattor & correlation & summary form #######
      
      # Use captured pch and col outside the function 
      point_char = 19
      point_size = 1
      point_alpha = 0.3
      point_col <- base_ts_nmContri_spread$color
      base_contribute_pair = base_ts_nmContri_spread[, 1:factor.No]
      
      # Initialize an empty data frame to store results
      correl_r_p <- 
        data.frame(Var1 = character(), Var2 = character(), 
                   Correlation = numeric(), P_Value = numeric(), 
                   stringsAsFactors = FALSE)
      
      
      # Iterate over all pairs of variables
      for (i in 1:(ncol(base_contribute_pair) - 1)) {
        for (j in (i + 1):ncol(base_contribute_pair)) {
          corr_result <- cor.test(base_contribute_pair[[i]], 
                                  base_contribute_pair[[j]])
          
          correl_r_p <- rbind(correl_r_p, 
                              c(colnames(base_contribute_pair)[i], 
                                colnames(base_contribute_pair)[j], 
                                round(corr_result$estimate, digits=3), 
                                round(corr_result$p.value, digits=5)))
        }
      }
      
      colnames(correl_r_p) <- c("Var1", "Var2", "Correlation", "P_Value")
      correl_r_p$site.serial = site.serial
      
      correl_r_p$factor.No = factor.No
      correl_r_p$Dataset = site.serial
      
      correl_r_percentile <- quantile(as.numeric(correl_r_p$Correlation), 
                                      probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))
      
      correl_r_p_summary = rbind(correl_r_p_summary, correl_r_p)
      
      # Pair plotting
      pdf(paste0(site.serial.factor.pre, "G-space.pdf"))
      
      # par(oma = c(4, 4, 6, 10), mar = c(4, 4, 2, 2)) 
      pairs(base_ts_nmContri_spread[, 1:factor.No], 
            upper.panel = panel.scatter,
            lower.panel = panel.corr,
            main = name.prefix)
      
      dev.off()
      
      
      ####### Daily time-series source concentration - prediction vs. observation #######
      
      predict_daily_species_conc$Date = site_date_PM_species_conc$Date
      predict_daily_species_conc =
        predict_daily_species_conc %>%
        relocate(Date, .before = Serial.No) %>%
        select(-Serial.No)
      
      predict_daily_species_long =
        predict_daily_species_conc %>% 
        pivot_longer(
          # apply to all numeric columns, instead of specify the columns (Al is not always the first one)
          cols = where(is.numeric), #Al:PM2.5, 
          names_to = c("Species"),
          values_to = "Concentration"
        )
      predict_daily_species_long$Data = "PMF_Predictions"
      
      obs_daily_species_long =
        site_date_PM_species_conc %>% 
        pivot_longer(
          # apply to all numeric columns, instead of specify the columns (Al is not always the first one)
          cols = where(is.numeric), #Al:PM2.5, 
          names_to = c("Species"),
          values_to = "Concentration"
        )
      obs_daily_species_long$Data = "Observations"
      
      obs_daily_species_long$SiteCode = obs_daily_species_long$State = NULL
      # summary(predict_daily_species_long$Date == obs_daily_species_long$Date) 
      # summary(predict_daily_species_long$Species == obs_daily_species_long$Species)
      
      ### RMSE estimation
      
      pred_obs_species_wide =
        cbind(dplyr::select(predict_daily_species_long, -Data), 
              dplyr::select(obs_daily_species_long, -c("Data", "Date", "Species")))
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
      
      # extract info for PM2.5
      RMSE_PM2.5 = pred_obs_compare$RMSE[pred_obs_compare$Species == "PM2.5"]
      R2_PM2.5 = pred_obs_compare$R2[pred_obs_compare$Species == "PM2.5"]
      
      ### time-series plotting
      pred_obs_species_conc = 
        rbind(predict_daily_species_long, obs_daily_species_long)
      pred_obs_species_conc$Date = as.Date(pred_obs_species_conc$Date)
      
      ### time-series
      # set gap in figure if no data for >7 days
      pred_obs_species_conc <- 
        pred_obs_species_conc %>%
        arrange(Date) %>%
        mutate(gap = c(0, diff(as.numeric(Date))) > 7, # calculate gaps, convert Date to numeric for diff
               group = cumsum(gap)) # Cumulative sum to create a new group after each gap
      
      pred_obs_species_position <-
        ddply(pred_obs_species_conc, .(Species),
              summarise,
              y_position = quantile(Concentration, 0.999),
              x_position = median(Date))
      
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
      
      pred_obs_species_position = 
        join(pred_obs_species_position, pred_obs_species_med)
      
      pred_obs_species_conc_plot <-
        ggplot(pred_obs_species_conc, 
               aes(x = Date, y = Concentration, 
                   group = group, color = Data)) + # group = 1
        geom_line(linewidth = 0.2, alpha = 0.6) +
        geom_point(size = 0.7) +
        labs(x = "Date", y = format_variable("Concentration µg/m3")) +
        facet_wrap(Species ~., scales = "free_y", ncol = 3) + # ncol, only for facet_wrap, not facet_grid
        scale_y_continuous(limits = c(0, NA), 
                           breaks = function(x) pretty(x, n = 3)) +  # reduce the number of breaks
        scale_color_manual(values = c("#377eb8", "#ff7f00")) + # c("steelblue", "brown2"), c("#377eb8", "#ff7f00")
        geom_text(data = pred_obs_species_position, size = 3.8,
                  aes(x = x_position, y = y_position, label = Species_conc), 
                  inherit.aes = FALSE) + 
        theme_base() +
        theme(
          panel.grid = element_line(colour = "white"),
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 11),
          strip.background = element_blank(), strip.text = element_blank(),
          legend.position.inside = c(0.9, 0.02), legend.background = element_blank(),
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
              x_position = max(Observations)*0.9,
              y_position = max(PMF_Predictions)*0.9)
      
      species_pred_vs_obs_plot <-
        ggplot(species_conc_pred_vs_obs,
               aes(x = Observations,
                   y = PMF_Predictions)) +
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
          # remove the backgroup line
          plot.background = element_rect(color = NA),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12))
      
      # species_pred_vs_obs_plot
    
      
      ####### Daily & species-specific scaled residuals #######
      
      daily_species_scale_residual_long =
        data.frame(Date = site_date_PM_species_conc$Date, daily_species_scale_residual) %>% 
        pivot_longer(
          cols = where(is.numeric), #Al:PM2.5, 
          names_to = c("Species"),
          # names_pattern = "new_?(.*)_(.)(.*)",
          values_to = "Scaled_residuals"
        )
      
      # merge & show the setting of strong_weak classification
      daily_species_scale_residual_long =
        merge(daily_species_scale_residual_long, strong_weak_df)
      daily_species_scale_residual_long$Species_sw = 
        paste0(daily_species_scale_residual_long$Species, "-",
               daily_species_scale_residual_long$strong_weak)
      
      # library(ggh4x), change strip color
      # strip <- strip_themed(background_x = elem_list_rect(fill = c("gold", "tomato")
      # facet_wrap2()
      
      ### histogram for all species
      all_species_residual <-
        ggplot(daily_species_scale_residual_long, 
               aes(x = Scaled_residuals)) +
        geom_histogram(binwidth = 0.5, alpha = 0.8) +
        facet_wrap(~ Species_sw, ncol = 4) + # labeller = as_labeller(format_variable)
        geom_vline(xintercept = c(-3, 3), color = "cyan3", linewidth = 0.5) +
        scale_x_continuous(breaks = function(x) unique(c(-3, 3, pretty(x)))) +
        ggtitle("Original Scale") +
        # theme(strip.text = element_text(family = "Arial Unicode MS")) +
        theme_bw()
      
      all_species_residual_zoom <-
        all_species_residual +
        # not ylim directly, now keep the bars of which the values > y up limit
        coord_cartesian(ylim = c(0, 50)) + # ylim(0, 50) +
        ggtitle("Zoom-in Scale")
      
      # combine two figures into one with plot_layout{patchwork}
      all_species_residual_plot <- 
        all_species_residual + 
        all_species_residual_zoom + 
        plot_layout(ncol = 2)
      
      ### time series of daily Q/Qexp
      # set gap in date if there is no data for > 15 days
      daily_Q_Qexp$Date = as.Date(daily_Q_Qexp$Date)
      daily_Q_Qexp <- 
        daily_Q_Qexp %>%
        arrange(Date) %>%
        mutate(gap = c(0, diff(as.numeric(Date))) > 15, # calculate gaps, convert Date to numeric for diff
               group = cumsum(gap)) # Cumulative sum to create a new group after each gap
      
      daily_Q_Qexp_plot <-
        ggplot(daily_Q_Qexp, 
               aes(x = Date, y = Q_Qexp_ratio)) + # group = 1
        geom_line(aes(group = group), color = "lightblue") +
        geom_point(color = "slategray", size = 1.2) +
        labs(y = "Q/Qexp Ratio") +
        theme_bw()  +
        theme(axis.title.x = element_text(size = 0), 
              axis.title.y = element_text(color="grey25", size = 14, vjust=1),
              axis.text.x = element_text(color="grey25", size = 12), 
              axis.text.y = element_text(color="grey25", size = 12))
      
      ### species-specific Q/Qexp
      species_Q_Qexp = join(species_Q_Qexp, species_class)
      species_Q_Qexp_plot <-
        ggplot(species_Q_Qexp, 
               aes(x = reorder(Species, sequence), 
                   y = Q_Qexp_ratio)) +
        geom_hline(yintercept = 1, color = "brown2", linetype = "dashed", linewidth = 0.5) +
        geom_bar(stat = "identity", fill = "steelblue", width = 0.4) +
        scale_x_discrete(labels = function(x) format_variable(x)) +
        labs(y = "Q/Qexp Ratio") +
        theme_bw()+
        theme(axis.title.x = element_text(size = 0), 
              axis.title.y = element_text(color="grey25", size = 14, family = "Arial Unicode MS"),
              axis.text.x = element_text(color="grey25", size = 12, family = "Arial Unicode MS",
                                         angle = 90, hjust = 0, vjust = 0.3), 
              axis.text.y = element_text(color="grey25", size = 12, family = "Arial Unicode MS"))
      
      Q_Qexp_plot = daily_Q_Qexp_plot / species_Q_Qexp_plot # patchwork{}
      
      pred_obs_compare_resd =
        merge(pred_obs_compare, 
              select(species_Q_Qexp, 
                     Species, Q_Qexp_ratio, higher.3.scalRes.per))
      
      pred_obs_compare_resd = 
        data.frame(site.serial = site.serial, factor.No = factor.No, 
                   pred_obs_compare_resd)
      
      pred_obs_compare_summary =
        rbind(pred_obs_compare_summary, pred_obs_compare_resd)
      
      ####### output data summary #######
      
      base_oneFactor = data.frame(serial.No = site.serial, Factor = factor.No, 
                                  start_date = start.date, end_date = end.date,
                                  median_PMF_PM2.5 = median_PMF_PM2.5, 
                                  median_obs_PM2.5 = median_obs_PM2.5, 
                                  cor_PMF.obs_PM = cor.PMF.obs.PM,
                                  RMSE_PM2.5 = RMSE_PM2.5, R2_PM2.5 = R2_PM2.5, 
                                  Correlation.lm.conc.fraction = cor.two.fraction.contri,
                                  Qmin = lowest_Qm, Qmin_task_No = lowest_Qm_taskNo, 
                                  Q.exp = Q.exp, Q.true = Q.true, Q.robust = Q.robust,
                                  converge_percent = converge_percent,
                                  count.strong.species = strong.species.count, 
                                  count.all.species = species.weak.strong.count,
                                  day.count = site.data.row,
                                  DISP_error.code = disp.error.code, DISP_qdrop = disp.qdrop, 
                                  bs.map = bs_overall_map)
      
      summary_base = rbind(summary_base, base_oneFactor[1, ])
      
      write.csv(summary_base, paste0("NJ_YZ_", "base_DISP_summary.csv"))
      write.csv(correl_r_p_summary, paste0("NJ_YZ_", "base_factor_correlation.csv"))
      write.csv(pred_obs_compare_summary, paste0("NJ_YZ_", "PMF_vs_obs.csv"))
      
      ####### Output files #######
      
      ggsave(paste0(site.serial.factor.pre, "daily.pdf"), plot = daily_ts_plot, width = 13, height = 8)
      ggsave(paste0(site.serial.factor.pre, "month.pdf"), plot = month_ts_plot, width = 11, height = 7)
      ggsave(paste0(site.serial.factor.pre, "annual.pdf"), plot = year_ts_plot, width = 11, height = 7)
      ggsave(paste0(site.serial.factor.pre, "_year_month.pdf"), plot = ym_ts_plot, width = 13, height = 8)
      ggsave(paste0(site.serial.factor.pre, "overall.pdf"), plot = overall_contri, width = 9, height = 5)
      # ggsave(paste0(name.prefix, "source-PM.pdf"), plot = PM_source_daily, width = 9, height = 7)
      ggsave(paste0(site.serial.factor.pre, "source_profile.pdf"), plot = source_profile, width = 5.8, height = 6,
             device = cairo_pdf) # device = cairo_pdf, save the special fonts (super-/sub- script) in pdf
      ggsave(paste0(site.serial.factor.pre, "Q_Qexp.pdf"), plot = Q_Qexp_plot, width = 9, height = 5,
             device = cairo_pdf) # download xquartz for use of device = cairo_pdf.
      ggsave(paste0(site.serial.factor.pre, "species_residual.pdf"), plot = all_species_residual_plot, width = 14.5, height = 9)
      ggsave(paste0(site.serial.factor.pre, "obs_pred_ts.pdf"), plot = pred_obs_species_conc_plot, width = 14.5, height = 9)
      ggsave(paste0(site.serial.factor.pre, "obs_vs_pred.pdf"), plot = species_pred_vs_obs_plot, width = 9, height = 9)
      
      
      #### prepare data outputs
      conc_percent_bsDisp_output = conc_percent_bsDisp
      lm_beta_plot_output = lm_beta_plot
      # lm_beta_plot_output
      
      ### merge concentration & percent data of different temporal scales
      ts_daily_output = merge(ts_conc_plot, base_day_percent_plot)
      ts_daily_output$group = ts_daily_output$gap = NULL
      # dim(ts_conc_plot); dim(base_day_percent_plot); dim(ts_daily_output)
      
      ts_annual_output = merge(ts_annual_conc, ts_annual_fraction)
      # dim(ts_annual_conc); dim(ts_annual_fraction); dim(ts_annual_output)
      ts_year_month_output = merge(ts_year_month_conc, ts_year_month_fraction)
      # dim(ts_month_conc); dim(ts_month_fraction); dim(ts_month_output)
      ts_year_month_output$Year = year(ts_year_month_output$Year_Month)
      ts_year_month_output$Month = month(ts_year_month_output$Year_Month)
      
      ### add site.serial & factor.No info
      conc_percent_bsDisp_output$site.serial = lm_beta_plot_output$site.serial = 
        ts_daily_output$site.serial = ts_annual_output$site.serial = 
        ts_year_month_output$site.serial = daily_species_scale_residual$site.serial = 
        daily_Q_Qexp$site.serial = species_Q_Qexp$site.serial = 
        site.serial
      conc_percent_bsDisp_output$Factor.No = lm_beta_plot_output$Factor.No = 
        ts_daily_output$Factor.No = ts_annual_output$Factor.No = 
        ts_year_month_output$Factor.No = daily_species_scale_residual$factor.No = 
        daily_Q_Qexp$factor.No = species_Q_Qexp$factor.No = 
        factor.No
      
      # output csv files
      write.csv(lm_beta_plot_output, paste0(site.serial.factor.pre, "overall.csv"))
      write.csv(conc_percent_bsDisp_output, paste0(site.serial.factor.pre, "source_profile.csv"))
      
      write.csv(ts_daily_output, paste0(site.serial.factor.pre, "daily.csv"))
      write.csv(ts_annual_output, paste0(site.serial.factor.pre, "annual.csv"))
      write.csv(ts_year_month_output, paste0(site.serial.factor.pre, "month.csv"))
      
      write.csv(daily_species_scale_residual, paste0(site.serial.factor.pre, "species_residual.csv"))
      write.csv(daily_Q_Qexp, paste0(site.serial.factor.pre, "daily_Q_Qexp.csv"))
      write.csv(species_Q_Qexp, paste0(site.serial.factor.pre, "species_Q_Qexp.csv"))
      
    }, error=function(e) {
      print(paste("Error at iteration", site.serial, "factor", factor.No, ":", e$message))
    })
  }
}

