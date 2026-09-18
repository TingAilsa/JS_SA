
setwd("/Users/TingZhang/Documents/EPA PMF/Data")
getwd()
data.dir <- "/Users/TingZhang/Documents/EPA PMF/Data"

#### Libraries to use #### 
library(tidyr)
library(dplyr)
library(plyr)
library(base)
library(lubridate)
library(readxl)
library(scales)
library(stringr)
library(purrr)

#### read & process data #####
# read the concentration csv after manually replace the extremes by monthly mean
# conc_aft_exe = read.csv("NJ_NJUdata_all_extremes_replaced_4.5unc_DN_conc.csv")
conc_aft_exe = read.csv("NJ_NJUdata_no_extremes_4.5unc_DN_ReplaceAllExe&lessExe_conc.csv") # file after manually replacing part of the extreme values
conc_org = read.csv("NJ_NJUdata_no_extremes_4.5unc_intp_DN_conc.csv") # original file, by extremes have been partially removed earlier in batch
conc_aft_exe$X1 = conc_org$X1 = conc_aft_exe$X = conc_org$X = NULL

# get the dispersion normalization coefficient
dn_coef = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/NJ_NJUdata_PMF/NJU_DN_coefficient_mean_1523.29501541806.csv")
dn_coef$X = NULL
dn_coef_mean = 1523.29501541806

dn_coef$VC_coef_use = dn_coef$VC / dn_coef_mean

# Date format
conc_aft_exe$Date = as.Date(conc_aft_exe$Date, format = "%m/%d/%y")
conc_org$Date = as.Date(conc_org$Date)

# double check if column names & dates are same
summary(names(conc_org) == names(conc_aft_exe))
summary(conc_aft_exe$Date == conc_org$Date)

#### identify data that have been manually revised #####
conc_with_changed_specied = read.csv("NJ_NJUdata_all_extremes_replaced_4.5unc_DN_conc.csv")

# check and combine the information of reestimated cells
conc_aft_cells <-
  select(conc_with_changed_specied, Mostly_Zn, Most_exe, More_exe_lessHigh) %>%
  mutate(replace_conc =
           pmap_chr(list(Mostly_Zn, Most_exe, More_exe_lessHigh),
                    function(zn, exe, lessHigh) {
    # Combine non-empty strings into a vector, collapse them into a single string separated by ", "
    combined <- str_c(c(zn, exe, lessHigh)[c(zn, exe, lessHigh) != ""], collapse = ", ")
    # Return combined string or an empty string if combined is empty
    if_else(combined == "", "", combined)
  }))

unique(conc_aft_cells$replace_conc)


# Creating the new dataframe org_vs_aftExe to store comparison results of whether values are the same
# org_vs_aftExe <- conc_org == conc_aft_exe is not working due to the floating points
# the floating-point issue appears because there exist many very low value, need to first set a tolerance for floating point comparison
tolerance <- 1e-06
org_vs_aftExe <- conc_org > (conc_aft_exe + tolerance)

# convert matrix to df
org_vs_aftExe = data.frame(org_vs_aftExe)
org_vs_aftExe$Date = conc_org$Date

# manual validate the results stored in org_vs_aftExe
org_vs_aftExe[231:236, 1:8]; conc_org[231:236, 1:8]; conc_aft_exe[231:236, 1:8]
org_vs_aftExe[149:155, 12:19]; conc_org[149:155, 12:19]; conc_aft_exe[149:155, 12:19]


#### compare with data before dispersion normalization #####

# convert concentration to before normalization
conc_noExe_bef_dn = conc_aft_exe
for (col in names(org_vs_aftExe)) {
  if (col != "Date") {
    conc_noExe_bef_dn[[col]] =
      conc_noExe_bef_dn[[col]] / dn_coef$VC_coef_use
  }
}
head(conc_noExe_bef_dn)

# comapre with real non DN concentrations
conc_no_dn = read.csv("NJ_NJUdata_no_extremes_4.5unc_intp_conc.csv")
conc_no_dn$Date = as.Date(conc_no_dn$Date)
summary(conc_noExe_bef_dn$PM2.5 == conc_no_dn$PM2.5)
cor(conc_noExe_bef_dn$PM2.5, conc_no_dn$PM2.5)
conc_noExe_bef_dn$PM2.5[1:10]; conc_no_dn$PM2.5[1:10]
conc_noExe_bef_dn$Al[1:10]; conc_no_dn$Al[1:10]

#### re-estimate the uncertainty #####

# MDL is from EPA, Beizhan
mdl_k = read_excel("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/LOD_elements_IC_ions/LODProcessing_ICPMS_element.xlsx")
mdl_k = plyr::rename(mdl_k, 
                     c("Cl-" = "ClIon",
                       "NO3-" = "NO3Ion",
                       "SO4 2-" = "SO4Ion",
                       "NH4+" = "NH4Ion",
                       "K+" = "KIon",
                       "Na+" = "NaIon",
                       "NH4+" = "NH4Ion"))


#### Identify and save common columns in conc and mdl_k, and reorder

# common columns
common_columns <- intersect(colnames(conc_noExe_bef_dn), colnames(mdl_k))
mdl_k_common_ordered = mdl_k[, common_columns]

# check to ensure the order matches conc_noExe_bef_dn
summary(names(mdl_k_common_ordered) == names(conc_noExe_bef_dn)[2:ncol(conc_noExe_bef_dn)])

mdl_k_common_ordered$data = c("LOD ug/m3", "K value")
mdl_k_common_ordered = relocate(mdl_k_common_ordered, "data", .before = "PM2.5")

# expand mdl data to be same rows as concentration data for each variable
mdl_k_common_expanded = 
  mdl_k_common_ordered[rep(row.names(mdl_k_common_ordered), 
                           each = nrow(conc_noExe_bef_dn)), ]

# subset LOD & error fraction
comp_mdl = subset(mdl_k_common_expanded, data == "LOD ug/m3")
comp_k = subset(mdl_k_common_expanded, data == "K value")
comp_mdl$data = comp_k$data = NULL

sapply(conc_noExe_bef_dn, class)
sapply(comp_mdl, class)

# compare concentration and MDL of a given component
conc_noExe_bef_dn_species = conc_noExe_bef_dn[, 2:ncol(conc_noExe_bef_dn)]
conc_vs_mdl = data.frame(Map(">", conc_noExe_bef_dn_species, comp_mdl))
sapply(conc_vs_mdl, class)
dim(conc_vs_mdl)
dim(comp_mdl)

# assign data conditionally to concentration and uncertainty values for PMF
conc_pmf = conc_vs_mdl * conc_noExe_bef_dn_species +
  (!conc_vs_mdl) * comp_mdl * 0.5
unc_pmf = conc_vs_mdl * (((comp_mdl / 2)^2 + (comp_k * conc_noExe_bef_dn_species)^2)^0.5) +
  (!conc_vs_mdl) * 5/6 * comp_mdl
unc_pmf$PM2.5 = 3 * conc_noExe_bef_dn_species$PM2.5

### extra 4.5 times uncertainty for manually interpolated values based on DN dataset & prior PMF runs
points_intp_aft_DN = org_vs_aftExe[, 2:ncol(org_vs_aftExe)]

points_intp_aft_DN[points_intp_aft_DN == TRUE] = 4.5
points_intp_aft_DN[points_intp_aft_DN == FALSE] = NA

unc_intp_aft_DN = unc_pmf * points_intp_aft_DN

### gen uncertainty of data before replacement following DN-PMF tries
unc_pre_DN = read.csv("NJ_NJUdata_no_extremes_4.5unc_intp_unc.csv")
unc_pre_DN$Date = as.Date(unc_pre_DN$Date)

# detect uncertainty to be replaced
unc_replace_aft_DN = org_vs_aftExe[, 2:ncol(org_vs_aftExe)]

unc_replace_aft_DN[unc_replace_aft_DN == TRUE] = NA
unc_replace_aft_DN[unc_replace_aft_DN == FALSE] = 1

unc_intp_bef_DN = unc_pre_DN[, 2:ncol(unc_pre_DN)] * unc_replace_aft_DN

### fill all NAs in unc_intp_bef_DN with real values from unc_intp_aft_DN
unc_final_DN = unc_intp_bef_DN

unc_final_DN[is.na(unc_final_DN)] <- unc_intp_aft_DN[is.na(unc_final_DN)]
summary(unc_final_DN)

# add date
unc_final_DN$Date = conc_noExe_bef_dn$Date
unc_final_DN = 
  relocate(unc_final_DN, Date, .before = Al)
names(unc_final_DN)

#### dispersion normalization for new conc & unc ####
conc_final_DN = conc_noExe_bef_dn

for (col in names(conc_final_DN)) {
  if (col != "Date") {
    conc_final_DN[[col]] =
      conc_final_DN[[col]] * dn_coef$VC_coef_use
    unc_final_DN[[col]] =
      unc_final_DN[[col]] * dn_coef$VC_coef_use
  }
}

summary(conc_final_DN); summary(conc_org)


#### output result ####

write.csv(unc_final_DN, 
          "NJ_NJUdata_no_extremes_4.5unc_DN_ReplaceAllExe&lessExe_unc.csv",
          row.names = FALSE)

