# work path
# setwd("/Users/ztttttt/Documents/PMF/NJ_NJUdata_PMF")
# data.dir <- "/Users/ztttttt/Documents/PMF/NJ_NJUdata_PMF"
# getwd()


setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/NJ_NJUdata_PMF")
data.dir <- "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/NJ_NJUdata_PMF"
getwd()


# packages
library(dplyr)
library(purrr)
library(zoo)
library(lubridate)
library(ggplot2)
library(readxl)
library(data.table)

#### Functions ####

# Function to replace NA with geometric mean
na_to_geom_mean <- function(x) {
  if(all(is.na(x))) return(x) # Return if all values are NA
  gm <- exp(mean(log(x), na.rm = TRUE)) # Geometric mean, excluding NA
  replace(x, is.na(x), gm) # Replace NA with geometric mean
}

# Function to replace negatives with half of the lowest positive (half of MDL)
negatives_to_halfMDL <- function(x) {
  # Find the lowest positive value in the column
  lowest_positive <- min(x[x > 0], na.rm = TRUE)
  
  # Calculate half of the lowest positive value
  half_lowest_positive <- lowest_positive / 2
  
  # Replace negative values with half of the lowest positive value
  x[x < 0] <- half_lowest_positive
  
  # Return the modified column
  return(x)
}

# Function to replace the row number, variable number, and factor number
row_var_factor <- function(params_file, row.number, var.number, factor.number, unc.level) {
  line.detect <- line_number(params_file, "n1,")
  line_to_replace <- params_file[line.detect + 1]
  
  line_replaced_n1 <- replace_nth_segment(line_to_replace, 1, row.number) # n1
  line_replaced_n12 <- replace_nth_segment(line_replaced_n1, 2, var.number) # n2
  line_replaced_n12p <- replace_nth_segment(line_replaced_n12, 3, factor.number) # np
  line_replaced_n12pu <- replace_nth_segment(line_replaced_n12p, 5, unc.level) # np
  
  params_file[line.detect + 1] <- line_replaced_n12pu
  return(params_file)
}

#### Reorder columns for datasets with PM2.5 and species  ####

species_col_reorder = function(species_df) {
  
  species_df = plyr::rename(species_df, 
                            c("PM2.5" = "PM25", 
                              "Cl-" = "ClIon",
                              "Cl." = "ClIon",
                              "NO2" = "NO2Ion",
                              "NO2." = "NO2Ion",
                              "NO2-" = "NO2Ion",
                              "NO3" = "NO3Ion",
                              "NO3-" = "NO3Ion",
                              "SO4" = "SO4Ion",
                              "SO4 2-" = "SO4Ion",
                              "Ca2+" = "CaIon",
                              "NH4" = "NH4Ion",
                              "NH4+" = "NH4Ion",
                              "K+" = "KIon",
                              "Na+" = "NaIon",
                              "NH4." = "NH4Ion",
                              "K." = "KIon",
                              "Na." = "NaIon"))
  
  # rearrange the sequence of columns
  OC.EC = c("EC", "EC1", "EC2", "EC3", 
            "OC", "OC1", "OC2", "OC3", "OC4",
            "OP")
  
  ions = c("ClIon", "CaIon", "NaIon", "KIon", "NH4Ion", "NO3", "SO4", "ammNO3", "ammSO4")
  
  pah = c("Fla", "Pyr", "Chry", "BaA", "BbF", "BkF", "BaP", "DahA", "BghiP", "InP")
  
  species_df = species_df %>%
    select(!(matches(OC.EC)), 
           everything())
  
  species_df = species_df %>%
    select(!(matches(ions)), 
           everything())

  species_df = species_df %>%
    select(!(matches(pah)), 
           everything())
  
  # Place PM2.5 at the end
  # species_df = species_df %>% relocate(PM25, .after = SO4Ion)
  species_df = species_df %>% select(-PM25, everything(), PM25)
  
  return(species_df)
}

#### data used for uncertainty estimation ####

# MDL is from EPA, Beizhan
mdl_k = read_excel("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/LOD_elements_IC_ions/LODProcessing_ICPMS_element_pah.xlsx")
mdl_k = species_col_reorder(mdl_k)
names(mdl_k)


#### 2024.02-05 Previous process with extemems, negatives in original data ####

# manully remove peaks if the extreme was high for only one species

# the Si concentration = Al/ratio, the ratio is from Zheng_2019_AE_PM source Nanjing_supplement.docx
# Al/Si ratios is 0.7339688 winter (12,1,2), 0.58658205 spring (3,4,5), 0.71824973 summer (6,7,8), 0.73418882 autumn (9,10,11) 

nju_conc_org = read.csv("NJ_nju_conc.csv")
nju_conc_org = read.csv("NJ_nju_conc_remove_3_Fe.csv")
nju_conc_org = read.csv("NJ_nju_conc_remove_all_extremes.csv")

nju_conc_org$Date = as.Date(as.character(nju_conc_org$Date), format = "%Y%m%d")

# check the histogram
ggplot(nju_conc_org, aes(x=Al)) +
  geom_histogram(bins = 100)

# re-estimate the Si concentration
nju_conc_org$month_num = month(nju_conc_org$Date)

nju_conc_org$al.vs.si = 0.7339688
nju_conc_org$al.vs.si[nju_conc_org$month_num %in% c(3,4,5)] = 0.58658205
nju_conc_org$al.vs.si[nju_conc_org$month_num %in% c(6,7,8)] = 0.71824973
nju_conc_org$al.vs.si[nju_conc_org$month_num %in% c(9,10,11)] = 0.73418882

nju_conc_org$Si = nju_conc_org$Al/nju_conc_org$al.vs.si

nju_conc_org$al.vs.si = nju_conc_org$month_num = NULL

# replace the NAs with the geometric mean of that month
nju_conc_org$month = format(nju_conc_org$Date, "%Y-%m")


nju_conc_org_modified <- 
  nju_conc_org %>%
  dplyr::mutate(across(where(is.numeric), 
                       ~ negatives_to_halfMDL(.x)))

nju_conc_org_noNA <-
  nju_conc_org_modified %>%
  group_by(month) %>%
  dplyr::mutate(across("PM2.5":"EC", 
                       ~ na_to_geom_mean(.x))) %>%
  ungroup() %>%
  select(-month)

nju_conc_org_na = 
  data.frame(Date = nju_conc_org$Date,
             is.na(nju_conc_org[, 2:ncol(nju_conc_org)]))
  

#### Identify and save common columns in conc and mdl_k, and reorder

# common columns
common_columns <- intersect(colnames(nju_conc_org_noNA), colnames(mdl_k))
mdl_k_new = mdl_k[, common_columns]
nju_conc_new = nju_conc_org_noNA[, common_columns]

# ensure the order matches nju_conc_org_noNA
mdl_k_common_ordered <- 
  mdl_k_new[, 
            match(colnames(nju_conc_new), 
                  colnames(mdl_k_new))]

mdl_k_common_ordered$data = c("LOD ug/m3", "K value")
mdl_k_common_ordered = relocate(mdl_k_common_ordered, "data", .before = "PM2.5")

mdl_k_common_expanded = 
  mdl_k_common_ordered[rep(row.names(mdl_k_common_ordered), 
                           each = nrow(nju_conc_new)), ]
comp_mdl = subset(mdl_k_common_expanded, data == "LOD ug/m3")
comp_k = subset(mdl_k_common_expanded, data == "K value")
comp_mdl$data = comp_k$data = NULL

sapply(nju_conc_new, class)
sapply(comp_mdl, class)

# compare concentration and MDL of a given component
conc_vs_mdl = data.frame(Map(">", nju_conc_new, comp_mdl))
sapply(conc_vs_mdl, class)
dim(conc_vs_mdl)
dim(comp_mdl)

# assign data conditionally to concentration and uncertainty values for PMF
conc_pmf = conc_vs_mdl * nju_conc_new +
  (!conc_vs_mdl) * comp_mdl * 0.5
unc_pmf = conc_vs_mdl * (((comp_mdl / 2)^2 + (comp_k * nju_conc_new)^2)^0.5) +
  (!conc_vs_mdl) * 5/6 * comp_mdl
unc_pmf$PM2.5 = 3 * nju_conc_new$PM2.5

### extra 4 times uncertainty for interpolated values

common_columns_2 <- intersect(colnames(nju_conc_new), colnames(nju_conc_org_na))
nju_conc_org_na = nju_conc_org_na[, common_columns_2]
summary(names(nju_conc_org_na) == names(unc_pmf))

nju_conc_org_na[nju_conc_org_na == TRUE] = 4.5
nju_conc_org_na[nju_conc_org_na == FALSE] = 1
nju_conc_org_na$Si = 4.5 * nju_conc_org_na$Si

unc_pmf_intp = unc_pmf * nju_conc_org_na

# insert date into  concentration & uncertainty datasets
conc_pmf$Date = unc_pmf_intp$Date = nju_conc_org_noNA$Date

conc_pmf = relocate(conc_pmf, "Date", .before = "PM2.5")
conc_pmf = relocate(conc_pmf, "PM2.5", .after = "EC")

unc_pmf_intp = relocate(unc_pmf_intp, "Date", .before = "PM2.5")
unc_pmf_intp = relocate(unc_pmf_intp, "PM2.5", .after = "EC")

# reorder the mark of "NA", manually set NA and reinterpolated points
nju_conc_org_na_intp = nju_conc_org_na
nju_conc_org_na_intp$Date = unc_pmf_intp$Date

names(nju_conc_org_na_intp[names(unc_pmf_intp)])
nju_conc_org_na_intp = nju_conc_org_na_intp[names(unc_pmf_intp)]
names(nju_conc_org_na_intp)
write.csv(nju_conc_org_na_intp, "NJU_manual_presetNA_interpolation.csv")


write.csv(conc_pmf, "NJ_NJUdata_conc.csv", row.names = F)
write.csv(unc_pmf_intp, "NJ_NJUdata_unc.csv", row.names = F)

write.csv(conc_pmf, "NJ_NJUdata_remove_3_Fe_conc.csv", row.names = F)
write.csv(unc_pmf_intp, "NJ_NJUdata_remove_3_Fe_unc.csv", row.names = F)

write.csv(conc_pmf, "NJ_NJUdata_remove_all_extremes_conc.csv", row.names = F)
write.csv(unc_pmf_intp, "NJ_NJUdata_remove_all_extremes_unc.csv", row.names = F)

write.csv(conc_pmf, "NJ_NJUdata_no_extremes_4.5unc_intp_conc.csv", row.names = F)
write.csv(unc_pmf_intp, "NJ_NJUdata_no_extremes_4.5unc_intp_unc.csv", row.names = F)

##### prepare data for nonGUI PMF
# combining conc & unc files
conc_pmf_cmd = conc_pmf
unc_pmf_intp_cmd = unc_pmf_intp
conc_pmf_cmd = relocate(conc_pmf_cmd, "PM2.5", .after = "EC")
unc_pmf_intp_cmd = relocate(unc_pmf_intp_cmd, "PM2.5", .after = "EC")
conc_pmf_cmd = relocate(conc_pmf_cmd, "Al", .before = "Mg")
unc_pmf_intp_cmd = relocate(unc_pmf_intp_cmd, "Al", .before = "Mg")

colnames(conc_pmf_cmd) = paste0("conc_", colnames(conc_pmf_cmd))
colnames(unc_pmf_intp_cmd) = paste0("unc_", colnames(unc_pmf_intp_cmd))

cmd_conc_unc = cbind(conc_pmf_cmd, unc_pmf_intp_cmd)

# Interleave columns of concentration and uncertainty files
## generate a new vector to index the columns of cbind file
interleave.cmd <- 
  rep(1:ncol(unc_pmf_intp_cmd), each = 2) + 
  (0:1) * ncol(unc_pmf_intp_cmd)
## reorder the cbind file
cmd_interleave = cmd_conc_unc[interleave.cmd]

cmd_interleave$unc_Date = NULL
colnames(cmd_interleave)[1] = "Date"
head(cmd_interleave)

write.csv(cmd_interleave, "NJ_NJUdata_cmd.csv", row.names = F)

write.csv(cmd_interleave, "NJ_NJUdata_remove_3_Fe_cmd.csv", row.names = F)

write.csv(cmd_interleave, "NJ_NJUdata_remove_all_extremes_cmd.csv", row.names = F)


################################################################################
################################ 2024. 07 ###############################
################################################################################

#### 1. data prepare: Nanjing interpolation based on manully removed points ####

setwd("/Users/TingZhang/Documents/EPA PMF/Data")

###### Nanjing, NJU, Wang Qingeng data
# conc after manually remove extreme values that were never predicted
# 2024.07.09, As & Co not included, cause Cu & Pb have same high values on the same days
dataset = "NJ_NJUdata_"
data_use = "mannual_rmv_extremes_4.5unc_intp"

conc_pmf_mn = read.csv(paste0(dataset, data_use, "_conc_withNA.csv"))
conc_pmf_mn$X1 = NULL
conc_pmf_mn$Date = as.Date(conc_pmf_mn$Date, format = "%m/%d/%y")
head(conc_pmf_mn$Date)

# reorder the columns
conc_pmf_mn = species_col_reorder(conc_pmf_mn)

# get the month
conc_pmf_mn$month = format(conc_pmf_mn$Date, "%Y-%m")

# replace NA by montly mean
conc_pmf_mn_noNA <-
  conc_pmf_mn %>%
  group_by(month) %>%
  dplyr::mutate(across("Al":"PM25", 
                       ~ na_to_geom_mean(.x))) %>%
  ungroup() %>%
  select(-month)
summary(conc_pmf_mn_noNA)

# get positions of data mannually replaced by NA
conc_pmf_mn_na = 
  data.frame(Date = conc_pmf_mn$Date,
             is.na(conc_pmf_mn[, 2:ncol(conc_pmf_mn)]))


####  1. data prepare: Yangzhou, overall, Si based on Si/Al in Nanjing, CDC elements  ####

setwd("/Users/TingZhang/Documents/EPA PMF/Data")

yz_conc_org = fread("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/YZ_data/YZ_element_ion_pah_C_2024.07.csv")
yz_conc_org$V1 = NULL
names(yz_conc_org)[1] = "Date"
dim(yz_conc_org)
yz_conc_org$Date = as.Date(yz_conc_org$Date, format = "%m/%d/%y") #format = "%Y/%m/%d"
yz_conc_org$Date[1]

head(yz_conc_org)

# # check the histogram
# ggplot(yz_conc_org, aes(x=Al)) +
#   geom_histogram(bins = 100)

####### JS CDC 2024.06 update element
yz_element = fread("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/YZ_data/JS_CDC_swt_ICP-MS_elements_2024.06.csv")
names(yz_element)

yz_element$Date[1]
yz_element$Date = as.Date(as.character(yz_element$Date), format = "%Y%m%d")
yz_element$Date[1]

# match with newly measured elements
yz_conc_upd = merge(yz_conc_org, yz_element)
dim(yz_conc_upd)

########## Si estimation
# the Si concentration = Al/ratio, the ratio is from Zheng_2019_AE_PM source Nanjing_supplement.docx
# Al/Si ratios is 0.7339688 winter (12,1,2), 0.58658205 spring (3,4,5), 0.71824973 summer (6,7,8), 0.73418882 autumn (9,10,11) 

# re-estimate the Si concentration
yz_conc_upd$month_num = month(yz_conc_upd$Date)

# ratios of different seasons
yz_conc_upd$al.vs.si = 0.7339688
yz_conc_upd$al.vs.si[yz_conc_upd$month_num %in% c(3,4,5)] = 0.58658205
yz_conc_upd$al.vs.si[yz_conc_upd$month_num %in% c(6,7,8)] = 0.71824973
yz_conc_upd$al.vs.si[yz_conc_upd$month_num %in% c(9,10,11)] = 0.73418882

# re-estimated Si concentration
yz_conc_upd$Si = yz_conc_upd$Al/yz_conc_upd$al.vs.si
yz_conc_upd$al.vs.si = yz_conc_upd$month_num = NULL



yz_conc_upd = na.omit(yz_conc_upd) # remove NAs 2017-11-16
dim(yz_conc_upd)

plot(yz_conc_upd$Al, yz_conc_upd$Al_new)
cor.test(yz_conc_upd$Al, yz_conc_upd$Al_new)

yz_conc_upd$Al = (yz_conc_upd$Al + yz_conc_upd$Al_new)/2
yz_conc_upd$Al_new = NULL

# reorder columns
yz_conc_upd = species_col_reorder(yz_conc_upd)
yz_conc_upd = relocate(yz_conc_upd, Si, .after = Se)
names(yz_conc_upd); head(yz_conc_upd)

###### replaced with updated OC EC
# reestimate OC EC in 2023
yz_ocec_2023 = fread("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/YZ_data/Yangzhou_OC EC_updated_based on 2023.csv")
yz_ocec_2023$Date = as.Date(yz_ocec_2023$Date, format = "%m/%d/%y") #format = "%Y/%m/%d"
yz_ocec_2023$Date[1]

# mean if duplicated
library(plyr)
yz_ocec_2023 =
  ddply(yz_ocec_2023, .(Date), summarise,
        OP = mean(OP), OC = mean(OC), EC = mean(EC),
        OC1 = mean(OC1), OC2 = mean(OC2), OC3 = mean(OC3), OC4 = mean(OC4),
        EC1 = mean(EC1), EC2 = mean(EC2), EC3 = mean(EC3))
summary(yz_ocec_2023)

# only keep those in yz_conc_upd
yz_ocec_2023 = subset(yz_ocec_2023, Date %in% unique(yz_conc_upd$Date))
summary(unique(yz_conc_upd$Date) %in% unique(yz_ocec_2023$Date))
dim(yz_ocec_2023)

# replace common column in yz_conc_upd by yz_ocec_2023
setDT(yz_ocec_2023)
common_columns <- intersect(names(yz_conc_upd), names(yz_ocec_2023)[-1])
yz_conc_upd[, (common_columns) := yz_ocec_2023[, common_columns, with = FALSE]]

# write.csv(yz_conc_upd, "YZ_elementSi_ion_pah_C_2024.07.csv", row.names = FALSE)
# write.csv(yz_conc_upd, "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/YZ_data/YZ_elementSi_ion_pah_C_2024.07.csv", row.names = FALSE)

write.csv(yz_conc_upd, "YZ_element_ion_pah_updateC_2024.08.csv", row.names = FALSE)
write.csv(yz_conc_upd, "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/YZ_data/YZ_element_ion_pah_updateC_2024.08.csv", row.names = FALSE)

yz_conc_upd = fread("YZ_element_ion_pah_updateC_2024.08.csv")
head(yz_conc_upd)

yz_conc_upd_1 = yz_conc_upd
head(yz_conc_upd_1)

# create a pseudo files to show all except for Si as FALSE, positions of data mannually replaced by NA
## a deep copy of yz_conc_upd, use copy instead of conc_pmf_mn_na = yz_conc_upd
## if using conc_pmf_mn_na = yz_conc_upd, then everything in yz_conc_upd becomes 0!!!!
## VIP!!! because := in data.table modifies the object in place, changes to conc_pmf_mn_na are affecting yz_conc_upd and yz_conc_upd_1!!!!!
## By using copy(), ensure that conc_pmf_mn_na is a completely separate object and that any modifications to it do not affect yz_conc_upd_1.

conc_pmf_mn_na = copy(yz_conc_upd)
conc_pmf_mn_na[, (2:ncol(conc_pmf_mn_na)) := FALSE]
conc_pmf_mn_na$Si = TRUE
conc_pmf_mn_na = data.frame(conc_pmf_mn_na)
head(conc_pmf_mn_na); names(conc_pmf_mn_na)
dim(conc_pmf_mn_na)

######  1.1 data prepare: Yangzhou, Carbonaceous groups, no PAH ######

###### Yangzhou, JS-CDC data
dataset = "YZ_"
data_use = "Csub"

# species not to use
species_exclude = c("EC", "OC",
                    "Fla", "Pyr", "Chry", "BaA", "BbF", "BkF", "BaP", "DahA", "BghiP", "InP")

# conc data for the next step
conc_pmf_mn_noNA = as.data.frame(yz_conc_upd)
conc_pmf_mn_noNA[ , species_exclude] <- list(NULL)
head(conc_pmf_mn_noNA)
dim(conc_pmf_mn_noNA)

######  1.2 data prepare: Yangzhou, total OC/EC, with PAH ######

###### Yangzhou, JS-CDC data
dataset = "YZ_"
data_use = "OCEC_pah"

# species not to use
species_exclude = c("EC1", "EC2", "EC3", "OC1", "OC2", "OC3", "OC4", "OP")

# conc data for the next step
conc_pmf_mn_noNA = as.data.frame(yz_conc_upd)
conc_pmf_mn_noNA[ , species_exclude] <- list(NULL)
head(conc_pmf_mn_noNA)
dim(conc_pmf_mn_noNA)

######  1.3 data prepare: Yangzhou, total OC/EC, no PAH ######

###### Yangzhou, JS-CDC data
dataset = "YZ_"
data_use = "OCEC"

# species not to use
species_exclude = c("EC1", "EC2", "EC3", "OC1", "OC2", "OC3", "OC4", "OP",
                    "Fla", "Pyr", "Chry", "BaA", "BbF", "BkF", "BaP", "DahA", "BghiP", "InP")

# conc data for the next step
conc_pmf_mn_noNA = as.data.frame(yz_conc_upd)
conc_pmf_mn_noNA[ , species_exclude] <- list(NULL)
head(conc_pmf_mn_noNA)

#### 2. GUI inputs preparation  ####

#### Identify and save common columns in conc and mdl_k, and reorder
# common columns
common_columns <- intersect(colnames(conc_pmf_mn_noNA), colnames(mdl_k))
mdl_k_new = mdl_k[, common_columns]
conc_pmf_toComp = conc_pmf_mn_noNA[, common_columns]
head(conc_pmf_toComp)

# ensure the order matches conc_pmf_mn_noNA
summary(names(mdl_k_new) == names(conc_pmf_toComp))

# change names to extract LOD and K values separately
mdl_k_new$data = c("LOD ug/m3", "K value")
mdl_k_new = relocate(mdl_k_new, "data", .before = "Al")

mdl_k_common_expanded = 
  mdl_k_new[rep(row.names(mdl_k_new), 
                           each = nrow(conc_pmf_toComp)), ]
comp_mdl = subset(mdl_k_common_expanded, data == "LOD ug/m3")
comp_k = subset(mdl_k_common_expanded, data == "K value")
comp_mdl$data = comp_k$data = NULL

sapply(conc_pmf_toComp, class)
sapply(comp_mdl, class)

# compare concentration and MDL of a given component
conc_vs_mdl = data.frame(Map(">", conc_pmf_toComp, comp_mdl))
sapply(conc_vs_mdl, class)
dim(conc_vs_mdl)
dim(comp_mdl)

write.csv(conc_vs_mdl, paste0(dataset, data_use, "_conc_vs_mdl.csv"), row.names = F)

# assign data conditionally to concentration and uncertainty values for PMF
conc_pmf = conc_vs_mdl * conc_pmf_toComp +
  (!conc_vs_mdl) * comp_mdl * 0.5
unc_pmf = conc_vs_mdl * (((comp_mdl / 2)^2 + (comp_k * conc_pmf_toComp)^2)^0.5) +
  (!conc_vs_mdl) * 5/6 * comp_mdl
unc_pmf$PM25 = conc_pmf$PM25 = conc_pmf_toComp$PM25 = conc_pmf_mn_noNA$PM25
unc_pmf$PM25 = 3 * conc_pmf_toComp$PM25

### extra 4 times uncertainty for interpolated values

common_columns_2 <- intersect(colnames(conc_pmf_toComp), colnames(conc_pmf_mn_na))
conc_pmf_mn_na_use = conc_pmf_mn_na[, common_columns_2]
summary(names(conc_pmf_mn_na_use) == names(unc_pmf))

conc_pmf_mn_na_use[conc_pmf_mn_na_use == TRUE] = 4.5
conc_pmf_mn_na_use[conc_pmf_mn_na_use == FALSE] = 1

unc_pmf_intp = unc_pmf * conc_pmf_mn_na_use

summary(unc_pmf_intp); summary(unc_pmf); summary(conc_pmf_mn_na_use)

# insert date into  concentration & uncertainty datasets
conc_pmf$Date = unc_pmf_intp$Date = conc_pmf_mn_noNA$Date

conc_pmf = relocate(conc_pmf, "Date", .before = "Al")
unc_pmf_intp = relocate(unc_pmf_intp, "Date", .before = "Al")
conc_pmf_mn_noNA = relocate(conc_pmf_mn_noNA, "Date", .before = "Al")

summary(unc_pmf_intp); summary(conc_pmf); summary(conc_pmf_mn_noNA)
dim(unc_pmf_intp); dim(conc_pmf)

# write.csv(conc_pmf, paste0(dataset, data_use, "_conc.csv"), row.names = F)
# write.csv(unc_pmf_intp, paste0(dataset, data_use, "_unc.csv"), row.names = F)

write.csv(conc_pmf, paste0(dataset, data_use, "_EC1adjust_conc.csv"), row.names = F)
write.csv(unc_pmf_intp, paste0(dataset, data_use, "_EC1adjust_unc.csv"), row.names = F)


# #### 3. dispersion normalization ####
# 
# # get the dispersion normalization coefficient
# dn_coef = read.csv("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/NJ_NJUdata_PMF/NJU_DN_coefficient_mean_1523.29501541806.csv")
# dn_coef$X = NULL
# dn_coef$VC_coef_use = dn_coef$VC / mean( dn_coef$VC)
# 
# conc_pmf_DN = conc_pmf
# unc_pmf_DN = unc_pmf_intp
# 
# for (col in names(conc_pmf_DN)) {
#   if (col != "Date") {
#     conc_pmf_DN[[col]] =
#       conc_pmf_DN[[col]] * dn_coef$VC_coef_use
#     unc_pmf_DN[[col]] =
#       unc_pmf_DN[[col]] * dn_coef$VC_coef_use
#   }
# }
# 
# write.csv(conc_pmf_DN, paste0(dataset, data_use, "_DN_conc.csv"), row.names = F)
# write.csv(unc_pmf_DN, paste0(dataset, data_use, "_DN_unc.csv"), row.names = F)

#### 4. NON-GUI inputs ####
setwd("/Users/TingZhang/Documents/EPA PMF/Data")

##### 4.1 NON-GUI. read data #####

## original 
conc_gui = fread(paste0(dataset, data_use, "_conc.csv"))
unc_gui = fread(paste0(dataset, data_use, "_unc.csv"))

## dispersion normalization, if DN data, no need to 
# conc_gui = fread(paste0(dataset, data_use, "_DN_conc.csv"))
# unc_gui = fread(paste0(dataset, data_use, "_DN_unc.csv"))

# conc_gui$V1 = unc_gui$V1 = NULL
names(conc_gui); names(unc_gui)

##### 4.2 NON-GUI. SWB, Strong, Weak, or Bad classification (SNR & MDL), estimated based on non-DN data #####
## only for data before dispersion normalization

# concentration vs. MDL
conc_vs_mdl = fread(paste0(dataset, data_use, "_conc_vs_mdl.csv"))
# conc_vs_mdl$V1 = NULL

# extract species colnames, those after excluding "SiteCode", "State" & "Date"
species_columns = 
  setdiff(names(conc_gui), "Date")
conc.col = ncol(conc_gui)

### SNR signal-to-noise ratio estimation
# according to EPA PMF 5.0 User Guide.pdf, function 5-3 & 5-4 from page 23
conc_unc_diff_site = 
  (conc_gui[, ..species_columns] - 
     unc_gui[, ..species_columns])/
  unc_gui[, ..species_columns] * conc_vs_mdl

conc_unc_diff_site$PM25 = -999
# conc_unc_diff_site[1:5, 4:10]

# get the SNR 
estimate_snr = 
  colSums(conc_unc_diff_site)/
  nrow(conc_unc_diff_site)
# convert the result to a one-row data.frame for later match
estimate_snr = data.frame(t(estimate_snr))  

# generate the weak & bad species based on SNR 
species.bad.snr = colnames(estimate_snr)[which(
  colSums(estimate_snr) < 0.2)]
species.weak.snr = colnames(estimate_snr)[which(
  colSums(estimate_snr) >= 0.2 &
    colSums(estimate_snr) < 2)]

### the percent of species-specific fration of above MDL

conc_gui_aboveMDL = data.frame(colSums(conc_vs_mdl))
conc_gui_aboveMDL$CompName = row.names(conc_gui_aboveMDL)
names(conc_gui_aboveMDL)[1] = "above_MDL_count"
conc_gui_aboveMDL$Percent = 
  conc_gui_aboveMDL$above_MDL_count * 100 / nrow(conc_gui)

# generate the weak & bad species based on the conc_vs_mdl 
species.weak.Pmdl = conc_gui_aboveMDL$CompName[
  conc_gui_aboveMDL$Percent <= 50 &
    conc_gui_aboveMDL$Percent > 20]
species.bad.Pmdl = conc_gui_aboveMDL$CompName[
  conc_gui_aboveMDL$Percent <= 20]
# change to character

species.weak.Pmdl = as.character(species.weak.Pmdl)
species.bad.Pmdl = as.character(species.bad.Pmdl)

### other criteria
# setting Si as weak, cause it's not from the measurement
other.weak = c("Si", "PM25")

species.weak = append(species.weak.snr, species.weak.Pmdl)
species.weak = append(species.weak, other.weak)

species.bad = append(species.bad.snr, species.bad.Pmdl)

# if a species exist both in "bad" & "weak", define as "WEAK"
species.bad = species.bad[! (
  species.bad %in% species.weak)]

# remove the duplicated strings from the character vector
species.bad = unique(
  unlist(
    strsplit(
      species.bad, " ")))
species.weak = unique(
  unlist(
    strsplit(
      species.weak, " ")))

# arrange in alphabetic order 
species.bad = sort(species.bad)
species.weak = sort(species.weak)

# "strong" species
species.strong = setdiff(species_columns, 
                         append(species.weak, species.bad))


###### create dataset to save SWB results
cmd_species_class = 
  data.frame(
    matrix(ncol = length(species_columns), 
           nrow = 1))
names(cmd_species_class) = species_columns

# add PM2.5 and other variables
cmd_species_class = 
  data.frame(data_use, 
             cmd_species_class, 
             sum.weak.good = NA, style.weak.good = NA, site.row = NA)

# assign 1 to strong, 0 to weak
cmd_species_class[1, species.strong] = 1
cmd_species_class[1, species.weak] = 0
cmd_species_class[1, species.bad] = NA

# used species
species_use = species_columns[
  species_columns %in% 
    c(species.strong, species.weak)]

nonGUI_disp_species <- cmd_species_class[, species_use]
cmd_species_class$style.weak.good[1] <- 
  paste0("/", nonGUI_disp_species, collapse = "")

# assign other values
cmd_species_class$sum.weak.good[1] = length(species_use)
cmd_species_class$site.row = nrow(conc_gui)

write.csv(cmd_species_class, paste0(dataset, data_use, "_SWB.csv"))

##### 4.3 NON-GUI. conc & unc #####

conc_nongui <- conc_gui
unc_nongui <- unc_gui

### remove bad species columns
# no bad for now
conc_nongui <- conc_nongui[, !species.bad, with=FALSE]
unc_nongui <- unc_nongui[, !species.bad, with=FALSE]

# extra uncertainty for weak species
unc_nongui[, (species.weak) := 
             lapply(.SD, 
                    function(x) 
                      x * 3), 
           .SDcols = species.weak]


# add info into column names before combination
names(conc_nongui) = paste0("conc_", names(conc_nongui))
names(unc_nongui) = paste0("unc_", names(unc_nongui))

# summary(conc_nongui$conc_Date == unc_nongui$unc_Date)

# combining conc & unc files
cmd_conc_unc = cbind(conc_nongui, unc_nongui)

# Interleave columns of concentration and uncertainty files
## generate a new vector to index the columns of cbind file
interleave.col.order <- 
  rep(1:ncol(unc_nongui), each = 2) + 
  (0:1) * ncol(unc_nongui)

## reorder the cbind file
# cmd_input_interleave = cmd_conc_unc[interleave.col.order] # for data.frame only
cmd_input_interleave = 
  as.data.table(
    as.matrix(
      cmd_conc_unc)[, 
                    interleave.col.order])

# only keep one Date & cluster column
cmd_input_interleave$unc_Date = NULL
names(cmd_input_interleave)[1] = "Date"
# head(cmd_input_interleave)

cmd_input_interleave = 
  cmd_input_interleave[
    with(cmd_input_interleave, 
         order(Date)), ]


dropbox_path = "/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/NJ_NJUdata_PMF"

## save original data 
write.csv(cmd_input_interleave, 
          file = file.path(
            dropbox_path, 
            paste0(dataset, data_use, "_CMD.csv")),
          row.names = FALSE)

write.csv(cmd_input_interleave, 
          paste0(dataset, data_use, "_CMD.csv"),
          row.names = FALSE)

# ## save Dispersion Normalization data 
# write.csv(cmd_input_interleave, 
#           file = file.path(
#             dropbox_path, 
#             paste0(dataset, data_use, "_DN_CMD.csv")),
#           row.names = FALSE)
# 
# write.csv(cmd_input_interleave, 
#           paste0(dataset, data_use, "_DN_CMD.csv"),
#           row.names = FALSE)

##### 4.4 NON-GUI. iniparams.txt files #####

######## 4.4.1 NON-GUI read & prepare inputs ######## 

######## read iniparams files 
base_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_non-GUI_files/iniparams_templates/iniparams_base_1.txt")
BS_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_non-GUI_files/iniparams_templates/iniparams_BS_2.txt")
DISP_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_non-GUI_files/iniparams_templates/iniparams_DISP_3.txt")
before_BS_DISP_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_non-GUI_files/iniparams_templates/iniparams_BS_PHASE_of_BSDISP_4.txt")
BS_DISP_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_non-GUI_files/iniparams_templates/iniparams_BS-DISP_5.txt")

factor.number.series = c(5:11)

######## folders, other related info 

# DN_status = "_DN"
DN_status = ""

## file names & folders
conc_csv_file = paste0(dataset, data_use, DN_status, "_CMD.csv")
conc_csv = fread(conc_csv_file) 
site_folder = paste0(dataset, data_use, DN_status)

# Create a new folder for site_cmd 
site_cmd_folder <- file.path("NJ_NJUdata_nonGui", site_folder)

if (!dir.exists(site_cmd_folder)) {
  dir.create(site_cmd_folder, recursive = TRUE)
}

# create subfolders Factor_6 to Factor_11 under site_cmd
for (i in factor.number.series) {
  subfolder <- file.path(site_cmd_folder, paste0("Factor_", i))
  if (!dir.exists(subfolder)) {
    dir.create(subfolder, recursive = TRUE)
  }
}

# change first row to serial number, dates are not available in non-GUI PMF
colnames(conc_csv)[1] = "SerialNumber"
conc_csv$SerialNumber = 1:nrow(conc_csv)

# create new path
path_nonGUI <- file.path(site_cmd_folder, conc_csv_file)

# Save conc_csv to site_cmd_folder
write.csv(conc_csv, path_nonGUI, row.names = FALSE)


## SWB and other inputs 
site_info = cmd_species_class

site.row = site_info$site.row
variable.NO = site_info$sum.weak.good
## weak0 or strong1 for PM2.5 species 
weak.strong.assign = site_info$style.weak.good

# change to the style used for txt
# remove the first "/", and change the rest to "\t"
weak.strong.assign = gsub("/", "\\\t", 
                          sub("^/", "", weak.strong.assign))

######## 4.4.2 NON-GUI update iniparams.txt ######## 

# replace the number of factors to use and name of output files
for (factor.No in factor.number.series){
  base_par = base_par_org
  BS_par = BS_par_org
  DISP_par = DISP_par_org
  before_BS_DISP_par = before_BS_DISP_par_org
  BS_DISP_par = BS_DISP_par_org
  
  # output path
  path.CF = paste0(site_cmd_folder, "/Factor_", factor.No, "/")
  overall.unc = 0 # default, overall.unc = 0.05
  
  # replace with the row, variable, & factor number 
  base_par = row_var_factor(base_par, site.row, variable.NO, factor.No, overall.unc)
  BS_par = row_var_factor(BS_par, site.row, variable.NO, factor.No, overall.unc)
  DISP_par = row_var_factor(DISP_par, site.row, variable.NO, factor.No, overall.unc)
  before_BS_DISP_par = row_var_factor(before_BS_DISP_par, site.row, variable.NO, factor.No, overall.unc)
  BS_DISP_par = row_var_factor(BS_DISP_par, site.row, variable.NO, factor.No, overall.unc)
  
  ##### B1: iniparams for Base run
  # create and replace the names for base run input & output files
  
  base.input = conc_csv_file
  output.pre = paste0(dataset, data_use, DN_status, "_F_", factor.No)
  
  base.output = paste0(output.pre, "_")
  
  base_par = base_input_output(base_par, base.input, base.output)
  
  write.table(base_par, 
              file = paste0(path.CF, 
                            "iniparams_base_", 
                            dataset, data_use, DN_status, 
                            "_F_", factor.No, ".txt"), 
              sep = "\t",
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
  
  ##### B2: iniparams common input for BS (bootstrap) & DISP files
  # create and replace the name of input file
  BS.DISP.input.1 = base.input
  BS.DISP.input.2 = paste0(output.pre, "_.dat")
  
  BS.output = paste0(output.pre, "_BS_")
  DISP.output = paste0(output.pre, "_DISP_")
  before_BS_DISP.output = paste0(output.pre, "_before_BS_DISP_")
  BS_DISP.output = paste0(output.pre, "_BS_DISP_")
  
  BS_par = bs_disp_input_output(BS_par, 
                                BS.DISP.input.1, 
                                BS.DISP.input.2, 
                                BS.output)
  
  DISP_par = bs_disp_input_output(DISP_par, 
                                  BS.DISP.input.1, 
                                  BS.DISP.input.2, 
                                  DISP.output)
  DISP_par = dispbcmask_rp(DISP_par, 
                           weak.strong.assign, 
                           factor.No)
  
  before_BS_DISP_par = bs_disp_input_output(before_BS_DISP_par, 
                                            BS.DISP.input.1, 
                                            BS.DISP.input.2, 
                                            before_BS_DISP.output)
  
  BS_DISP_par = bs_disp_input_output(BS_DISP_par, 
                                     BS.DISP.input.1, 
                                     BS.DISP.input.2, 
                                     BS_DISP.output)
  
  # BS_par[29:40]; DISP_par[29:40]; before_BS_DISP_par[29:40]; BS_DISP_par[29:40]; 
  
  ##### B3: output BS (bootstrap) & DISP files
  write.table(BS_par, 
              file = paste0(path.CF,"iniparams_BS_", 
                            dataset, data_use, DN_status, 
                            "_F_", factor.No, ".txt"), 
              sep = "\t",
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
  
  write.table(DISP_par, 
              file = paste0(path.CF, "iniparams_DISP_", 
                            dataset, data_use, DN_status, 
                            "_F_", factor.No, ".txt"), 
              sep = "\t",
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
  
  write.table(before_BS_DISP_par, 
              file = paste0(path.CF, "iniparams_BS_DISP_before_", 
                            dataset, data_use, DN_status, 
                            "_F_", factor.No, ".txt"), 
              sep = "\t",
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
  
  write.table(BS_DISP_par, 
              file = paste0(path.CF, "iniparams_BS_DISP_", 
                            dataset, data_use, DN_status, 
                            "_F_", factor.No, ".txt"), 
              sep = "\t",
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
}




