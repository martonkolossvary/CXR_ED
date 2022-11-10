
# Initialize -----
library(parseRPDR); library(data.table); library(ggplot2)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
INSTITUTE    <- c("MGH")
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_image <- paste0(folder_wd, "IMAGES/")
folder_pred  <- paste0(folder_wd, "MODELS/")
folder_stat  <- paste0(folder_wd, "STAT/")
nThread      <- 10

fn_col    <- "PNG_name_full"
label_col <- "ANY_ENDPOINT_DEATH_30days"
valid_col <- "Tuning"
DL_pred   <- "pred_1"
hosp      <- "enc_hosp"
roc_outcomes <- c("ANY_ENDPOINT_DEATH_30days", "ANY_ENDPOINT_30days", "DEATH_30days", "MI_30days", "PE_30days", "AD_30days")
covariates   <- c("time_age_at_ED", "gender", "BIOMARKER_POS")

# Load final database -----
load(paste0(folder_cache, "FINAL_ALL.RData"))
d_final$BIOMARKER_POS <- d_final$TROPONIN_POS | d_final$DDIMER_POS
d_final$BIOMARKER_POS[is.na(d_final$BIOMARKER_POS)] <- d_final$TROPONIN_POS[is.na(d_final$BIOMARKER_POS)]
d_final$BIOMARKER_POS[is.na(d_final$BIOMARKER_POS)] <- d_final$DDIMER_POS[is.na(d_final$BIOMARKER_POS)]
d_final$gender[is.na(d_final$gender)] <- "Male"#; d_final$gender <- as.factor(d_final$gender)
d_final$DEATH_30days <- d_final$DEATH_30days *1

d_view <- data.table::fread(paste0(folder_wd, "DB/AP_PA.csv"))
setorder(d_view, V1)

# Merge AP, PA data with final database -----
missing_view <- d_final[is.na(d_final$ViewPosition) | d_final$ViewPosition == ""]$ID_img
all(missing_view == d_view$ID_img); rm(missing_view)

check_1000      <- d_final$ViewPosition[1:1000]
d_final[is.na(d_final$ViewPosition) | d_final$ViewPosition == ""]$ViewPosition <- d_view$ViewPosition
check_1000_fill <- d_final$ViewPosition[1:1000]

sum(check_1000 == check_1000_fill)
length(check_1000[check_1000 != check_1000_fill])


### CREATE NEW COMBINED DATA FILE ###
## Format final datasets -----
d_final_MGH_AP <- d_final[enc_hosp == "MGH" & ViewPosition == "AP"]
d_final_MGH_PA <- d_final[enc_hosp == "MGH" & ViewPosition == "PA"]
d_final_BWH <- d_final[enc_hosp == "BWH"]

## Create train/tune
d_final_MGH_AP$Tuning <- NA
d_final_MGH_PA$Tuning <- NA
d_final_BWH$Tuning <- NA

### Create for AP dataset
IDs      <- 1:dim(d_final_MGH_AP)[1]
train_ID <- as.numeric(caret::createDataPartition(as.factor(d_final_MGH_AP$ANY_ENDPOINT_DEATH_30days), p = 0.6, list = FALSE)) #Training IDs
d_final_MGH_AP$Tuning[train_ID] <- 0

valid_ID <- IDs[!IDs %in% train_ID]
d_valid  <- d_final_MGH_AP[valid_ID, ]
tune_ID  <- as.numeric(caret::createDataPartition(d_valid$ANY_ENDPOINT_DEATH_30days, p = 0.5, list = FALSE)) #Tune IDs
tune_png <- d_valid[tune_ID]$ID_img
d_final_MGH_AP[ID_img %in% tune_png]$Tuning <- 1

### Create for PA dataset
IDs      <- 1:dim(d_final_MGH_PA)[1]
train_ID <- as.numeric(caret::createDataPartition(as.factor(d_final_MGH_PA$ANY_ENDPOINT_DEATH_30days), p = 0.6, list = FALSE)) #Training IDs
d_final_MGH_PA$Tuning[train_ID] <- 0

valid_ID <- IDs[!IDs %in% train_ID]
d_valid  <- d_final_MGH_PA[valid_ID, ]
tune_ID  <- as.numeric(caret::createDataPartition(d_valid$ANY_ENDPOINT_DEATH_30days, p = 0.5, list = FALSE)) #Tune IDs
tune_png <- d_valid[tune_ID]$ID_img
d_final_MGH_PA[ID_img %in% tune_png]$Tuning <- 1

d_final <- rbind(d_final_MGH_AP, d_final_MGH_PA, d_final_BWH)


# Create stratified datasets -----
save(d_final, file = paste0(folder_cache, "FINAL_ALL.RData"), envir = .GlobalEnv)

load(paste0(folder_cache, "FINAL_ALL.RData"))
d_final <- d_final[ViewPosition == "AP"]
save(d_final, file = paste0(folder_cache, "FINAL_AP.RData"), envir = .GlobalEnv)
data.table::fwrite(d_final[, c("PNG_name_full", "ANY_ENDPOINT_DEATH_30days", "Tuning")], paste0(folder_cache, "CXR_ED_database_AP.csv"), dateTimeAs = "write.csv")

load(paste0(folder_cache, "FINAL_ALL.RData"))
d_final <- d_final[ViewPosition == "PA"]
save(d_final, file = paste0(folder_cache, "FINAL_PA.RData"), envir = .GlobalEnv)
data.table::fwrite(d_final[, c("PNG_name_full", "ANY_ENDPOINT_DEATH_30days", "Tuning")], paste0(folder_cache, "CXR_ED_database_PA.csv"), dateTimeAs = "write.csv")

