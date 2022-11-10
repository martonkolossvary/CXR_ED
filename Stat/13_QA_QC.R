######README#####
# RUN ROC ANALYSES


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

# Files to load -----
which_set        <- list.files(paste0(folder_pred, "V2"), full.names = TRUE)
which_set_names  <- list.files(paste0(folder_pred, "V2"), full.names = FALSE)
which_set        <- which_set[grep("pred", which_set)]
which_set_names  <- which_set_names[grep("pred", which_set_names)]
which_set_names  <- gsub("_pred.csv", "", which_set_names, fixed = TRUE)


# Load final database -----
load(paste0(folder_cache, "FINAL_ALL.RData"))
d_final$BIOMARKER_POS <- d_final$TROPONIN_POS | d_final$DDIMER_POS
d_final$BIOMARKER_POS[is.na(d_final$BIOMARKER_POS)] <- d_final$TROPONIN_POS[is.na(d_final$BIOMARKER_POS)]
d_final$BIOMARKER_POS[is.na(d_final$BIOMARKER_POS)] <- d_final$DDIMER_POS[is.na(d_final$BIOMARKER_POS)]
d_final$gender[is.na(d_final$gender)] <- "Male"#; d_final$gender <- as.factor(d_final$gender)
d_final$DEATH_30days <- d_final$DEATH_30days * 1


# Troponin positivity rates
table(d_final[MI_30days == TRUE]$TROPONIN_POS)
table(d_final[MI_30days == FALSE]$TROPONIN_POS)

# Death vs CV outcomes
table(d_final[DEATH_30days == TRUE]$ANY_ENDPOINT_30days)
table(d_final[DEATH_30days == FALSE]$ANY_ENDPOINT_30days)

sum(d_final[DEATH_30days == TRUE]$ANY_ENDPOINT_30days) / length(d_final[DEATH_30days == TRUE]$DEATH_30days)
sum(d_final[ANY_ENDPOINT_30days == TRUE]$DEATH_30days) / length(d_final[ANY_ENDPOINT_30days == TRUE]$ANY_ENDPOINT_30days)

table(d_final[DEATH_30days == TRUE]$DEATH_30days_source)
table(d_final[DEATH_30days == TRUE & ANY_ENDPOINT_7days == TRUE]$DEATH_30days_source)


# Export out missing AP/PA label images
png_copy <- d_final[is.na(d_final$ViewPosition) | d_final$ViewPosition == ""]$ID_img
png_copy <- paste0(folder_wd, "PNG_potential/", png_copy)
file.copy(png_copy, paste0(folder_wd, "PNG_AP_PA/"))
db <- d_final[is.na(d_final$ViewPosition) | d_final$ViewPosition == "", c("ID_img")]
write.csv(db, paste0(folder_wd, "DB/AP_PA.csv"))
