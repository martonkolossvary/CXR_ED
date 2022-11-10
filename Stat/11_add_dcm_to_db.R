######README#####
# Create final database for available images and filter for first occurence per patient and CXR


# Initialize -----
library(parseRPDR); library(data.table)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
INSTITUTE    <- c("BWH", "MGH")
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/RAW/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_image <- paste0(folder_wd, "IMAGES/")
nThread      <- 10


# Load dcm data -----
attach(paste0(folder_cache, "RDT_DCM_data_potential.RData"))
d_rdt <- d_rdt_all
detach()

# Create list of images passed QA/QC -----
ID_img <- setDT(as.data.frame(list.files(paste0(folder_wd, "PNG_potential/"))))
colnames(ID_img) <- "ID_img"

# Merge tables -----
d_rdt <- data.table::merge.data.table(d_rdt, ID_img, by = "ID_img", all.x = FALSE, all.y = TRUE)

## Select earliest CXR images ---- 51950 encounters
d_rdt <- d_rdt[ , .SD[which.min(time_between_rdt_study_hours)], by = ID_encED_time]

## In case of multiple encounters at different sites, take earliest ---- 51519
d_rdt <- d_rdt[ , .SD[which.min(time_rdt)], by = ID_enc_EMPI]

# Load final data to merge with database ----
attach(paste0(folder_cache, "BWH_FINAL_CORRECTED.RData"))
d_BWH <- d
detach()

attach(paste0(folder_cache, "MGH_FINAL_CORRECTED.RData"))
d_MGH <- d
detach()

d <- data.table::rbindlist(list(d_BWH, d_MGH), fill = TRUE)
rm(d_BWH, d_MGH)

# Subset population ----
CXR_lab_card <- d[CXR_N>0 & (TROPONIN_N>0 | DDIMER_N>0) & PRC_N>0] #HAS AT LEAST 1 CXR AND 1 LAB AND 1 CARDIAC RESULT WITHIN 1 DAY
CXR_lab_card_P   <- length(unique(CXR_lab_card$ID_MERGE))
CXR_lab_card_E   <- length(unique(CXR_lab_card$ID_encED_time))

# Merge with CXR data ----- One duplicate keep first one as it has more data
d_out <- data.table::merge.data.table(CXR_lab_card, d_rdt, by = "ID_encED_time", all.x = FALSE, all.y = TRUE)
d_out <- d_out[!duplicated(d_out, by = "ID_encED_time")]

# Clear up identical columns and with .x -----
d_out[, colnames(d_out)[grep(".y", colnames(d_out), fixed = TRUE)] := NULL]
colnames(d_out) <- sub(".x", "", colnames(d_out), fixed = TRUE)

# Format final datasets -----
d_final <- d_out
d_final_MGH <- d_final[enc_hosp == "MGH"]
d_final_BWH <- d_final[enc_hosp == "BWH"]

### CREATE NEW COMBINED DATA FILE ###
## Create train/tune
d_final_MGH$Tuning <- NA
d_final_BWH$Tuning <- NA

IDs      <- 1:dim(d_final_MGH)[1]
train_ID <- as.numeric(caret::createDataPartition(as.factor(d_final_MGH$ANY_ENDPOINT_DEATH_30days), p = 0.6, list = FALSE)) #Training IDs
d_final_MGH$Tuning[train_ID] <- 0

valid_ID <- IDs[!IDs %in% train_ID]
d_valid  <- d_final_MGH[valid_ID, ]
tune_ID  <- as.numeric(caret::createDataPartition(d_valid$ANY_ENDPOINT_DEATH_30days, p = 0.5, list = FALSE)) #Tune IDs
tune_png <- d_valid[tune_ID]$ID_img
d_final_MGH[ID_img %in% tune_png]$Tuning <- 1


d_final <- rbind(d_final_MGH, d_final_BWH)
d_final$PNG_name_full <- paste0("mnt/md0/mkolossvary/CXR_ED/PNG_ALL/", d_final$ID_img)

d_final$ANY_ENDPOINT_DEATH_30days <- d_final$ANY_ENDPOINT_DEATH_30days*1
d_final$DEATH_30days <- d_final$DEATH_30days*1
d_final$ANY_ENDPOINT_30days <- d_final$ANY_ENDPOINT_30days*1
d_final$MI_30days <- d_final$MI_30days*1
d_final$PE_30days <- d_final$PE_30days*1
d_final$AD_30days <- d_final$AD_30days*1


# Save database and subsets of it -----
save(d_final, file = paste0(folder_cache, "FINAL_ALL.RData"), envir = .GlobalEnv)

d_final_MGH <- d_final[enc_hosp == "MGH"]
save(d_final_MGH, file = paste0(folder_cache, "FINAL_MGH.RData"), envir = .GlobalEnv)

d_final_BWH <- d_final[enc_hosp == "BWH"]
save(d_final_BWH, file = paste0(folder_cache, "FINAL_BWH.RData"), envir = .GlobalEnv)

data.table::fwrite(d_final[, c("PNG_name_full", "ANY_ENDPOINT_DEATH_30days", "Tuning")], paste0(folder_cache, "CXR_ED_database.csv"), dateTimeAs = "write.csv")



# CREATE BWH AND MGH+BWH TUNING SPLIT COLUMNS -----
# Initialize -----
library(parseRPDR); library(data.table)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
INSTITUTE    <- c("BWH", "MGH")
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_image <- paste0(folder_wd, "IMAGES/")
nThread      <- 10

load(paste0(folder_cache, "FINAL_ALL.RData"))

d_final_MGH <- d_final[enc_hosp == "MGH"]
d_final_BWH <- d_final[enc_hosp == "BWH"]

## Create train/tune for BWH
d_final_MGH$Tuning_BWH <- NA
d_final_BWH$Tuning_BWH <- NA

IDs      <- 1:dim(d_final_BWH)[1]
train_ID <- as.numeric(caret::createDataPartition(as.factor(d_final_BWH$ANY_ENDPOINT_DEATH_30days), p = 0.6, list = FALSE)) #Training IDs
d_final_BWH$Tuning_BWH[train_ID] <- 0

valid_ID <- IDs[!IDs %in% train_ID]
d_valid  <- d_final_BWH[valid_ID, ]
tune_ID  <- as.numeric(caret::createDataPartition(d_valid$ANY_ENDPOINT_DEATH_30days, p = 0.5, list = FALSE)) #Tune IDs
tune_png <- d_valid[tune_ID]$ID_img
d_final_BWH[ID_img %in% tune_png]$Tuning_BWH <- 1
d_final <- rbind(d_final_MGH, d_final_BWH)


## Create train/tune for ALL
d_final$Tuning_ALL <- NA

IDs      <- 1:dim(d_final)[1]
train_ID <- as.numeric(caret::createDataPartition(as.factor(d_final$ANY_ENDPOINT_DEATH_30days), p = 0.6, list = FALSE)) #Training IDs
d_final$Tuning_ALL[train_ID] <- 0

valid_ID <- IDs[!IDs %in% train_ID]
d_valid  <- d_final[valid_ID, ]
tune_ID  <- as.numeric(caret::createDataPartition(d_valid$ANY_ENDPOINT_DEATH_30days, p = 0.5, list = FALSE)) #Tune IDs
tune_png <- d_valid[tune_ID]$ID_img
d_final[ID_img %in% tune_png]$Tuning_ALL <- 1

save(d_final, file = paste0(folder_cache, "FINAL_ALL.RData"), envir = .GlobalEnv)

d_final_MGH <- d_final[enc_hosp == "MGH"]
save(d_final_MGH, file = paste0(folder_cache, "FINAL_MGH.RData"), envir = .GlobalEnv)

d_final_BWH <- d_final[enc_hosp == "BWH"]
save(d_final_BWH, file = paste0(folder_cache, "FINAL_BWH.RData"), envir = .GlobalEnv)

data.table::fwrite(d_final[, c("PNG_name_full", "ANY_ENDPOINT_DEATH_30days", "Tuning", "Tuning_BWH", "Tuning_ALL")], paste0(folder_cache, "CXR_ED_database.csv"), dateTimeAs = "write.csv")











# Create folders with the respective images and gzip -----
# type <- "ALL"
# dir.create(paste0(folder_wd, "PNG_", type))
# img_to_copy <- paste0(folder_wd, "PNG_potential/", d_final$ID_img)
# file.copy(img_to_copy, paste0(folder_wd, "PNG_", type))
# tar(paste0(folder_wd, "PNG_", type, ".tgz"), files = paste0(folder_wd, "PNG_", type), compression = "gzip")
# 
# type <- "MGH"
# dir.create(paste0(folder_wd, "PNG_", type))
# img_to_copy <- paste0(folder_wd, "PNG_potential/", d_final_MGH$ID_img)
# file.copy(img_to_copy, paste0(folder_wd, "PNG_", type))
# tar(paste0(folder_wd, "PNG_", type, ".tgz"), files = paste0(folder_wd, "PNG_", type), compression = "gzip")
# 
# type <- "BWH"
# dir.create(paste0(folder_wd, "PNG_", type))
# img_to_copy <- paste0(folder_wd, "PNG_potential/", d_final_BWH$ID_img)
# file.copy(img_to_copy, paste0(folder_wd, "PNG_", type))
# tar(paste0(folder_wd, "PNG_", type, ".tgz"), files = paste0(folder_wd, "PNG_", type), compression = "gzip")

# type <- "MGH_train"
# dir.create(paste0(folder_wd, "PNG_", type))
# img_to_copy <- paste0(folder_wd, "PNG_potential/", d_final_MGH_train$ID_img)
# file.copy(img_to_copy, paste0(folder_wd, "PNG_", type))
# tar(paste0(folder_wd, "PNG_", type, ".tgz"), files = paste0(folder_wd, "PNG_", type), compression = "gzip")
# 
# type <- "MGH_test"
# dir.create(paste0(folder_wd, "PNG_", type))
# img_to_copy <- paste0(folder_wd, "PNG_potential/", d_final_MGH_test$ID_img)
# file.copy(img_to_copy, paste0(folder_wd, "PNG_", type))
# tar(paste0(folder_wd, "PNG_", type, ".tgz"), files = paste0(folder_wd, "PNG_", type), compression = "gzip")
# 
# type <- "MGH_tune"
# dir.create(paste0(folder_wd, "PNG_", type))
# img_to_copy <- paste0("PNG_potential/", d_final_MGH_tune$ID_img)
# file.copy(img_to_copy, paste0(folder_wd, "PNG_", type))
# tar(paste0(folder_wd, "PNG_", type, ".tgz"), files = paste0(folder_wd, "PNG_", type), compression = "gzip")


