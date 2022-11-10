######README#####
# Code to summarize CXR_ED data - run on server due to potential timezone problems

#####
# jobsubmit -p basic -A circgp -m 50G -t 0-00:30:00 -c 1 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/5_SERVER_postprocess_MGH.R
#####


# Initialize -----
library(parseRPDR); library(data.table)
folder_wd         <- "/autofs/vast/circ/mjk2/CXR_ED"
INSTITUTE         <- "MGH"
folder_rpdr_home  <- paste0(folder_wd, "/RPDR/", INSTITUTE)
folder_cache_home <- paste0(folder_wd, "/CACHE")
folder_codes_home <- paste0(folder_wd, "/CODES")

f_pat    <- paste0(folder_cache_home, "/Patient_data_", INSTITUTE, ".RData");      load(f_pat)
f_enc    <- paste0(folder_cache_home, "/Encounter_data_", INSTITUTE, ".RData");    load(f_enc)
f_enc_ED <- paste0(folder_cache_home, "/ED_Encounter_data_", INSTITUTE, ".RData"); load(f_enc_ED)
f_lab    <- paste0(folder_cache_home, "/Laboratory_data_", INSTITUTE, ".RData");   load(f_lab)
f_rdt    <- paste0(folder_cache_home, "/Radiological_data_", INSTITUTE, ".RData"); load(f_rdt)
f_dia    <- paste0(folder_cache_home, "/Diagnosis_data_", INSTITUTE, ".RData");    load(f_dia)
f_prc    <- paste0(folder_cache_home, "/Procedural_data_", INSTITUTE, ".RData");    load(f_prc)

# REMOVE ED duplicates ----
d_enc_ED <- unique(d_enc_ED, by = "ID_encED_time")

# Merge datasources -----
## Create ED encounter master table -----
d <- data.table::merge.data.table(d_enc_ED, d_pat_all, by = "ID_MERGE",
                                  all.x = TRUE, all.y = FALSE)
rm(d_enc_ED, d_pat_all)


### Truncate times to days
d$time_enc_admit     <- as.POSIXct(format(d$time_enc_admit, format = "%Y-%m-%d %I:%M:%S %p"))
d$time_enc_disch     <- as.POSIXct(format(d$time_enc_disch, format = "%Y-%m-%d %I:%M:%S %p"))
d$time_date_of_birth <- as.POSIXct(format(d$time_date_of_birth, format = "%Y-%m-%d %I:%M:%S %p"))
d$time_date_of_death <- as.POSIXct(format(d$time_date_of_death, format = "%Y-%m-%d %I:%M:%S %p"))
d$time_age_at_ED     <- as.numeric(difftime(d$time_enc_admit, d$time_date_of_birth, units = "days"))/365

## REMOVE visits where age<18 and where death date unavailable -----
d <- d[d$time_age_at_ED>18, ]
#d <- d[!(d$vital_status != "Not reported as deceased" & is.na(d$time_date_of_death)), ]

## Combine enc diagnoses ------
colnames(d_enc_7days_end)[2:dim(d_enc_7days_end)[2]] <- paste0(colnames(d_enc_7days_end)[2:dim(d_enc_7days_end)[2]], "_enc_7days")
colnames(d_enc_30days_end)[2:dim(d_enc_30days_end)[2]] <- paste0(colnames(d_enc_30days_end)[2:dim(d_enc_30days_end)[2]], "_enc_30days")
colnames(d_enc_365days_end)[2:dim(d_enc_365days_end)[2]] <- paste0(colnames(d_enc_365days_end)[2:dim(d_enc_365days_end)[2]], "_enc_365days")
colnames(d_enc_anydays_end)[2:dim(d_enc_anydays_end)[2]] <- paste0(colnames(d_enc_anydays_end)[2:dim(d_enc_anydays_end)[2]], "_enc_anydays")

### Merge with master data -----
d_enc_diag <- data.table::merge.data.table(d_enc_7days_end, list(d_enc_30days_end, d_enc_365days_end, d_enc_anydays_end),
                                           by = "ID_encED_time", all.x = TRUE, all.y = TRUE)
rm(d_enc_7days_end, d_enc_30days_end, d_enc_365days_end, d_enc_anydays_end)

d_enc_diag[, (grep("ID_encED_time.", colnames(d_enc_diag))) := NULL]
d <- data.table::merge.data.table(d, d_enc_diag, by = "ID_encED_time",
                                  all.x = TRUE, all.y = FALSE); rm(d_enc_diag)


## Combine dia diagnoses ------
colnames(d_dia_7days_end)[2:dim(d_dia_7days_end)[2]] <- paste0(colnames(d_dia_7days_end)[2:dim(d_dia_7days_end)[2]], "_dia_7days")
colnames(d_dia_30days_end)[2:dim(d_dia_30days_end)[2]] <- paste0(colnames(d_dia_30days_end)[2:dim(d_dia_30days_end)[2]], "_dia_30days")
colnames(d_dia_365days_end)[2:dim(d_dia_365days_end)[2]] <- paste0(colnames(d_dia_365days_end)[2:dim(d_dia_365days_end)[2]], "_dia_365days")
colnames(d_dia_anydays_end)[2:dim(d_dia_anydays_end)[2]] <- paste0(colnames(d_dia_anydays_end)[2:dim(d_dia_anydays_end)[2]], "_dia_anydays")

### Merge with master data -----
d_dia_diag <- data.table::merge.data.table(d_dia_7days_end, d_dia_30days_end, by = "ID_encED_time",
                                           all.x = TRUE, all.y = TRUE); rm(d_dia_7days_end, d_dia_30days_end)

d_dia_diag <- data.table::merge.data.table(d_dia_diag, d_dia_365days_end, by = "ID_encED_time",
                                           all.x = TRUE, all.y = TRUE); rm(d_dia_365days_end)

d_dia_diag <- data.table::merge.data.table(d_dia_diag, d_dia_anydays_end, by = "ID_encED_time",
                                           all.x = TRUE, all.y = TRUE); rm(d_dia_anydays_end)

d <- data.table::merge.data.table(d, d_dia_diag, by = "ID_encED_time",
                                  all.x = TRUE, all.y = FALSE); rm(d_dia_diag)


## Combine rdt exams ------
d_rdt_1days$CXR       <- tolower(d_rdt_1days$rdt_test_desc) %in% tolower(code_cxr$rdt_test_desc)
d_rdt_cxr     <- d_rdt_1days[CXR == TRUE]
rm(d_rdt_1days)

d_rdt_cxr_sum <- d_rdt_cxr[, .(CXR_N = sum(CXR)), by = ID_encED_time] #Ignore warnings
rm(d_rdt_cxr)

### Merge with master data -----
d <- data.table::merge.data.table(d, d_rdt_cxr_sum, by = "ID_encED_time",
                                  all.x = TRUE, all.y = FALSE); rm(d_rdt_cxr_sum)


## Summary of lab results ------
d_lab_1days$TROPONIN <- tolower(d_lab_1days$lab_descript) %in% tolower(code_trop$lab_descript)
d_lab_1days$DDIMER   <- tolower(d_lab_1days$lab_descript) %in% tolower(code_ddimer$lab_descript)
d_lab_trop   <- d_lab_1days[TROPONIN == TRUE]
d_lab_ddimer <- d_lab_1days[DDIMER == TRUE]
rm(d_lab_1days)

d_lab_trop_sum <- d_lab_trop[, .(TROPONIN_N = sum(TROPONIN),
                                 TROPONIN_POS = any(!is.na(lab_result_abn)),
                                 TROPONIN_MAX = max(lab_result_pretty, na.rm = TRUE)),
                             by = ID_encED_time] #Ignore warnings

d_lab_ddimer_sum <- d_lab_ddimer[, .(DDIMER_N = sum(DDIMER),
                                     DDIMER_POS = any(!is.na(lab_result_abn)),
                                     DDIMER_MAX = max(lab_result_pretty, na.rm = TRUE)),
                                 by = ID_encED_time] #Ignore warnings
rm(d_lab_trop, d_lab_ddimer)

### Merge with master data -----
d <- data.table::merge.data.table(d, d_lab_trop_sum, by = "ID_encED_time",
                                  all.x = TRUE, all.y = FALSE)
d <- data.table::merge.data.table(d, d_lab_ddimer_sum, by = "ID_encED_time",
                                  all.x = TRUE, all.y = FALSE)
rm(d_lab_trop_sum, d_lab_ddimer_sum)


## Combine prc exams ------
d_prc_1days$Prc_ang     <- tolower(d_prc_1days$prc_code_type_code) %in% tolower(code_prc$prc_code_type_code)
d_prc_ang     <- d_prc_1days[Prc_ang == TRUE]
rm(d_prc_1days)

d_prc_ang_sum <- d_prc_ang[, .(PRC_N = sum(Prc_ang)), by = ID_encED_time] #Ignore warnings
rm(d_prc_ang)

### Merge with master data -----
d <- data.table::merge.data.table(d, d_prc_ang_sum, by = "ID_encED_time",
                                  all.x = TRUE, all.y = FALSE); rm(d_prc_ang_sum)


# Create new variables -----
d$LAB_N   <- rowSums(d[, c("TROPONIN_N", "DDIMER_N")], na.rm = TRUE)
d$LAB_POS <- rowSums(d[, c("TROPONIN_POS", "DDIMER_POS")], na.rm = TRUE)>0

### ICD endpoints -----
d$ANY_ENDPOINT_7days <- rowSums(d[, c("ANY_ENDPOINT_enc_7days", "ANY_ENDPOINT_dia_7days")], na.rm = TRUE)>0
d$MI_7days <- rowSums(d[, c("MI_enc_7days", "MI_dia_7days")], na.rm = TRUE)>0
d$PE_7days <- rowSums(d[, c("PE_enc_7days", "PE_dia_7days")], na.rm = TRUE)>0
d$AD_7days <- rowSums(d[, c("AD_enc_7days", "AD_dia_7days")], na.rm = TRUE)>0
d$time_ANY_ENDPOINT_7days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_ANY_ENDPOINT_enc_7days", "time_ANY_ENDPOINT_dia_7days")], 1, min, na.rm = TRUE), units = "days"))
d$time_MI_7days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_MI_enc_7days", "time_MI_dia_7days")], 1, min, na.rm = TRUE), units = "days"))
d$time_PE_7days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_PE_enc_7days", "time_PE_dia_7days")], 1, min, na.rm = TRUE), units = "days"))
d$time_AD_7days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_AD_enc_7days", "time_AD_dia_7days")], 1, min, na.rm = TRUE), units = "days"))

d$ANY_ENDPOINT_30days <- rowSums(d[, c("ANY_ENDPOINT_enc_30days", "ANY_ENDPOINT_dia_30days")], na.rm = TRUE)>0
d$MI_30days <- rowSums(d[, c("MI_enc_30days", "MI_dia_30days")], na.rm = TRUE)>0
d$PE_30days <- rowSums(d[, c("PE_enc_30days", "PE_dia_30days")], na.rm = TRUE)>0
d$AD_30days <- rowSums(d[, c("AD_enc_30days", "AD_dia_30days")], na.rm = TRUE)>0
d$time_ANY_ENDPOINT_30days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_ANY_ENDPOINT_enc_30days", "time_ANY_ENDPOINT_dia_30days")], 1, min, na.rm = TRUE), units = "days"))
d$time_MI_30days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_MI_enc_30days", "time_MI_dia_30days")], 1, min, na.rm = TRUE), units = "days"))
d$time_PE_30days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_PE_enc_30days", "time_PE_dia_30days")], 1, min, na.rm = TRUE), units = "days"))
d$time_AD_30days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_AD_enc_30days", "time_AD_dia_30days")], 1, min, na.rm = TRUE), units = "days"))

d$ANY_ENDPOINT_365days <- rowSums(d[, c("ANY_ENDPOINT_enc_365days", "ANY_ENDPOINT_dia_365days")], na.rm = TRUE)>0
d$MI_365days <- rowSums(d[, c("MI_enc_365days", "MI_dia_365days")], na.rm = TRUE)>0
d$PE_365days <- rowSums(d[, c("PE_enc_365days", "PE_dia_365days")], na.rm = TRUE)>0
d$AD_365days <- rowSums(d[, c("AD_enc_365days", "AD_dia_365days")], na.rm = TRUE)>0
d$time_ANY_ENDPOINT_365days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_ANY_ENDPOINT_enc_365days", "time_ANY_ENDPOINT_dia_365days")], 1, min, na.rm = TRUE), units = "days"))
d$time_MI_365days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_MI_enc_365days", "time_MI_dia_365days")], 1, min, na.rm = TRUE), units = "days"))
d$time_PE_365days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_PE_enc_365days", "time_PE_dia_365days")], 1, min, na.rm = TRUE), units = "days"))
d$time_AD_365days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_AD_enc_365days", "time_AD_dia_365days")], 1, min, na.rm = TRUE), units = "days"))

### Death endpoints -----
d$DEATH_7days <- (d$vital_status != "Not reported as deceased") &
  difftime(d$time_date_of_death, d$time_enc_admit, units = "days")<=7; d$DEATH_7days[is.na(d$DEATH_7days)] <- FALSE
d[, time_DEATH_7days := as.POSIXct(NA)]; d$time_DEATH_7days[d$DEATH_7days] <- d$time_date_of_death[d$DEATH_7days]
d[, DEATH_7days_source := NA]; d$DEATH_7days_source[d$DEATH_7days == TRUE] <- d$vital_status[d$DEATH_7days == TRUE]

d$DEATH_30days <- (d$vital_status != "Not reported as deceased") &
                   difftime(d$time_date_of_death, d$time_enc_admit, units = "days")<=30; d$DEATH_30days[is.na(d$DEATH_30days)] <- FALSE
d[, time_DEATH_30days := as.POSIXct(NA)]; d$time_DEATH_30days[d$DEATH_30days] <- d$time_date_of_death[d$DEATH_30days]
d[, DEATH_30days_source := NA]; d$DEATH_30days_source[d$DEATH_30days == TRUE] <- d$vital_status[d$DEATH_30days == TRUE]

d$DEATH_365days  <- (d$vital_status != "Not reported as deceased") &
                     difftime(d$time_date_of_death, d$time_enc_admit, units = "days")<=365; d$DEATH_365days[is.na(d$DEATH_365days)] <- FALSE
d[, time_DEATH_365days := as.POSIXct(NA)]; d$time_DEATH_365days[d$DEATH_365days] <- d$time_date_of_death[d$DEATH_365days]
d[, DEATH_365days_source := NA]; d$DEATH_365days_source[d$DEATH_365days == TRUE] <- d$vital_status[d$DEATH_365days == TRUE]

### Combined endpoints -----
d$ANY_ENDPOINT_DEATH_7days  <- rowSums(d[, c("ANY_ENDPOINT_7days", "DEATH_7days")], na.rm = TRUE)>0
d$time_ANY_ENDPOINT_DEATH_7days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_ANY_ENDPOINT_7days", "time_DEATH_7days")], 1, min, na.rm = TRUE), units = "days"))

d$ANY_ENDPOINT_DEATH_30days  <- rowSums(d[, c("ANY_ENDPOINT_30days", "DEATH_30days")], na.rm = TRUE)>0
d$time_ANY_ENDPOINT_DEATH_30days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_ANY_ENDPOINT_30days", "time_DEATH_30days")], 1, min, na.rm = TRUE), units = "days"))

d$ANY_ENDPOINT_DEATH_365days  <- rowSums(d[, c("ANY_ENDPOINT_365days", "DEATH_365days")], na.rm = TRUE)>0
d$time_ANY_ENDPOINT_DEATH_365days <- as.POSIXct(trunc.POSIXt(apply(d[, c("time_ANY_ENDPOINT_365days", "time_DEATH_365days")], 1, min, na.rm = TRUE), units = "days"))

### Save all results
save(list = ls(all.names = TRUE), file = paste0(folder_cache_home, "/", INSTITUTE, "_FINAL.RData"), envir = .GlobalEnv)
rm(list = ls())