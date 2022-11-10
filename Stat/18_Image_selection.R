######README#####
# RUN ROC SUB ANALYSES

# Need to manually change model type and i for the for cycle in the roc calculations

# Initialize -----
library(parseRPDR); library(data.table); library(ggplot2); library(pROC)
set.seed(42)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_image <- paste0(folder_wd, "IMAGES/")
folder_pred  <- paste0(folder_wd, "MODELS/")
folder_stat  <- paste0(folder_wd, "STAT/")
nThread      <- 10

load(paste0(folder_cache, "FINAL_ALL.RData"))
grad <- data.table::fread("/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/CACHE/CXR_ED_GradCAM.csv")

data <- data.table::merge.data.table(d_final, grad, by = "PNG_name_full",all.x = FALSE, all.y = TRUE)


cols <- c("ID_MERGE", "PNG_name_full", "time_enc_admit", "gender", "enc_diag_princ", "time_age_at_ED", "race", "ANY_ENDPOINT_30days", "time_ANY_ENDPOINT_30days",
          "MI_30days", "time_MI_30days", "PE_30days", "time_PE_30days", "AD_30days", "time_AD_30days", "DEATH_30days", "time_DEATH_30days",
          "TROPONIN_POS", "DDIMER_POS", "pred_0", "pred_1", "pred_0_finetune", "pred_1_finetune")

## MGH TEST CASES ----- #
load(paste0(folder_cache, "RAW/Procedural_data_MGH.RData"))
grad_MGH <- data[enc_hosp.x == "MGH"]
setorderv(x = grad_MGH, cols = c("pred_1", "ANY_ENDPOINT_DEATH_30days.x"), order = c(-1, -1))
grad_MGH[, 1:5]
grad_MGH[, ..cols][TROPONIN_POS == TRUE & ANY_ENDPOINT_30days == 1]
grad_MGH[PNG_name_full == "mnt/md0/mkolossvary/CXR_ED/PNG_ALL/73921.png", ..cols]
d_prc_1days[ID_MERGE == "105045437"]

## BWH TEST CASES ----- #
load(paste0(folder_cache, "RAW/Procedural_data_BWH.RData"))
grad_BWH <- data[enc_hosp.x == "BWH"]
setorderv(x = grad_BWH, cols = c("pred_1_finetune", "ANY_ENDPOINT_DEATH_30days.x"), order = c(-1, -1))
grad_BWH[, ..cols][TROPONIN_POS == TRUE & ANY_ENDPOINT_30days == 1]

grad_BWH[PNG_name_full == "mnt/md0/mkolossvary/CXR_ED/PNG_ALL/63953.png", ..cols]
d_prc_1days[ID_MERGE == "104350421"]

grad_BWH[390:400, 1:5]
grad_BWH[390:400, ..cols]
d_prc_1days[ID_MERGE == "105316668"]

