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

# Load final database -----
load(paste0(folder_cache, "FINAL_ALL.RData"))
pred  <- data.table::fread("/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/MODELS/V2/proj_densenet121_v2_pred.csv")
pred_2  <- data.table::fread("/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/MODELS/Finetune/proj_densenet121_v2_BWH_finetune_pred.csv")
colnames(pred_2) <- c("PNG_name_full", "ANY_ENDPOINT_DEATH_30days", "pred_0_finetune", "pred_1_finetune")

d_final <- data.table::merge.data.table(d_final, pred[, c("PNG_name_full", "pred_0", "pred_1")], by = "PNG_name_full",all.x = TRUE, all.y = FALSE)
d_final <- data.table::merge.data.table(d_final, pred_2[, c("PNG_name_full", "pred_0_finetune", "pred_1_finetune")], by = "PNG_name_full",all.x = TRUE, all.y = FALSE)

# Select cases for GradCAM -----
d_GC_MGH <- d_final[enc_hosp == "MGH"]
d_GC_MGH <- d_GC_MGH[is.na(Tuning)]
data.table::setorderv(x = d_GC_MGH, cols = c("pred_1", "ANY_ENDPOINT_DEATH_30days"), order = c(-1, -1))
d_GC_MGH_out <- data.table::rbindlist(list(d_GC_MGH[1:100, ], d_GC_MGH[5651:5750, ]))

d_GC_BWH <- d_final[enc_hosp == "BWH"]
d_GC_BWH_int <- d_GC_BWH[is.na(Tuning)]
data.table::setorderv(x = d_GC_BWH_int, cols = c("pred_1", "ANY_ENDPOINT_DEATH_30days"), order = c(-1, -1))
d_GC_BWH_int_out <- data.table::rbindlist(list(d_GC_BWH_int[1:100, ], d_GC_BWH_int[22665:22764, ]))

d_GC_BWH_ext <- d_GC_BWH[is.na(Tuning_BWH)]
data.table::setorderv(x = d_GC_BWH_ext, cols = c("pred_1", "ANY_ENDPOINT_DEATH_30days"), order = c(-1, -1))
d_GC_BWH_ext_out <- data.table::rbindlist(list(d_GC_BWH_ext[1:100, ], d_GC_BWH_ext[4453:4552, ]))

out <- data.table::rbindlist(list(d_GC_MGH_out, d_GC_BWH_int_out, d_GC_BWH_ext_out))

write.csv(out, "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/CACHE/CXR_ED_GradCAM.csv")
