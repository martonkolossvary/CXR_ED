######README#####
# Get accession numbers for CXRs within 1 day, and split them per 5000

# Initialize -----
library(parseRPDR); library(data.table)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
INSTITUTE    <- "MGH"
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")

chunk_name <- paste0(folder_cache, "Radiological_data_", INSTITUTE, ".RData")
load(chunk_name)


folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
INSTITUTE    <- "MGH"
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")

chunk_name <- paste0(folder_cache, INSTITUTE, "_FINAL", ".RData")
load(chunk_name)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
INSTITUTE    <- "MGH"
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")


# Filter for final data ----
d <- unique(d, by = "ID_encED_time") #Remove possible encounters on the same day

QUERY <- d #TOTAL POPULATION
CXR <- d[CXR_N>0] #HAS AT LEAST 1 CXR WITHIN 1 DAY
CXR_lab <- d[CXR_N>0 & (TROPONIN_N>0 | DDIMER_N>0)] #HAS AT LEAST 1 CXR AND 1 LAB RESULT WITHIN 1 DAY
CXR_card <- d[CXR_N>0 & PRC_N>0] #HAS AT LEAST 1 CXR AND 1 LAB RESULT WITHIN 1 DAY
CXR_lab_card <- d[CXR_N>0 & (TROPONIN_N>0 | DDIMER_N>0) & PRC_N>0] #HAS AT LEAST 1 CXR AND 1 LAB AND 1 CARDIAC RESULT WITHIN 1 DAY

CXR_lab_card <- unique(CXR_lab_card, by = "ID_MERGE") #If for patient level data
d_to <- CXR_lab_card[, c("ID_encED_time", "ID_MERGE", "ID_enc_EMPI"), with = FALSE]

# Merge with CXR data -----
d_rdt_1days$CXR       <- tolower(d_rdt_1days$rdt_test_desc) %in% tolower(code_cxr$rdt_test_desc)
d_rdt_cxr             <- d_rdt_1days[CXR == TRUE]

d_out <- data.table::merge.data.table(d_to, d_rdt_cxr, by = "ID_encED_time", all.x = TRUE, all.y = FALSE)

# Export data tables -----
how_many <- ceiling(length(unique(d_out$ID_MERGE.x))/5000)
splits <- split(unique(d_out$ID_MERGE.x), sort(1:length(unique(d_out$ID_MERGE.x)) %% how_many))

lapply(1:how_many, function(x) {
  #write.csv(d_out[ID_MERGE.x %in% splits[[x]]], paste0(folder_codes, "rdt/", INSTITUTE, "_rdt_", x, ".csv"))
  data.table::fwrite(d_out[ID_MERGE.x %in% splits[[x]]], paste0(folder_codes, "rdt/", INSTITUTE, "_rdt_", x, ".csv"))
})

rm(list = ls())
