#####README#####
# Build laboratory, radiological and procedural data dictionary of possible exam codes which will be used for inclusion criteria

# Initialize-----
library(parseRPDR); library(data.table)
folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/" #Location of dropbox folder
INSTITUTE    <- "BWH"
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_db    <- paste0(folder_wd, "DB/") #Location of intermediate databases: lab/rdt data dictionaries
nThread      <- 4


# Rdt data dictionary-----
d_rdt <- parseRPDR::load_all(folder = folder_rpdr, which_data = c("rdt"), nThread = nThread, many_sources = FALSE)
d_rdt_types <- d_rdt$rdt[, .N, by = .(rdt_mode, rdt_group, rdt_test_code, rdt_test_desc)]
data.table::setorder(d_rdt_types, cols = -"N")
data.table::fwrite(d_rdt_types, file = paste0(folder_db, "rdt_types.csv"))


# Lab data dictionary-----
d_lab <- parseRPDR::load_all(folder = folder_rpdr, which_data = c("lab"), nThread = nThread, many_sources = FALSE)
d_lab_types <- d_lab$lab[, .N, by = .(lab_group, lab_loinc, lab_testID, lab_descript)]
data.table::setorder(d_lab_types, cols = -"N")
data.table::fwrite(d_lab_types, file = paste0(folder_db, "lab_types.csv"))


# Prc data dictionary-----
prc_files <- list.files(folder_rpdr)[grep("prc", list.files(folder_rpdr), ignore.case = TRUE)]
d_prc_types_ALL <- data.table::data.table(prc_code_type = NA, prc_code = NA, prc_name = NA, prc_code_type_code = NA, N = NA)
for(i in prc_files) {
  d_prc <- parseRPDR::load_prc(file = paste0(folder_rpdr, i), nThread = nThread)
  d_prc_types <- d_prc[, .N, by = .(prc_code_type, prc_code, prc_name)] #Count how many
  d_prc_types[, prc_code_type_code := paste(prc_code_type, prc_code, sep = ":")] #Create merged ID
  
  d_prc_types_ALL <- data.table::merge.data.table(d_prc_types_ALL, d_prc_types, by = "prc_code_type_code", all = TRUE) #Merge with existing
  d_prc_types_ALL$N.x[is.na(d_prc_types_ALL$N.x)] <- 0; d_prc_types_ALL$N.y[is.na(d_prc_types_ALL$N.y)] <- 0
  d_prc_types_ALL$N.y <- as.numeric(d_prc_types_ALL$N.x) + as.numeric(d_prc_types_ALL$N.y)
  d_prc_types_ALL$prc_code.y[is.na(d_prc_types_ALL$prc_code.y)] <- d_prc_types_ALL$prc_code.x[is.na(d_prc_types_ALL$prc_code.y)]
  d_prc_types_ALL$prc_code_type.y[is.na(d_prc_types_ALL$prc_code_type.y)] <- d_prc_types_ALL$prc_code_type.x[is.na(d_prc_types_ALL$prc_code_type.y)]
  d_prc_types_ALL$prc_name.y[is.na(d_prc_types_ALL$prc_name.y)] <- d_prc_types_ALL$prc_name.x[is.na(d_prc_types_ALL$prc_name.y)]
  
  d_prc_types_ALL[, c("prc_code_type.x", "prc_code.x", "prc_name.x", "N.x") := NULL]
  colnames(d_prc_types_ALL) <- c("prc_code_type_code", "prc_code_type", "prc_code", "prc_name", "N")
}

data.table::setorder(d_prc_types_ALL, cols = -"N")
data.table::fwrite(d_prc_types_ALL, file = paste0(folder_db, "prc_types.csv"))

rm(list = ls())
