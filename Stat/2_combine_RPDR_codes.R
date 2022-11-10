#####README#####
#2) Merge created data dictionaries with existing ones and create new csv for inclusions which need to be imported into excel using the data import wizard


# Initialize-----
library(parseRPDR); library(data.table)
folder_dropbox  <- "/Users/mjk2/Dropbox (Partners HealthCare)/" #Location of dropbox folder
folder_project  <- paste0(folder_dropbox, "CXR_ED/") #Current directory
folder_codes    <- paste0(folder_project, "CODES/") #Location of codes specifying examinations and encounters
folder_db       <- paste0(folder_project, "DB/") #Location of intermediate databases: lab/rdt data dictionaries
INSTITUTE       <- "BWH"

# Load RPDR rdt and lab codes and remove possible duplicates-----
code_rdt  <- readxl::read_xlsx(path = paste0(folder_codes, "RPDR_Codes.xlsx"), sheet = "RPDR_RDT_codes"); code_rdt <- data.table::as.data.table(code_rdt)
code_lab  <- readxl::read_xlsx(path = paste0(folder_codes, "RPDR_Codes.xlsx"), sheet = "RPDR_LAB_codes"); code_lab <- data.table::as.data.table(code_lab)
code_prc  <- readxl::read_xlsx(path = paste0(folder_codes, "RPDR_Codes.xlsx"), sheet = "RPDR_PRC_codes"); code_prc <- data.table::as.data.table(code_prc)

code_rdt <- unique(code_rdt, by = c("rdt_test_desc", "rdt_mode", "rdt_group", "rdt_test_code"))
code_lab <- unique(code_lab, by = c("lab_testID", "lab_group", "lab_loinc", "lab_descript"))
code_prc <- unique(code_prc, by = c("prc_code_type_code", "prc_code_type", "prc_code", "prc_name"))

# Load rdt and lab data dictionaries for project and remove possible duplicates-----
dic_rdt  <- data.table::fread(paste0(folder_db, "rdt_types.csv"))
dic_lab  <- data.table::fread(paste0(folder_db, "lab_types.csv"))
dic_prc  <- data.table::fread(paste0(folder_db, "prc_types.csv"))

dic_rdt <- unique(dic_rdt, by = c("rdt_test_desc", "rdt_mode", "rdt_group", "rdt_test_code"))
dic_lab <- unique(dic_lab, by = c("lab_testID", "lab_group", "lab_loinc", "lab_descript"))
dic_prc <- unique(dic_prc, by = c("prc_code_type_code", "prc_code_type", "prc_code", "prc_name"))

# Merge, process and save rdt and lab codes-----
out_rdt <- data.table::merge.data.table(x = dic_rdt, y = code_rdt, by = "rdt_test_desc", all = TRUE)
out_rdt$N.x[is.na(out_rdt$N.x)] <- 0; out_rdt$N.y[is.na(out_rdt$N.y)] <- 0
out_rdt$N.x <- as.numeric(out_rdt$N.x) + as.numeric(out_rdt$N.y)
out_rdt[, c("rdt_mode.y", "rdt_group.y", "rdt_test_code.y", "N.y") := NULL]
colnames(out_rdt) <- c("rdt_test_desc", "rdt_mode", "rdt_group", "rdt_test_code", "N", "CXR", "Rad_Ang")
data.table::setorder(out_rdt, cols = -"N")
out_rdt <- unique(out_rdt, by = c("rdt_test_desc", "rdt_mode", "rdt_group", "rdt_test_code"))
write.csv(out_rdt, paste0(folder_codes, "RPDR_Codes_rdt.csv"), na = "", row.names = FALSE)

out_lab <- data.table::merge.data.table(x = dic_lab, y = code_lab, by = "lab_testID", all = TRUE)
out_lab$N.x[is.na(out_lab$N.x)] <- 0; out_lab$N.y[is.na(out_lab$N.y)] <- 0
out_lab$N.x <- as.numeric(out_lab$N.x) + as.numeric(out_lab$N.y)
out_lab[, c("lab_group.y", "lab_loinc.y", "lab_descript.y", "N.y") := NULL]
colnames(out_lab) <- c("lab_testID", "lab_group", "lab_loinc", "lab_descript", "N", "Trop", "Ddimer")
data.table::setorder(out_lab, cols = -"N")
out_lab <- unique(out_lab, by = c("lab_testID", "lab_group", "lab_loinc", "lab_descript"))
write.csv(out_lab, paste0(folder_codes, "RPDR_Codes_lab.csv"), na = "", row.names = FALSE)

out_prc <- data.table::merge.data.table(x = dic_prc, y = code_prc, by = "prc_code_type_code", all = TRUE)
out_prc$N.x[is.na(out_prc$N.x)] <- 0; out_prc$N.y[is.na(out_prc$N.y)] <- 0
out_prc$N.x <- as.numeric(out_prc$N.x) + as.numeric(out_prc$N.y)
out_prc[, c("prc_code_type.y", "prc_code.y", "prc_name.y", "N.y") := NULL]
colnames(out_prc) <- c("prc_code_type_code", "prc_code_type", "prc_code", "prc_name", "N", "Prc_Ang")
data.table::setorder(out_prc, cols = -"N")
out_prc <- unique(out_prc, by = c("prc_code_type_code", "prc_code_type", "prc_code", "prc_name"))
write.csv(out_prc, paste0(folder_codes, "RPDR_Codes_prc.csv"), na = "", row.names = FALSE)

#Results for both MGH and BWH need to be manually imported using data import from text setting every column to text in excel!
rm(list = ls())
