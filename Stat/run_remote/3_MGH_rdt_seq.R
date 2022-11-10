######README#####
# Create source specific RData files according to inclusion criteria

######
#jobsubmit -p basic -A circgp -m 370G -t 5-00:00:00 -c 20 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/3_MGH_rdt_seq.R
######

#####Code to run on server for CXR_ED project


# Initialize -----
library(parseRPDR); library(data.table)
folder_wd    <- "/autofs/vast/circ/mjk2/CXR_ED/"
INSTITUTE    <- "MGH"
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
nThread      <- 20
n_file_types <- 8
file_code    <- "ML690_20210423_042519"
n_cycles <- length(list.files(folder_rpdr))/n_file_types

# Variables to keep -----
vars_to_keep <- c("folder_wd", "INSTITUTE", "folder_rpdr", "folder_cache", "folder_codes", "nThread", "vars_to_keep")

# Load ED encounters ----
chunk_name <- paste0(folder_cache, "ED_Encounter_data_", INSTITUTE, ".RData")
load(chunk_name)

# Process Rdt data ----
chunk_name <- paste0(folder_cache, "Radiological_data_", INSTITUTE, ".RData")

d_rdt_1days <- NULL
for(i in 1:n_cycles) {
  d_rdt <- parseRPDR::load_rdt(file = paste0(folder_rpdr, file_code, "_Rdt_", i, ".txt"), nThread = nThread)
  
  ## Filter for appropriate rdt examinations -----
  code_cxr  <- readxl::read_xlsx(path = paste0(folder_codes, "RPDR_Codes.xlsx"), sheet = "RPDR_RDT_codes")
  code_cxr  <- code_cxr[code_cxr$CXR == 1, ]
  code_ang  <- readxl::read_xlsx(path = paste0(folder_codes, "RPDR_Codes.xlsx"), sheet = "RPDR_RDT_codes")
  code_ang  <- code_ang[code_ang$Rad_Ang == 1, ]
  
  d_rdt <- d_rdt[tolower(d_rdt$rdt_test_desc) %in% tolower(code_cxr$rdt_test_desc) |
                 tolower(d_rdt$rdt_test_desc) %in% tolower(code_ang$rdt_test_desc)]
  
  ## Find rdt results within 1 day of ED -----
  d_rdt_1days_i <- find_exam(d_from = d_rdt, d_to = d_enc_ED,
                             d_from_time = "time_rdt",
                             d_to_time = "time_enc_admit",
                             time_diff_name = "time_diff_ED_rdt",
                             add_column = "ID_encED_time",
                             before = TRUE, after = TRUE,
                             time = 1, time_unit = "days",
                             multiple = "all", nThread = nThread, shared_RAM = TRUE)
  
  d_rdt_1days   <- data.table::rbindlist(list(d_rdt_1days, d_rdt_1days_i))
}
## Save results -----
rm(list = c("d_rdt_1days_i", "d_rdt"))
save(list = ls(all.names = TRUE), file = chunk_name, envir = .GlobalEnv)