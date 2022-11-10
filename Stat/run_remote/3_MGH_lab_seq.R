######README#####
# Create source specific RData files according to inclusion criteria

######
#jobsubmit -p basic -A circgp -m 370G -t 5-00:00:00 -c 20 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/3_MGH_lab_seq.R
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


# Process lab data ----
chunk_name <- paste0(folder_cache, "Laboratory_data_", INSTITUTE, ".RData")

d_lab_1days <- NULL
for(i in 1:n_cycles) {
  d_lab <- parseRPDR::load_lab(file = paste0(folder_rpdr, file_code, "_Clb_", i, ".txt"), nThread = nThread)
  
  ## Filter for appropriate rdt examinations -----
  code_trop  <- readxl::read_xlsx(path = paste0(folder_codes, "RPDR_Codes.xlsx"), sheet = "RPDR_LAB_codes")
  code_trop  <- code_trop[code_trop$Trop == 1, ]
  code_ddimer  <- readxl::read_xlsx(path = paste0(folder_codes, "RPDR_Codes.xlsx"), sheet = "RPDR_LAB_codes")
  code_ddimer  <- code_ddimer[code_ddimer$Ddimer == 1, ]
  
  d_lab <- d_lab[tolower(d_lab$lab_descript) %in% tolower(code_trop$lab_descript) |
                 tolower(d_lab$lab_descript) %in% tolower(code_ddimer$lab_descript)]
  
  ## Find lab results within 1 day of ED -----
  d_lab_1days_i <- find_exam(d_from = d_lab, d_to = d_enc_ED,
                             d_from_time = "time_lab",
                             d_to_time = "time_enc_admit",
                             time_diff_name = "time_diff_ED_lab",
                             add_column = "ID_encED_time",
                             before = TRUE, after = TRUE,
                             time = 1, time_unit = "days",
                             multiple = "all", nThread = nThread, shared_RAM = TRUE)
  
  ## Convert lab results to pretty -----
  d_lab_1days_i <- convert_lab(d_lab_1days_i)
  d_lab_1days   <- data.table::rbindlist(list(d_lab_1days, d_lab_1days_i))
}

## Save results -----
rm(list = c("d_lab_1days_i", "d_lab"))
save(list = ls(all.names = TRUE), file = chunk_name, envir = .GlobalEnv)
