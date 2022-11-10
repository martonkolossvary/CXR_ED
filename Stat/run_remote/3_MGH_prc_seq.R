######README#####
# Create source specific RData files according to inclusion criteria

######
#jobsubmit -p basic -A circgp -m 370G -t 7-00:00:00 -c 20 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/3_MGH_prc_seq.R -1
######

#####Code to run on server for CXR_ED project
i_in <- commandArgs(trailingOnly = TRUE)


# Initialize -----
library(parseRPDR); library(data.table)
folder_wd    <- "/autofs/vast/circ/mjk2/CXR_ED/"
INSTITUTE    <- "MGH"
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
nThread      <- 20
n_file_types <- 8
n_cycles <- length(list.files(folder_rpdr))/n_file_types
which <- i_in[1]
file_code    <- "ML690_20210514_095121"

# Variables to keep -----
vars_to_keep <- c("folder_wd", "INSTITUTE", "folder_rpdr", "folder_cache", "folder_codes", "nThread", "vars_to_keep")


# Load ED encounters ----
chunk_name <- paste0(folder_cache, "ED_Encounter_data_", INSTITUTE, ".RData")
load(chunk_name)


# Process dia data ----
d_prc_1days <- NULL
for(i in which) {
  chunk_name <- paste0(folder_cache, "prc_", INSTITUTE, "/", "Procedural_data_", INSTITUTE, "_", i, ".RData")
  d_prc <- parseRPDR::load_prc(file = paste0(folder_rpdr, file_code, "_Prc_", i, ".txt"), nThread = nThread)
  d_prc[, prc_code_type_code := paste(prc_code_type, prc_code, sep = ":")] #Create merged ID
  
  code_prc  <- readxl::read_xlsx(path = paste0(folder_codes, "RPDR_Codes.xlsx"), sheet = "RPDR_PRC_codes")
  code_prc  <- code_prc[code_prc$Prc_Ang == 1, ]
  
  d_prc <- d_prc[tolower(d_prc$prc_code_type_code) %in% tolower(code_prc$prc_code_type_code)]
  
  ## Find dia results within any, 365 or 30 day of ED -----
  d_prc_1days   <- find_exam(d_from = d_prc, d_to = d_enc_ED,
                             d_from_time = "time_prc",
                             d_to_time = "time_enc_admit",
                             time_diff_name = "time_diff_ED_prc",
                             add_column = "ID_encED_time",
                             before = TRUE, after = TRUE,
                             time = 1, time_unit = "days",
                             multiple = "all", nThread = nThread, shared_RAM = TRUE)
  
}
## Save results -----
rm(list = c("d_prc" ,"d_enc_ED"))
save(list = ls(all.names = TRUE), file = chunk_name, envir = .GlobalEnv)
