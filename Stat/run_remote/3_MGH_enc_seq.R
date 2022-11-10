######README#####
#1) First build laboratory and radiological data dictionary of possible exam codes: 1_data_dic.R
#2) Combine existing RPDR codebase with new data dictionary codes: 2_combine_RPDR_codes.R
#3) Run server code on server: 3_SERVER_preprocess.R

######
#jobsubmit -p basic -A circgp -m 370G -t 7-00:00:00 -c 20 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/3_MGH_enc_seq.R
#srun -p basic -A circgp -N 1 --ntasks-per-node=1 --mem=70G --time 0-02:00:00 --cpus-per-task=10 --pty bash
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

# Outcome definitions -----
diseases <- list(MI = c("410.01", "410.11", "410.21", "410.31", "410.41", "410.51", "410.61", "410.71", "410.81", "410.91",
                        "I22.0", "I22.1", "I22.2", "I22.8", "I22.9", "I21.01", "I21.02", "I21.09",
                        "I21.11", "I21.19", "I21.21", "I21.29", "I21.3", "I21.4", "I21.9", "I21.A1", "I21.A9"),
                 PE = c("415.1", "415.11", "415.12", "415.13", "415.19",
                        "I26", "I26.0", "I26.01", "I26.02", "I26.09", "I26.9",  "I26.90", "I26.92", "I26.93", "I26.94", "I26.99"),
                 AD = c("441", "441.01", "441.02", "441.03",
                        "I71.0", "I71.00", "I71.01", "I71.02", "I71.03"))
diseases_comb <- list(MI = c(paste0("ICD9:", c("410.01", "410.11", "410.21", "410.31", "410.41", "410.51", "410.61", "410.71", "410.81", "410.91")),
                             paste0("ICD10:", c("I22.0", "I22.1", "I22.2", "I22.8", "I22.9", "I21.01", "I21.02", "I21.09",
                                                "I21.11", "I21.19", "I21.21", "I21.29", "I21.3", "I21.4", "I21.9", "I21.A1", "I21.A9"))),
                      PE = c(paste0("ICD9:", c("415.1", "415.11", "415.12", "415.13", "415.19")),
                             paste0("ICD10:", c("I26", "I26.0", "I26.01", "I26.02", "I26.09", "I26.9",  "I26.90", "I26.92", "I26.93", "I26.94", "I26.99"))),
                      AD = c(paste0("ICD9:", c("441", "441.01", "441.02", "441.03")),
                             paste0("ICD10:", c("I71.0", "I71.00", "I71.01", "I71.02", "I71.03"))))

# Variables to keep -----
vars_to_keep <- c("folder_wd", "INSTITUTE", "folder_rpdr", "folder_cache", "folder_codes", "nThread", "diseases", "diseases_comb", "vars_to_keep")


# Process Encounter data ----
chunk_name <- paste0(folder_cache, "Encounter_data_", INSTITUTE, ".RData")

d_enc_ED = d_enc_7days_end = d_enc_30days_end = d_enc_365days_end = d_enc_anydays_end <- NULL
for(i in 1:n_cycles) {
  d_enc <- parseRPDR::load_enc(file = paste0(folder_rpdr, file_code, "_Enc_", i, ".txt"), nThread = nThread)
  
  ## Filter for ED encounters -----
  code_ED  <- readxl::read_xlsx(path = paste0(folder_codes, "RPDR_Codes.xlsx"), sheet = paste0("RPDR_ED_loc_", INSTITUTE))
  code_ED  <- code_ED[code_ED$ED == 1, ]
  d_enc_ED_i <- d_enc[tolower(d_enc$enc_clinic) %in% tolower(code_ED$Location)]
  d_enc_ED_i$ID_encED_time  <- paste(d_enc_ED_i$ID_MERGE, format(d_enc_ED_i$time_enc_admit, format = "%Y-%m-%d"), sep = "_")
  d_enc_ED_i <- unique(d_enc_ED_i, by = "ID_encED_time")
  
  ### Filter for given time period -----
  d_enc_ED_i <- d_enc_ED_i[time_enc_admit >= as.POSIXct("2005-01-01")  & time_enc_admit <= as.POSIXct("2015-12-31")]
  
  ## Save data -----
  d_enc_ED <- data.table::rbindlist(list(d_enc_ED, d_enc_ED_i))
  
  ## Find encounters within any, 365 and 30 days following ED -----
  d_enc_anydays <- find_exam(d_from = d_enc, d_to = d_enc_ED_i,
                             d_from_time = "time_enc_admit",
                             d_to_time = "time_enc_admit",
                             time_diff_name = "time_diff_ED_enc",
                             add_column = "ID_encED_time",
                             before = FALSE, after = TRUE,
                             time = 99999999, time_unit = "days",
                             multiple = "all", nThread = nThread, shared_RAM = TRUE)
  
  d_enc_7days   <- d_enc_anydays[time_diff_ED_enc <= 7]
  d_enc_30days  <- d_enc_anydays[time_diff_ED_enc <= 30]
  d_enc_365days <- d_enc_anydays[time_diff_ED_enc <= 365]
  
  ## Find whether endpoint definitions exist -----
  d_enc_7days_end_i <- convert_enc(d = d_enc_7days, codes_to_find = diseases, collapse = "ID_encED_time", nThread = nThread)
  d_enc_7days_end_i[, ANY_ENDPOINT := as.logical(apply(d_enc_7days_end_i[, .(MI, PE, AD)], 1, max, na.rm = TRUE))]
  suppressWarnings(d_enc_7days_end_i[, time_ANY_ENDPOINT := as.POSIXct(apply(d_enc_7days_end_i[, .(time_MI, time_PE, time_AD)],
                                                                             1, min, na.rm = TRUE), format = "%Y-%m-%d")])
  d_enc_7days_end <- data.table::rbindlist(list(d_enc_7days_end, d_enc_7days_end_i))
  
  d_enc_30days_end_i <- convert_enc(d = d_enc_30days, codes_to_find = diseases, collapse = "ID_encED_time", nThread = nThread)
  d_enc_30days_end_i[, ANY_ENDPOINT := as.logical(apply(d_enc_30days_end_i[, .(MI, PE, AD)], 1, max, na.rm = TRUE))]
  suppressWarnings(d_enc_30days_end_i[, time_ANY_ENDPOINT := as.POSIXct(apply(d_enc_30days_end_i[, .(time_MI, time_PE, time_AD)],
                                                                              1, min, na.rm = TRUE), format = "%Y-%m-%d")])
  d_enc_30days_end <- data.table::rbindlist(list(d_enc_30days_end, d_enc_30days_end_i))
  
  d_enc_365days_end_i <- convert_enc(d = d_enc_365days, codes_to_find = diseases, collapse = "ID_encED_time", nThread = nThread)
  d_enc_365days_end_i[, ANY_ENDPOINT := as.logical(apply(d_enc_365days_end_i[, .(MI, PE, AD)], 1, max, na.rm = TRUE))]
  suppressWarnings(d_enc_365days_end_i[, time_ANY_ENDPOINT := as.POSIXct(apply(d_enc_365days_end_i[, .(time_MI, time_PE, time_AD)],
                                                                               1, min, na.rm = TRUE), format = "%Y-%m-%d")])
  d_enc_365days_end <- data.table::rbindlist(list(d_enc_365days_end, d_enc_365days_end_i))
  
  d_enc_anydays_end_i <- convert_enc(d = d_enc_anydays, codes_to_find = diseases, collapse = "ID_encED_time", nThread = nThread)
  d_enc_anydays_end_i[, ANY_ENDPOINT := as.logical(apply(d_enc_anydays_end_i[, .(MI, PE, AD)], 1, max, na.rm = TRUE))]
  suppressWarnings(d_enc_anydays_end_i[, time_ANY_ENDPOINT := as.POSIXct(apply(d_enc_anydays_end_i[, .(time_MI, time_PE, time_AD)],
                                                                               1, min, na.rm = TRUE), format = "%Y-%m-%d")])
  d_enc_anydays_end <- data.table::rbindlist(list(d_enc_anydays_end, d_enc_anydays_end_i))
}
## Save results -----
rm(list = c("d_enc" ,"d_enc_ED_i", "d_enc_anydays", "d_enc_7days", "d_enc_30days", "d_enc_365days", "d_enc_7days_end_i", "d_enc_30days_end_i", "d_enc_365days_end_i", "d_enc_anydays_end_i"))
save(list = ls(all.names = TRUE), file = chunk_name, envir = .GlobalEnv)
save(list = c("d_enc_ED"), file = paste0(folder_cache, "ED_Encounter_data_", INSTITUTE, ".RData"), envir = .GlobalEnv)
