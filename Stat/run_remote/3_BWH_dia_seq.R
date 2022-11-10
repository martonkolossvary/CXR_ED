######README#####
# Create source specific RData files according to inclusion criteria

######
#jobsubmit -p basic -A circgp -m 370G -t 7-00:00:00 -c 20 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/3_BWH_dia_seq.R 1
######

#####Code to run on server for CXR_ED project
i_in <- commandArgs(trailingOnly = TRUE)


# Initialize -----
library(parseRPDR); library(data.table)
folder_wd    <- "/autofs/vast/circ/mjk2/CXR_ED/"
INSTITUTE    <- "BWH"
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
nThread      <- 20
n_file_types <- 8
n_cycles <- length(list.files(folder_rpdr))/n_file_types
which <- i_in[1]
file_code    <- "ML690_20210423_043348"

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


# Load ED encounters ----
chunk_name <- paste0(folder_cache, "ED_Encounter_data_", INSTITUTE, ".RData")
load(chunk_name)


# Process dia data ----
d_dia_7days_end = d_dia_30days_end = d_dia_365days_end = d_dia_anydays_end <- NULL
for(i in which) {
 chunk_name <- paste0(folder_cache, "dia_", INSTITUTE, "/", "Diagnosis_data_", INSTITUTE, "_", i, ".RData")
 d_dia <- parseRPDR::load_dia(file = paste0(folder_rpdr, file_code, "_Dia_", i, ".txt"), nThread = nThread)
 
 
 ## Find dia results within any, 365 or 30 day of ED -----
 d_dia_anydays <- find_exam(d_from = d_dia, d_to = d_enc_ED,
                            d_from_time = "time_dia",
                            d_to_time = "time_enc_admit",
                            time_diff_name = "time_diff_ED_dia",
                            add_column = "ID_encED_time",
                            before = FALSE, after = TRUE,
                            time = 9999999, time_unit = "days",
                            multiple = "all", nThread = nThread, shared_RAM = TRUE)
 
 d_dia_7days   <- d_dia_anydays[time_diff_ED_dia <= 7]
 d_dia_30days  <- d_dia_anydays[time_diff_ED_dia <= 30]
 d_dia_365days <- d_dia_anydays[time_diff_ED_dia <= 365]
 
 
 ## Find whether endpoint definitions exist -----
 d_dia_7days_end_i <- convert_dia(d = d_dia_7days, codes_to_find = diseases_comb, collapse = "ID_encED_time", nThread = nThread)
 d_dia_7days_end_i[, ANY_ENDPOINT := as.logical(apply(d_dia_7days_end_i[, .(MI, PE, AD)], 1, max, na.rm = TRUE))]
 suppressWarnings(d_dia_7days_end_i[, time_ANY_ENDPOINT := as.POSIXct(apply(d_dia_7days_end_i[, .(time_MI, time_PE, time_AD)],
                                                                            1, min, na.rm = TRUE), format = "%Y-%m-%d", tz = "est")])
 d_dia_7days_end <- data.table::rbindlist(list(d_dia_7days_end, d_dia_7days_end_i))
 
 d_dia_30days_end_i <- convert_dia(d = d_dia_30days, codes_to_find = diseases_comb, collapse = "ID_encED_time", nThread = nThread)
 d_dia_30days_end_i[, ANY_ENDPOINT := as.logical(apply(d_dia_30days_end_i[, .(MI, PE, AD)], 1, max, na.rm = TRUE))]
 suppressWarnings(d_dia_30days_end_i[, time_ANY_ENDPOINT := as.POSIXct(apply(d_dia_30days_end_i[, .(time_MI, time_PE, time_AD)],
                                                                             1, min, na.rm = TRUE), format = "%Y-%m-%d", tz = "est")])
 d_dia_30days_end <- data.table::rbindlist(list(d_dia_30days_end, d_dia_30days_end_i))
 
 d_dia_365days_end_i <- convert_dia(d = d_dia_365days, codes_to_find = diseases_comb, collapse = "ID_encED_time", nThread = nThread)
 d_dia_365days_end_i[, ANY_ENDPOINT := as.logical(apply(d_dia_365days_end_i[, .(MI, PE, AD)], 1, max, na.rm = TRUE))]
 suppressWarnings(d_dia_365days_end_i[, time_ANY_ENDPOINT := as.POSIXct(apply(d_dia_365days_end_i[, .(time_MI, time_PE, time_AD)],
                                                                              1, min, na.rm = TRUE), format = "%Y-%m-%d", tz = "est")])
 d_dia_365days_end <- data.table::rbindlist(list(d_dia_365days_end, d_dia_365days_end_i))
 
 d_dia_anydays_end_i <- convert_dia(d = d_dia_anydays, codes_to_find = diseases_comb, collapse = "ID_encED_time", nThread = nThread)
 d_dia_anydays_end_i[, ANY_ENDPOINT := as.logical(apply(d_dia_anydays_end_i[, .(MI, PE, AD)], 1, max, na.rm = TRUE))]
 suppressWarnings(d_dia_anydays_end_i[, time_ANY_ENDPOINT := as.POSIXct(apply(d_dia_anydays_end_i[, .(time_MI, time_PE, time_AD)],
                                                                              1, min, na.rm = TRUE), format = "%Y-%m-%d", tz = "est")])
 d_dia_anydays_end <- data.table::rbindlist(list(d_dia_anydays_end, d_dia_anydays_end_i))
}
## Save results -----
rm(list = c("d_dia" ,"d_enc_ED", "d_dia_anydays", "d_dia_7days", "d_dia_30days", "d_dia_365days", "d_dia_7days_end_i", "d_dia_30days_end_i", "d_dia_365days_end_i", "d_dia_anydays_end_i"))
save(list = ls(all.names = TRUE), file = chunk_name, envir = .GlobalEnv)
