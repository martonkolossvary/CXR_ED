######README#####
# Merge files where batches were run seperately

######
#jobsubmit -p basic -A circgp -m 370G -t 1-00:00:00 -c 20 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/dia_MGH/4_BWH_dia_seq_MERGE.R
######


#####Code to add up Dia data - run on server due to potential timezone problems


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
file_code <- "ML690_20210423_043348"

folder_cache_home <- paste0(folder_cache, "dia_", INSTITUTE, "/")

# Merge all data to one -----
d_dia_7days_end_i = d_dia_30days_end_i = d_dia_365days_end_i = d_dia_anydays_end_i <- NULL
for(i in 1:n_cycles) {
  f_dia    <- paste0(folder_cache_home, "/Diagnosis_data_", INSTITUTE, "_", i, ".RData"); load(f_dia)
  
  d_dia_7days_end_i <- data.table::rbindlist(list(d_dia_7days_end_i, d_dia_7days_end))
  rm(list = "d_dia_7days_end")
  d_dia_30days_end_i <- data.table::rbindlist(list(d_dia_30days_end_i, d_dia_30days_end))
  rm(list = "d_dia_30days_end")
  d_dia_365days_end_i <- data.table::rbindlist(list(d_dia_365days_end_i, d_dia_365days_end))
  rm(list = "d_dia_365days_end")
  d_dia_anydays_end_i <- data.table::rbindlist(list(d_dia_anydays_end_i, d_dia_anydays_end))
  rm(list = "d_dia_anydays_end")
}

#Save in original format -----
d_dia_7days_end  <- d_dia_7days_end_i
rm(list = "d_dia_7days_end_i")
d_dia_30days_end  <- d_dia_30days_end_i
rm(list = "d_dia_30days_end_i")
d_dia_365days_end <- d_dia_365days_end_i
rm(list = "d_dia_365days_end_i")
d_dia_anydays_end <- d_dia_anydays_end_i
rm(list = "d_dia_anydays_end_i")

rm(list = c("d_dia" ,"d_enc_ED", "d_dia_7days", "d_dia_30days", "d_dia_365days", "d_dia_anydays", "d_dia_7days_end_i", "d_dia_30days_end_i", "d_dia_365days_end_i", "d_dia_anydays_end_i"))
rm(list = c("folder_cache_home" , "i"))
chunk_name <- paste0(folder_wd, "CACHE/", "Diagnosis_data_", INSTITUTE, ".RData")
save(list = ls(all.names = TRUE), file = chunk_name, envir = .GlobalEnv)

