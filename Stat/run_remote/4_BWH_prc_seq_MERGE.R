######README#####
# Merge files where batches were run seperately

######
#jobsubmit -p basic -A circgp -m 370G -t 1-00:00:00 -c 20 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/4_BWH_prc_seq_MERGE.R
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
n_cycles <- 14

folder_cache_home <- paste0(folder_cache, "prc_", INSTITUTE, "/")

# Merge all data to one -----
d_prc_1days_i <- NULL
for(i in 1:n_cycles) {
  f_prc    <- paste0(folder_cache_home, "/Procedural_data_", INSTITUTE, "_", i, ".RData"); load(f_prc)
  d_prc_1days_i <- data.table::rbindlist(list(d_prc_1days_i, d_prc_1days))
  rm(list = "d_prc_1days")
}

#Save in original format -----
d_prc_1days  <- d_prc_1days_i

rm(list = c("d_prc" ,"d_enc_ED", "d_prc_1days_i"))
rm(list = c("folder_cache_home" , "i"))
chunk_name <- paste0(folder_wd, "CACHE/", "Procedural_data_", INSTITUTE, ".RData")
save(list = ls(all.names = TRUE), file = chunk_name, envir = .GlobalEnv)

