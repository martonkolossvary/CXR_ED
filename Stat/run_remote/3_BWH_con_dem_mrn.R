######README#####
# Create source specific RData files according to inclusion criteria

######
#jobsubmit -p basic -A circgp -m 200G -t 1-00:00:00 -c 20 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/3_BWH_con_dem_mrn.R
######

#####Code to run on server for CXR_ED project
# Initialize -----
library(parseRPDR); library(data.table)
folder_wd    <- "/autofs/vast/circ/mjk2/CXR_ED/"
INSTITUTE    <- "BWH"
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE)
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
nThread      <- 20

# Variables to keep -----
vars_to_keep <- c("folder_wd", "INSTITUTE", "folder_rpdr", "folder_cache", "folder_codes", "nThread", "vars_to_keep")

# Process Patient-level information -----
chunk_name <- paste0(folder_cache, "Patient_data_", INSTITUTE, ".RData")
d_pat      <- parseRPDR::load_all(folder = paste0(folder_rpdr, "/"),
                                  which_data = c("mrn" , "con", "dem"),
                                  nThread = nThread, many_sources = FALSE)

## Remove duplicates and IDs with NA -----
d_pat_rm        <- lapply(d_pat, function(x) x[!is.na(x$ID_MERGE),])
d_pat_rm        <- lapply(d_pat_rm, function(x) x[!duplicated(x$ID_MERGE),])

## Merge patient-based datasources -----
d_pat_all <- data.table::merge.data.table(x = d_pat_rm$mrn, y = d_pat_rm$con,
                                          by = "ID_MERGE", all = TRUE)
d_pat_all <- data.table::merge.data.table(x = d_pat_all, y = d_pat_rm$dem,
                                          by = "ID_MERGE", all = TRUE)
## Save data -----
rm(d_pat, d_pat_rm)
save(list = ls(all.names = TRUE), file = chunk_name, envir = .GlobalEnv)
