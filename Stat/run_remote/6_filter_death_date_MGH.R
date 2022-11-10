######README#####
# Check for IDs who died but without a date, whether they have later information so we can see if its later than 1 year

#jobsubmit -p basic -A circgp -m 50G -t 2-00:00:00 -c 6 -M ALL Rscript /autofs/vast/circ/mjk2/CXR_ED/SCRIPTS/6_filter_death_date_MGH.R


# Initialize -----
library(parseRPDR); library(data.table)
folder_wd         <- "/autofs/vast/circ/mjk2/CXR_ED/"
INSTITUTE    <- "MGH"
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_db    <- paste0(folder_wd, "DB/") #Location of intermediate databases: lab/rdt data dictionaries
n_file_types <- 8
n_cycles <- length(list.files(folder_rpdr))/n_file_types
file_code  <- "ML690_20210423_042519" #BWH: "ML690_20210423_043348"  MGH: "ML690_20210423_042519"
file_code2 <- "ML690_20210514_095121" #BWH: "ML690_20210514_095408"  MGH: "ML690_20210514_095121

load(paste0(folder_cache, INSTITUTE, "_FINAL.RData"))

file_code  <- "ML690_20210423_042519" #BWH: "ML690_20210423_043348"  MGH: "ML690_20210423_042519"
file_code2 <- "ML690_20210514_095121" #BWH: "ML690_20210514_095408"  MGH: "ML690_20210514_095121
nThread      <- 6

# Get IDs which batch they are in -----
## Create list of IDs and batch numbers
sources <- c("Mrn")
ids <- vector(mode = 'list', length = length(sources)); names(ids) <- sources
for(i in 1:n_cycles) {
  ids_i <- lapply(sources, function(x) {
    id_i <- fread(paste0(folder_rpdr, file_code, "_", x, "_", i, ".txt"))
    if(x != "Mrn") {id_i <- cbind(unique(id_i$EMPI), rep(i, length(unique(id_i$EMPI))))
    } else {
      id_i <- cbind(unique(id_i$IncomingId), rep(i, length(unique(id_i$IncomingId))))
    }
  })
  if(i == 1){
    ids <- ids_i
  } else{
    ids <- Map(rbind, ids, ids_i)
  }
}
names(ids) <- sources
d_list_ids <- as.data.table(ids$Mrn)
colnames(d_list_ids) <- c("ID_MERGE", "batch")
d_list_ids$ID_MERGE <- as.character(d_list_ids$ID_MERGE)
rm(list = c("ids_i", "ids", "sources"))

## Filter for list of individuals who do not have death data ----
problematic <- (d$vital_status != "Not reported as deceased" & is.na(d$time_date_of_death)) | is.na(d$vital_status)
d_IDs <- d[problematic, c("ID_MERGE", "ID_encED_time", "time_enc_admit")]

d_IDs  <- merge.data.table(d_IDs, d_list_ids, by = "ID_MERGE")
d_IDs$time_latest_data <- as.POSIXct(rep(NA, dim(d_IDs)[1]))
d_IDs <- d_IDs[order(batch)]

# Cycle through batches for data -----
for(i in 1:max(d_IDs$batch)) {
  d_lab <- load_lab(paste0(folder_rpdr, file_code, "_Clb_", i, ".txt"), nThread = nThread)
  d_dia <- load_dia(paste0(folder_rpdr, file_code, "_Dia_", i, ".txt"), nThread = nThread)
  d_enc <- load_enc(paste0(folder_rpdr, file_code, "_Enc_", i, ".txt"), nThread = nThread)
  d_rdt <- load_rdt(paste0(folder_rpdr, file_code, "_Rdt_", i, ".txt"), nThread = nThread)
  d_prc <- load_prc(paste0(folder_rpdr, file_code2, "_Prc_", i, ".txt"), nThread = nThread)
  
  in_batch <- which(d_IDs$batch == i)
  for(j in in_batch) {
    i_lab <- d_lab[ID_MERGE == d_IDs$ID_MERGE[j]]; i_lab <- max(i_lab$time_lab)
    i_dia <- d_dia[ID_MERGE == d_IDs$ID_MERGE[j]]; i_dia <- max(i_dia$time_dia)
    i_enc <- d_enc[ID_MERGE == d_IDs$ID_MERGE[j]]; i_enc <- max(i_enc$time_enc_admit)
    i_rdt <- d_rdt[ID_MERGE == d_IDs$ID_MERGE[j]]; i_rdt <- max(i_rdt$time_rdt)
    i_prc <- d_prc[ID_MERGE == d_IDs$ID_MERGE[j]]; i_prc <- max(i_prc$time_prc)
    d_IDs$time_latest_data[j] <- max(c(i_lab, i_dia, i_enc, i_rdt, i_prc), na.rm = TRUE)
  }
  rm(list = c("d_lab", "d_dia", "d_enc", "d_rdt", "d_prc", "in_batch"))
}

# Merge with database and calculate time to encounter ----
d <- data.table::merge.data.table(d, d_IDs[, c("ID_encED_time", "time_latest_data")], by = "ID_encED_time", all.x = TRUE, all.y = TRUE)
d$time_diff_enc_latest <- difftime(d$time_latest_data, d$time_enc_admit, units = "days")

# If no data is available change death indicator -----
d[problematic, ]$DEATH_7days <- ifelse(d[problematic, ]$time_diff_enc_latest > 7, FALSE, NA)
d[problematic, ]$DEATH_30days <- ifelse(d[problematic, ]$time_diff_enc_latest > 30, FALSE, NA)
d[problematic, ]$DEATH_365days <- ifelse(d[problematic, ]$time_diff_enc_latest > 365, FALSE, NA)

# Save final data -----
rm(list = c("i_lab", "i_dia", "i_enc", "i_rdt", "i_prc", "d_IDs", "d_list_ids"))
save(list = ls(all.names = TRUE), file = paste0(folder_cache, INSTITUTE, "_FINAL_CORRECTED.RData"), envir = .GlobalEnv)
rm(list = ls())
