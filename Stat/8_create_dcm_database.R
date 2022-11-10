######README#####
# Create DICOM database and merge with rdt accession tables to have complete data
# Where data from file onwards is not available, the image is incorrect and needs to be deleted and manually downloaded
# Create a potential set of images that needs to be converted and then gone through 1 by 1


# Initialize -----
library(parseRPDR); library(data.table)

## Run on server -----
folder_wd    <- "/home/mkolossvary/CXR_ED/"
INSTITUTE    <- c("BWH", "MGH")
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_image <- paste0("/mnt/md0/mkolossvary/CXR_ED/IMAGES_Additional/")
nThread      <- 20

## Run locally -----
# folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
# INSTITUTE    <- c("BWH", "MGH")
# folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
# folder_cache <- paste0(folder_wd, "CACHE/")
# folder_codes <- paste0(folder_wd, "CODES/")
# folder_image <- paste0(folder_wd, "IMAGES/")
# nThread      <- 10


# Load MRN data for later QA/QC -----
attach(paste0(folder_cache, "Patient_data_BWH.RData"))
d_pat <- d_pat_all
detach()

attach(paste0(folder_cache, "Patient_data_MGH.RData"))
d_pat <- rbind(d_pat, d_pat_all, fill = TRUE)
detach()
d_pat[, colnames(d_pat)[c(7:20, 25:42, 44:dim(d_pat)[2])] := NULL] #Filter for only needed patient identifiers
d_pat <- d_pat[!duplicated(d_pat)]

# Create DICOM header database from downloaded images and filter for appropriate scans -----
t_start <- Sys.time()
folder_image <- paste0("/mnt/md0/mkolossvary/CXR_ED/IMAGES/")
dcm_1 <- create_img_db(folder_image, nThread = nThread)
folder_image <- paste0("/mnt/md0/mkolossvary/CXR_ED/IMAGES_Additional/")
dcm_2 <- create_img_db(folder_image, nThread = nThread)
print(paste("Time taken to build DICOM header database:", Sys.time() - t_start, "min"))

dcm <- data.table::rbindlist(list(dcm_1, dcm_2), fill = TRUE)

## Remove duplicates that have same accession and acquisition times -----
# dup <- duplicated(dcm, by = c("AccessionNumber", "time_acquisition"))
# dcm <- dcm[!dup]

## Select the earlier acquisition in case multiple have the same accession number (this only takes the first series of any accession)
# dcm <- dcm[order(AccessionNumber, time_acquisition)]
# dcm <- dcm[!duplicated(dcm$AccessionNumber)]


# Create large rdt table merging rdt tables used for mi2b2 to downloaded images -----
d_rdt_all <- NULL
for(inst in INSTITUTE) {
  files <- list.files(paste0(folder_codes), full.names = TRUE)
  files <- files[grep(inst, files, fixed = TRUE)]
  
  d_rdt <- lapply(1:length(files), function(x) {
    d_rdt_i <- data.table::fread(files[x], drop = 1); d_rdt_i[, ID_MERGE.y := NULL]
    colnames(d_rdt_i)[2] <- "ID_MERGE"; d_rdt_i[, ID_MERGE := as.character(ID_MERGE)]
    d_rdt_i$INSTITUTE <- inst
    d_rdt_i
  })
  d_rdt <- data.table::rbindlist(d_rdt)
  d_rdt <- d_rdt[!duplicated(d_rdt)]
  
  d_rdt_all <- rbind(d_rdt_all, d_rdt)
}

# Format large rdt table for merge with dcm -----
d_rdt_all <- d_rdt_all[rdt_accession != "null"] #Remove null accessions
d_rdt_all <- d_rdt_all[!(duplicated(d_rdt_all$rdt_accession) | duplicated(d_rdt_all$rdt_accession, fromLast = TRUE))] #Remove duplciated accessions, as we don't know the Institution in dcm
rdt_accession_letter   <- gsub("^[A-Z]", "", d_rdt_all$rdt_accession) #Remove leading letter
d_rdt_all$rdt_accession_letter_0 <- gsub("^0", "", rdt_accession_letter) #Remove leading 0
d_rdt_all <- d_rdt_all[!duplicated(d_rdt_all$rdt_accession_letter_0)] #Remove duplicated accessions without letter and 0 as we cannot merge

# Format dcm table to merge with rdt -----
rdt_accession_letter   <- gsub("^[A-Z]", "", dcm$AccessionNumber) #Remove leading letter
dcm$rdt_accession_letter_0 <- gsub("^0", "", rdt_accession_letter) #Remove leading 0

# Merge with dcm table where all rdt and dcm rows are maintained -----
d_rdt_all    <- data.table::merge.data.table(d_rdt_all, dcm, by = "rdt_accession_letter_0", all.x = TRUE, all.y = FALSE) #Exact matches
#d_rdt_all[dcm, on=.(rdt_accession_letter = AccessionNumber), names(dcm) := mget(paste0("i.", names(dcm)))] #Matches for NAs removing leading letter
#d_rdt_all[dcm, on=.(rdt_accession_letter_0 = AccessionNumber), names(dcm)[-1] := mget(paste0("i.", names(dcm)[-1]))]  #Matches for NAs removing leading 0

d_rdt_all <- data.table::merge.data.table(d_pat, d_rdt_all, by = "ID_MERGE", all.x = FALSE, all.y = TRUE)

# QA/QC due to modifications in accession numbers
d_rdt_all$time_between_rdt_study_hours <- difftime(d_rdt_all$time_rdt, d_rdt_all$time_study, units = "hours") 
d_rdt_all[abs(time_between_rdt_study_hours) > 24 & !is.na(time_between_rdt_study_hours),
          c(colnames(dcm)[3:dim(dcm)[2]], "time_between_rdt_study_hours") := NA] #From file onward delete data if non complient
d_rdt_all$ID_img <- paste0(1:dim(d_rdt_all)[1], ".png")

# Write results -----
#data.table::fwrite(x = d_rdt_all, file = paste0(folder_codes, "rdt/dcm/FINAL_rdt_dcm_table.csv"), dateTimeAs = "write.csv")
save(d_rdt_all, file = paste0(folder_cache, "RDT_DCM_data.RData"), envir = .GlobalEnv)



# Filter for potential images -----
dup <- duplicated(d_rdt_all$file) | duplicated(d_rdt_all$file, fromLast = TRUE) | is.na(d_rdt_all$file)
d_rdt_all <- d_rdt_all[!dup]

d_rdt_all <- d_rdt_all[!is.na(time_between_rdt_study_hours)] #Does not exist
d_rdt_all <- d_rdt_all[is.na(ViewPosition) | ViewPosition == "" | ViewPosition == "PA" | ViewPosition == "AP"] #Filter for accurate positions
d_rdt_all <- d_rdt_all[is.na(PhotometricInterpretation) | PhotometricInterpretation == "MONOCHROME1" | PhotometricInterpretation == "MONOCHROME2"] #Non monochrome


save(d_rdt_all, file = paste0(folder_cache, "RDT_DCM_data_potential.RData"), envir = .GlobalEnv)
#rm(list = ls())