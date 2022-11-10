######README#####
# Parse Rad files


# Initialize -----
library(parseRPDR); library(data.table); library(ggplot2)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
INSTITUTE    <- c("MGH")
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_image <- paste0(folder_wd, "IMAGES/")
folder_pred  <- paste0(folder_wd, "MODELS/")
folder_stat  <- paste0(folder_wd, "STAT/")
nThread      <- 10

# Load final database -----
load(paste0(folder_cache, "FINAL_ALL.RData"))
rad <- parseRPDR::load_all(paste0(folder_wd, "RPDR/Rad/"))

# Filter and parse
match    <-  rad$rad$rad_rep_num %in% d_final$rdt_accession
rad_filt <- rad$rad[rad$rad$rad_rep_num %in% d_final$rdt_accession]
rad_filt <- unique(rad_filt)

rad_parse <- parseRPDR::convert_notes(rad_filt, code = "rad_rep_txt", anchors = c("Exam Number", "Ordering Provider",
                                                                                  "HISTORY", "Associated Reports", "Report Below",
                                                                                  "REASON", "REPORT", "TECHNIQUE", "COMPARISON", "FINDINGS",
                                                                                  "IMPRESSION", "RECOMMENDATION", "SIGNATURES", "report_end"))
data.table::fwrite(rad_parse, file = paste0(folder_wd, "DB/Rad.csv"), dateTimeAs = "write.csv")
