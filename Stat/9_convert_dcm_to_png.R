######README#####
# Convert dcm to png by rescaling based on the longer axis in the image

# Initialize -----
library(parseRPDR); library(data.table); library(reticulate)
#use_miniconda("/home/mkolossvary/miniconda3/envs/Penv/", required = TRUE)
pydicom <- import("pydicom")
pd      <- import("pandas")

## Run on server -----
folder_wd    <- "/home/mkolossvary/CXR_ED/"
INSTITUTE    <- c("BWH", "MGH")
folder_rpdr  <- paste0(folder_wd, "RPDR/", INSTITUTE, "/")
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_PNG   <- "/mnt/md0/mkolossvary/CXR_ED/PNG/"
nThread      <- 20
arguments <- "-O +G +i 1 +on"


# Load database ----
attach(paste0(folder_cache, "RDT_DCM_data_potential.RData"))
d_rdt <- d_rdt_all
detach()


# Convert images -----
cat(message(paste0("CONVERTING ", dim(d_rdt)[1], " DICOM IMAGES TO PNG")))
pb <- txtProgressBar(min = 1, dim(d_rdt)[1], initial = 1, style = 3)
for(i in 1:dim(d_rdt)[1]) {
  
  ## Read dcm header and decide direction of scaling -----
  dcm_header <- try(pydicom$dcmread(fp = d_rdt$file[i], stop_before_pixels = TRUE), silent = TRUE)
  if(inherits(dcm_header, "try-error")) next

  rows    <- try(as.character(dcm_header[["Rows"]]$value), silent = TRUE)
  columns <- try(as.character(dcm_header[["Columns"]]$value), silent = TRUE)
  if(inherits(rows, "try-error") | inherits(columns, "try-error")) next
  
  if(columns > rows) {
    arguments_i <- paste(arguments, "+Sxv 512")
  } else {
    arguments_i <- paste(arguments, "+Syv 512")
  }
  
  ## Run system command -----
  command <- paste(arguments_i, d_rdt$file[i], paste0(folder_PNG, d_rdt$ID_img[i]))
  system2(command = "dcmj2pnm", args = command)
  cat(setTxtProgressBar(pb, i))
}
close(pb)