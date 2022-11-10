######README#####
# RUN ROC ANALYSES


# Initialize -----
library(parseRPDR); library(data.table); library(ggplot2); library(glmnet)
set.seed(42)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_image <- paste0(folder_wd, "IMAGES/")
folder_pred  <- paste0(folder_wd, "MODELS/")
folder_stat  <- paste0(folder_wd, "STAT/")
nThread      <- 10

model_type <- "ALL" # Which results to use: V2, Finetune, ALL
REGULARIZE <- FALSE # Use Lasso regression
STRATIFY   <- NULL # Stratify using "AP" / "PA" / NULL
if(model_type == "V2") {
  which_training_set <- "d_train"; which_test_set <- "d_test_BWH" # d_tune / d_test_MGH / d_test_BWH
} else if(model_type == "Finetune") {
  which_training_set <- "d_train_ft"; which_test_set <- "d_test_BWH_ft" # d_tune_ft / d_test_BWH_ft
} else if(model_type == "ALL") {
  which_training_set <- "d_train_all"; which_test_set <- "d_test_BWH_all" # d_tune_all / d_test_all / d_test_MGH_all / d_test_BWH_all
}

fn_col    <- "PNG_name_full"
label_col <- "ANY_ENDPOINT_DEATH_30days"
valid_col          <- "Tuning"
valid_col_finetune <- "Tuning_BWH"
valid_col_all      <- "Tuning_ALL"
DL_pred   <- "pred_1"
hosp      <- "enc_hosp"
roc_outcomes <- c("ANY_ENDPOINT_DEATH_30days", "ANY_ENDPOINT_30days", "DEATH_30days", "MI_30days", "PE_30days", "AD_30days")
covariates_1 <- c("time_age_at_ED", "gender", "BIOMARKER_POS")
covariates_2 <- c("ViewPosition") 
DL_labels <- c("Enlarged_Cardiomediastinum", "Cardiomegaly", "Lung_Lesion", "Lung_Opacity",
               "Edema", "Consolidation", "Pneumonia", "Atelectasis", "Pneumothorax", "Pleural_Effusion",
               "Pleural_Other", "Fracture", "Support_Devices")
covariates_3 <- c(DL_labels)

# Files to load -----
which_set        <- list.files(paste0(folder_pred, model_type), full.names = TRUE)
which_set_names  <- list.files(paste0(folder_pred, model_type), full.names = FALSE)
which_set        <- which_set[grep("pred", which_set)]
which_set_names  <- which_set_names[grep("pred", which_set_names)]
which_set_names  <- gsub("_pred.csv", "", which_set_names, fixed = TRUE)


# Load final database -----
load(paste0(folder_cache, "FINAL_ALL.RData"))
rad_dl <- data.table::fread(paste0(folder_wd, "DB/Radiology_Reports_With_Chexpert_Labels_022122.csv")) # Radiological report data

rad_dl <- rad_dl[!duplicated(rad_dl$ID_MERGE), ] # Remove duplicates
for(col in DL_labels) set(rad_dl, i=which(rad_dl[[col]]==1), j=col, value=2) # Change values so 0=None, 1=Uncertain, 2=Present
for(col in DL_labels) set(rad_dl, i=which(rad_dl[[col]]==-1), j=col, value=1) # Change values so 0=None, 1=Uncertain, 2=Present
rad_dl$ID_MERGE <- as.character(rad_dl$ID_MERGE)

d_final <- data.table::merge.data.table(d_final, rad_dl, by = "ID_MERGE", all.x = TRUE, all.y = FALSE) # Merge data
setnafill(d_final, type = "const", fill = 1, cols = DL_labels) # Fill missing values with Uncertain
d_final[, (DL_labels) := lapply(.SD, as.factor), .SDcols = DL_labels] # Convert to factor
d_final[, (c("gender", "BIOMARKER_POS", "ViewPosition")) := lapply(.SD, as.factor), .SDcols = c("gender", "BIOMARKER_POS", "ViewPosition")] # Convert to factor

Death_CV = CV = Death = MI = PE = AD <- matrix(NA, nrow = length(which_set), ncol = 12)
Outcomes = list(Death_CV, CV, Death, MI, PE, AD)

# Run on all predictions -----
for(i in 1:length(which_set)) {
  # Create dataset
  pred  <- data.table::fread(paste0(which_set[i]))
  d_roc <- data.table::merge.data.table(d_final[, c(fn_col, hosp, valid_col, valid_col_finetune, valid_col_all, roc_outcomes, covariates_1, covariates_2, covariates_3), with = FALSE],
                                        pred[, c(fn_col, DL_pred), with = FALSE],
                                        by = fn_col, all.x = FALSE, all.y = TRUE)
  if(!is.null(STRATIFY)) {
    d_roc <- d_roc[d_roc$ViewPosition == STRATIFY] # ONLY EVALUATE ON GIVEN VIEW POSITION
  }
  
  d_train <- d_roc[get(valid_col) == 0 & !is.na(get(valid_col)), ]
  d_tune  <- d_roc[get(valid_col) == 1 & !is.na(get(valid_col)), ]
  d_test_MGH <- d_roc[is.na(get(valid_col)) & get(hosp) == "MGH", ]
  d_test_BWH <- d_roc[is.na(get(valid_col)) & get(hosp) == "BWH", ]
  
  d_train_ft <- d_roc[get(valid_col_finetune) == 0 & !is.na(get(valid_col_finetune)), ]
  d_tune_ft  <- d_roc[get(valid_col_finetune) == 1 & !is.na(get(valid_col_finetune)), ]
  #d_test_MGH_ft <- d_roc[is.na(get(valid_col_finetune)) & get(hosp) == "MGH", ]
  d_test_BWH_ft <- d_roc[is.na(get(valid_col_finetune)) & get(hosp) == "BWH", ]
  
  d_train_all <- d_roc[get(valid_col_all) == 0 & !is.na(get(valid_col_all)), ]
  d_tune_all  <- d_roc[get(valid_col_all) == 1 & !is.na(get(valid_col_all)), ]
  d_test_MGH_all <- d_roc[is.na(get(valid_col_all)) & get(hosp) == "MGH", ]
  d_test_BWH_all <- d_roc[is.na(get(valid_col_all)) & get(hosp) == "BWH", ]
  d_test_all <- d_roc[is.na(get(valid_col_all)), ]
  
  d_train_i <- data.table::copy(eval(parse(text = which_training_set)))
  d_test_i <- data.table::copy(eval(parse(text = which_test_set)))
  
  # Calculate ROCs
  ## Parameter specifications - TRAINING -----
  x_m1 <- cbind(d_train_i$time_age_at_ED, as.numeric(d_train_i$gender)-1, as.numeric(d_train_i$BIOMARKER_POS)-1)
  x_m2 <- cbind(x_m1, abs(as.numeric(d_train_i$ViewPosition)-2))
  x_m3 <- cbind(x_m2, d_train_i$pred_1)
  x_DL <- cbind(d_train_i$pred_1, abs(as.numeric(d_train_i$ViewPosition)-2))
  
  inter_m2 <- cbind(x_m2[, 1]*x_m2[, 4], x_m2[, 2]*x_m2[, 4], x_m2[, 3]*x_m2[, 4])
  inter_m3 <- cbind(inter_m2, x_m3[, 5]*x_m3[, 4])
  inter_DL <- cbind(x_DL[, 1]*x_DL[, 2])
  
  DL_notes <- as.data.frame(d_train_i[, DL_labels, with = FALSE])
  colnames(DL_notes) <- paste0("X", as.character(1:length(colnames(DL_notes))))
  DL_notes_test <- as.data.frame(d_test_i[, DL_labels, with = FALSE])
  colnames(DL_notes_test) <- paste0("X", as.character(1:length(colnames(DL_notes_test))))
  
  ## Parameter specifications - TESTING -----
  x_m1_test <- cbind(d_test_i$time_age_at_ED, as.numeric(d_test_i$gender)-1, as.numeric(d_test_i$BIOMARKER_POS)-1)
  x_m2_test <- cbind(x_m1_test, abs(as.numeric(d_test_i$ViewPosition)-2))
  x_m3_test <- cbind(x_m2_test, d_test_i$pred_1)
  x_DL_test <- cbind(d_test_i$pred_1, abs(as.numeric(d_test_i$ViewPosition)-2))
  
  inter_m2_test <- cbind(x_m2_test[, 1]*x_m2_test[, 4], x_m2_test[, 2]*x_m2_test[, 4], x_m2_test[, 3]*x_m2_test[, 4])
  inter_m3_test <- cbind(inter_m2_test, x_m3_test[, 5]*x_m3_test[, 4])
  inter_DL_test <- cbind(x_DL_test[, 1]*x_DL_test[, 2])
  
  DL_notes_all  <- glmnet::makeX(train = DL_notes, test = DL_notes_test)
  DL_notes      <- DL_notes_all$x
  DL_notes_test <- DL_notes_all$xtest
  
  if(REGULARIZE) {
    ## Model specifications -----
    model_1  <- lapply(roc_outcomes, function(y) {glmnet::cv.glmnet(y = d_train_i[[y]], x = x_m1, family = "binomial", type.measure="auc", nfolds = 10)})
    model_2  <- lapply(roc_outcomes, function(y) {glmnet::cv.glmnet(y = d_train_i[[y]], x = cbind(x_m2, inter_m2, DL_notes), family = "binomial", type.measure="auc", nfolds = 10)})
    model_3  <- lapply(roc_outcomes, function(y) {glmnet::cv.glmnet(y = d_train_i[[y]], x = cbind(x_m3, inter_m3, DL_notes), family = "binomial", type.measure="auc", nfolds = 10)})
    model_DL <- lapply(roc_outcomes, function(y) {glmnet::cv.glmnet(y = d_train_i[[y]], x = cbind(x_DL, inter_DL), family = "binomial", type.measure="auc", nfolds = 10)})
    
    pred_model_1  <- lapply(model_1, function(model) {as.numeric(predict(model, s = "lambda.1se",  newx = x_m1_test, type = "link"))})
    pred_model_2  <- lapply(model_2, function(model) {as.numeric(predict(model, s = "lambda.1se",  newx = cbind(x_m2_test, inter_m2_test, DL_notes_test), type = "link"))})
    pred_model_3  <- lapply(model_3, function(model) {as.numeric(predict(model, s = "lambda.1se",  newx = cbind(x_m3_test, inter_m3_test, DL_notes_test), type = "link"))})
    pred_model_DL <- lapply(model_DL, function(model) {as.numeric(predict(model, s = "lambda.1se",  newx = cbind(x_DL_test, inter_DL_test), type = "link"))})
  } else {
    ## Model specifications -----
    model_1  <- lapply(roc_outcomes, function(y) {glmnet::glmnet(y = d_train_i[[y]], x = x_m1, family = "binomial", lambda = 0)})
    model_2  <- lapply(roc_outcomes, function(y) {glmnet::glmnet(y = d_train_i[[y]], x = cbind(x_m2, inter_m2, DL_notes), family = "binomial", lambda = 0)})
    model_3  <- lapply(roc_outcomes, function(y) {glmnet::glmnet(y = d_train_i[[y]], x = cbind(x_m3, inter_m3, DL_notes), family = "binomial", lambda = 0)})
    model_DL <- lapply(roc_outcomes, function(y) {glmnet::glmnet(y = d_train_i[[y]], x = cbind(x_DL, inter_DL), family = "binomial", lambda = 0)})
    
    pred_model_1  <- lapply(model_1, function(model) {as.numeric(predict(model, s = 0,  newx = x_m1_test, type = "link"))})
    pred_model_2  <- lapply(model_2, function(model) {as.numeric(predict(model, s = 0,  newx = cbind(x_m2_test, inter_m2_test, DL_notes_test), type = "link"))})
    pred_model_3  <- lapply(model_3, function(model) {as.numeric(predict(model, s = 0,  newx = cbind(x_m3_test, inter_m3_test, DL_notes_test), type = "link"))})
    pred_model_DL <- lapply(model_DL, function(model) {as.numeric(predict(model, s = 0,  newx = cbind(x_DL_test, inter_DL_test), type = "link"))})
  }
  
  roc_model_1   <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[[roc_outcomes[i]]], predictor = pred_model_1[[i]], ci = TRUE)})
  roc_model_2   <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[[roc_outcomes[i]]], predictor = pred_model_2[[i]], ci = TRUE)})
  roc_model_3   <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[[roc_outcomes[i]]], predictor = pred_model_3[[i]], ci = TRUE)})
  roc_model_DL  <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[[roc_outcomes[i]]], predictor = pred_model_DL[[i]], ci = TRUE)})
  
  for(j in 1:length(Outcomes)) {
    Outcomes[[j]][i, 1] <- which_set_names[i]
    Outcomes[[j]][i, 2] <- format(round(as.numeric(roc_model_1[[j]]$ci)[2], 3), nsmall = 3)
    Outcomes[[j]][i, 3] <- paste0("[", format(round(as.numeric(roc_model_1[[j]]$ci)[1], 3), nsmall = 3), "; ",
                                  format(round(as.numeric(roc_model_1[[j]]$ci)[3], 3), nsmall = 3), "]")
    Outcomes[[j]][i, 4] <- format(round(as.numeric(roc_model_2[[j]]$ci)[2], 3), nsmall = 3)
    Outcomes[[j]][i, 5] <- paste0("[", format(round(as.numeric(roc_model_2[[j]]$ci)[1], 3), nsmall = 3), "; ",
                                  format(round(as.numeric(roc_model_2[[j]]$ci)[3], 3), nsmall = 3), "]")
    Outcomes[[j]][i, 6] <- format(round(as.numeric(roc_model_3[[j]]$ci)[2], 3), nsmall = 3)
    Outcomes[[j]][i, 7] <- paste0("[", format(round(as.numeric(roc_model_3[[j]]$ci)[1], 3), nsmall = 3), "; ",
                                  format(round(as.numeric(roc_model_3[[j]]$ci)[3], 3), nsmall = 3), "]")
    Outcomes[[j]][i, 8] <- format(round(as.numeric(roc_model_DL[[j]]$ci)[2], 3), nsmall = 3)
    Outcomes[[j]][i, 9] <- paste0("[", format(round(as.numeric(roc_model_DL[[j]]$ci)[1], 3), nsmall = 3), "; ",
                                   format(round(as.numeric(roc_model_DL[[j]]$ci)[3], 3), nsmall = 3), "]")
    Outcomes[[j]][i, 10] <- format(round(pROC::roc.test(roc_model_1[[j]], roc_model_2[[j]], paired = TRUE, method = "delong")$p.value, 3), nsmall = 3)
    Outcomes[[j]][i, 11] <- format(round(pROC::roc.test(roc_model_2[[j]], roc_model_3[[j]], paired = TRUE, method = "delong")$p.value, 3), nsmall = 3)
    Outcomes[[j]][i, 12] <- format(round(pROC::roc.test(roc_model_1[[j]], roc_model_DL[[j]], paired = TRUE, method = "delong")$p.value, 3), nsmall = 3)
  }
}

Outcomes <- lapply(Outcomes, function(x) {
  colnames(x) = c("Name", "Model_1", "Model_1_CI", "Model_2", "Model_2_CI", "Model_3", "Model_3_CI",
                  "Model_DL", "Model_DL_CI", "p_M1_vs_M2", "p_M2_vs_M3", "p_M1_vs_DL")
  x})

lapply(1:length(Outcomes), function(x) {write.csv(Outcomes[[x]], paste0(folder_stat, which_test_set, "_", roc_outcomes[x], "_ROC.csv"), row.names = FALSE)})

# CLOSE =====
rm(list = ls()); gc(); dev.off()