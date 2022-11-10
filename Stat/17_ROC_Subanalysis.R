######README#####
# RUN ROC SUB ANALYSES

# Need to manually change model type and i for the for cycle in the roc calculations

# Initialize -----
library(parseRPDR); library(data.table); library(ggplot2); library(pROC)
set.seed(42)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_image <- paste0(folder_wd, "IMAGES/")
folder_pred  <- paste0(folder_wd, "MODELS/")
folder_stat  <- paste0(folder_wd, "STAT/")
nThread      <- 10

model_type <- "V2" # Which results to use: V2, Finetune
REGULARIZE <- FALSE # Use Lasso regression
STRATIFY   <- NULL # Stratify using "AP" / "PA" / NULL
if(model_type == "V2") {
  which_training_set <- "d_train"; which_test_set <- "d_test_MGH" # d_test_MGH / d_test_BWH
} else if(model_type == "Finetune") {
  which_training_set <- "d_train_ft"; which_test_set <- "d_test_BWH_ft" # d_test_BWH_ft
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
d_final$race <- factor(d_final$race, levels = c("African American", "American Indian", "Asian", "Asian American Indian", "Asian Hawaiian", "Asian Pacific Islander",
                                                "Black", "European", "Hawaiian", "Hispanic", "Hispanic Black", "Hispanic White", "Not Recorded", "White", "<NA>"),
                       labels = c("Black", "Non-Hispanic white", "Non-Hispanic white", "Non-Hispanic white", "Non-Hispanic white", "Non-Hispanic white",
                                  "Black", "Non-Hispanic white", "Non-Hispanic white", "Hispanic white", "Black", "Hispanic white", NA, "Non-Hispanic white", NA))
d_final$time_between_rdt_study_hours <- as.numeric(d_final$time_between_rdt_study_hours)

Death_CV = CV = Death = MI = PE = AD <- matrix(NA, nrow = length(which_set), ncol = 12)
Outcomes = list(Death_CV, CV, Death, MI, PE, AD)



# Run on all predictions -----
for(i in 7) {  #7: DenseNet121v2 for V2; 1: DenseNet121v2 finetuned
  # Create dataset
  pred  <- data.table::fread(paste0(which_set[i]))
  d_roc <- data.table::merge.data.table(d_final[, c(fn_col, hosp, valid_col, valid_col_finetune, valid_col_all, roc_outcomes, covariates_1, covariates_2, covariates_3, "race"), with = FALSE],
                                        pred[, c(fn_col, DL_pred), with = FALSE],
                                        by = fn_col, all.x = FALSE, all.y = TRUE)
  if(!is.null(STRATIFY)) {
    d_roc <- d_roc[d_roc$ViewPosition == STRATIFY] # ONLY TRAIN AND EVALUATE ON GIVEN VIEW POSITION
  }
  
  d_train <- d_roc[get(valid_col) == 0 & !is.na(get(valid_col)), ]
  d_train_ft <- d_roc[get(valid_col_finetune) == 0 & !is.na(get(valid_col_finetune)), ]
  d_test_MGH <- d_roc[is.na(get(valid_col)) & get(hosp) == "MGH", ]
  d_test_BWH <- d_roc[is.na(get(valid_col)) & get(hosp) == "BWH", ]
  d_test_BWH_ft <- d_roc[is.na(get(valid_col_finetune)) & get(hosp) == "BWH", ]
  
  d_test_MGH$above_median_age <- (d_test_MGH$time_age_at_ED >= median(d_test_MGH$time_age_at_ED))*1
  d_test_BWH$above_median_age <- (d_test_BWH$time_age_at_ED >= median(d_test_BWH$time_age_at_ED))*1
  d_test_BWH_ft$above_median_age <- (d_test_BWH_ft$time_age_at_ED >= median(d_test_BWH_ft$time_age_at_ED))*1
  
  d_train_i <- data.table::copy(eval(parse(text = which_training_set)))
  d_test_i <- data.table::copy(eval(parse(text = which_test_set)))
  
  # Filter for given group
  #d_test_i <- d_test_i[above_median_age == 1]
  #d_test_i <- d_test_i[gender == "Female"]
  #d_test_i <- d_test_i[race == "Hispanic white"] # "Black" "Non-Hispanic white" "Hispanic white"
  
  # Calculate ROCs
  ## Parameter specifications - TRAINING -----
  x_m1 <- cbind(d_train_i$time_age_at_ED, as.numeric(d_train_i$gender)-1)
  x_m2 <- cbind(d_train_i$time_age_at_ED, as.numeric(d_train_i$gender)-1, as.numeric(d_train_i$BIOMARKER_POS)-1)
  x_m3 <- cbind(d_train_i$time_age_at_ED, as.numeric(d_train_i$gender)-1, as.numeric(d_train_i$BIOMARKER_POS)-1, d_train_i$pred_1)
  x_DL <- cbind(d_train_i$pred_1, 1)
  
  inter_m2 <- NULL
  inter_m3 <- NULL
  inter_DL <- NULL
  
  DL_notes <- as.data.frame(d_train_i[, DL_labels, with = FALSE])
  colnames(DL_notes) <- paste0("X", as.character(1:length(colnames(DL_notes))))
  DL_notes_test <- as.data.frame(d_test_i[, DL_labels, with = FALSE])
  colnames(DL_notes_test) <- paste0("X", as.character(1:length(colnames(DL_notes_test))))
  
  ## Parameter specifications - TESTING -----
  x_m1_test <- cbind(d_test_i$time_age_at_ED, as.numeric(d_test_i$gender)-1)
  x_m2_test <- cbind(d_test_i$time_age_at_ED, as.numeric(d_test_i$gender)-1, as.numeric(d_test_i$BIOMARKER_POS)-1)
  x_m3_test <- cbind(d_test_i$time_age_at_ED, as.numeric(d_test_i$gender)-1, as.numeric(d_test_i$BIOMARKER_POS)-1, d_test_i$pred_1)
  x_DL_test <- cbind(d_test_i$pred_1, 1)
  
  inter_m2_test <- NULL
  inter_m3_test <- NULL
  inter_DL_test <- NULL
  
  DL_notes_all  <- glmnet::makeX(train = DL_notes, test = DL_notes_test)
  DL_notes      <- DL_notes_all$x
  DL_notes_test <- DL_notes_all$xtest
  
  if(REGULARIZE) {
    ## Model specifications -----
    model_1  <- lapply(roc_outcomes, function(y) {glmnet::cv.glmnet(y = d_train_i[[y]], x = x_m1, family = "binomial", type.measure="auc", nfolds = 10)})
    model_2  <- lapply(roc_outcomes, function(y) {glmnet::cv.glmnet(y = d_train_i[[y]], x = cbind(x_m2, inter_m2), family = "binomial", type.measure="auc", nfolds = 10)})
    model_3  <- lapply(roc_outcomes, function(y) {glmnet::cv.glmnet(y = d_train_i[[y]], x = cbind(x_m3, inter_m3), family = "binomial", type.measure="auc", nfolds = 10)})
    model_DL <- lapply(roc_outcomes, function(y) {glmnet::cv.glmnet(y = d_train_i[[y]], x = cbind(x_DL, inter_DL), family = "binomial", type.measure="auc", nfolds = 10)})
    
    pred_model_1  <- lapply(model_1, function(model) {as.numeric(predict(model, s = "lambda.1se",  newx = x_m1_test, type = "link"))})
    pred_model_2  <- lapply(model_2, function(model) {as.numeric(predict(model, s = "lambda.1se",  newx = cbind(x_m2_test, inter_m2_test), type = "link"))})
    pred_model_3  <- lapply(model_3, function(model) {as.numeric(predict(model, s = "lambda.1se",  newx = cbind(x_m3_test, inter_m3_test), type = "link"))})
    pred_model_DL <- lapply(model_DL, function(model) {as.numeric(predict(model, s = "lambda.1se",  newx = cbind(x_DL_test, inter_DL_test), type = "link"))})
  } else {
    ## Model specifications -----
    model_1  <- lapply(roc_outcomes, function(y) {glmnet::glmnet(y = d_train_i[[y]], x = x_m1, family = "binomial", lambda = 0)})
    model_2  <- lapply(roc_outcomes, function(y) {glmnet::glmnet(y = d_train_i[[y]], x = cbind(x_m2, inter_m2), family = "binomial", lambda = 0)})
    model_3  <- lapply(roc_outcomes, function(y) {glmnet::glmnet(y = d_train_i[[y]], x = cbind(x_m3, inter_m3), family = "binomial", lambda = 0)})
    model_DL <- lapply(roc_outcomes, function(y) {glmnet::glmnet(y = d_train_i[[y]], x = cbind(x_DL, inter_DL), family = "binomial", lambda = 0)})
    
    pred_model_1  <- lapply(model_1, function(model) {as.numeric(predict(model, s = 0,  newx = x_m1_test, type = "link"))})
    pred_model_2  <- lapply(model_2, function(model) {as.numeric(predict(model, s = 0,  newx = cbind(x_m2_test, inter_m2_test), type = "link"))})
    pred_model_3  <- lapply(model_3, function(model) {as.numeric(predict(model, s = 0,  newx = cbind(x_m3_test, inter_m3_test), type = "link"))})
    pred_model_DL <- lapply(model_DL, function(model) {as.numeric(predict(model, s = 0,  newx = cbind(x_DL_test, inter_DL_test), type = "link"))})
  }
  
  roc_model_1   <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[[roc_outcomes[i]]], predictor = pred_model_1[[i]], ci = TRUE)})
  roc_model_2   <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[[roc_outcomes[i]]], predictor = pred_model_2[[i]], ci = TRUE)})
  roc_model_3   <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[[roc_outcomes[i]]], predictor = pred_model_3[[i]], ci = TRUE)})
  roc_model_DL  <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[[roc_outcomes[i]]], predictor = pred_model_DL[[i]], ci = TRUE)})
  
  # roc_model_1   <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[["ANY_ENDPOINT_DEATH_30days"]], predictor = pred_model_1[[i]], ci = TRUE)})
  # roc_model_2   <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[["ANY_ENDPOINT_DEATH_30days"]], predictor = pred_model_2[[i]], ci = TRUE)})
  # roc_model_3   <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[["ANY_ENDPOINT_DEATH_30days"]], predictor = pred_model_3[[i]], ci = TRUE)})
  # roc_model_DL  <- lapply(1:length(roc_outcomes), function(i) {pROC::roc(response = d_test_i[["ANY_ENDPOINT_DEATH_30days"]], predictor = pred_model_DL[[i]], ci = TRUE)})
}


# Gather ROCs =====
# Manually run above on the 3 types of outcomes and save accordingly
ROC_test_MGH_1 <- roc_model_1; ROC_test_MGH_2 <- roc_model_2; ROC_test_MGH_3 <- roc_model_3; ROC_test_MGH_DL <- roc_model_DL
ROC_test_BWH_1 <- roc_model_1; ROC_test_BWH_2 <- roc_model_2; ROC_test_BWH_3 <- roc_model_3; ROC_test_BWH_DL <- roc_model_DL
ROC_test_BWHft_1 <- roc_model_1; ROC_test_BWHft_2 <- roc_model_2; ROC_test_BWHft_3 <- roc_model_3; ROC_test_BWHft_DL <- roc_model_DL
# =========


# Create ROC curves =====
colors_4   <- rev(ggsci::pal_npg()(4))

## Create ROC curves for MGH test -----
p_roc_test_MGH <- list()
for(i in 1:length(ROC_test_MGH_1)) {
  p_roc_test_MGH[[i]] <- ggroc(
    list("Model-1" = ROC_test_MGH_1[[i]],
         "Model-2" = ROC_test_MGH_2[[i]],
         "Model-3" = ROC_test_MGH_3[[i]],
         "Model-DL" = ROC_test_MGH_DL[[i]]),  lwd = 1.0) +
    
    labs(title = "Internal test set (N=5,750)") + xlab("Specificity") + ylab("Sensitivity") + 
    scale_color_manual("Models: ", values = colors_4) +
    
    annotate("text", x = 0.0, y = 0.25, hjust = 1, vjust = 1, col = colors_4[1], size = 5,
             label = paste0("Model-1: ", format(round(as.numeric(ROC_test_MGH_1[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_MGH_1[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_MGH_1[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    annotate("text", x = 0.0, y = 0.18, hjust = 1, vjust = 1, col = colors_4[2], size = 5,
             label = paste0("Model-2: ", format(round(as.numeric(ROC_test_MGH_2[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_MGH_2[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_MGH_2[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    annotate("text", x = 0.0, y = 0.11, hjust = 1, vjust = 1, col = colors_4[3], size = 5,
             label = paste0("Model-3: ", format(round(as.numeric(ROC_test_MGH_3[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_MGH_3[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_MGH_3[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    annotate("text", x = 0.0, y = 0.04, hjust = 1, vjust = 1, col = colors_4[4], size = 5,
             label = paste0("Model-DL: ", format(round(as.numeric(ROC_test_MGH_DL[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_MGH_DL[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_MGH_DL[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    
    geom_abline(slope=1, intercept = 1, linetype = "dashed", color = "grey") +
    theme_bw() + coord_fixed(ratio = 1) + #Theme
    theme(legend.position = "none",
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 8),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 12))
}


## Create ROC curves for BWH test -----
p_roc_test_BWH <- list()
for(i in 1:length(ROC_test_BWH_1)) {
  p_roc_test_BWH[[i]] <- ggroc(
    list("Model-1" = ROC_test_BWH_1[[i]],
         "Model-2" = ROC_test_BWH_2[[i]],
         "Model-3" = ROC_test_BWH_3[[i]],
         "Model-DL" = ROC_test_BWH_DL[[i]]),  lwd = 1.0) +
    
    labs(title = "External test set (N=22,764)") + xlab("Specificity") + ylab("Sensitivity") + 
    scale_color_manual("Models: ", values = colors_4) +
    
    annotate("text", x = 0.0, y = 0.25, hjust = 1, vjust = 1, col = colors_4[1], size = 5,
             label = paste0("Model-1: ", format(round(as.numeric(ROC_test_BWH_1[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_BWH_1[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_BWH_1[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    annotate("text", x = 0.0, y = 0.18, hjust = 1, vjust = 1, col = colors_4[2], size = 5,
             label = paste0("Model-2: ", format(round(as.numeric(ROC_test_BWH_2[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_BWH_2[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_BWH_2[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    annotate("text", x = 0.0, y = 0.11, hjust = 1, vjust = 1, col = colors_4[3], size = 5,
             label = paste0("Model-3: ", format(round(as.numeric(ROC_test_BWH_3[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_BWH_3[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_BWH_3[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    annotate("text", x = 0.0, y = 0.04, hjust = 1, vjust = 1, col = colors_4[4], size = 5,
             label = paste0("Model-DL: ", format(round(as.numeric(ROC_test_BWH_DL[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_BWH_DL[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_BWH_DL[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    
    geom_abline(slope=1, intercept = 1, linetype = "dashed", color = "grey") +
    theme_bw() + coord_fixed(ratio = 1) + #Theme
    theme(legend.position = "none",
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 8),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 12))
}


## Create ROC curves for BWH test following fine tuning -----
p_roc_test_BWHft <- list()
for(i in 1:length(ROC_test_BWHft_1)) {
  p_roc_test_BWHft[[i]] <- ggroc(
    list("Model-1" = ROC_test_BWHft_1[[i]],
         "Model-2" = ROC_test_BWHft_2[[i]],
         "Model-3" = ROC_test_BWHft_3[[i]],
         "Model-DL" = ROC_test_BWHft_DL[[i]]),  lwd = 1.0) +
    
    labs(title = "External test set following fine tuning (N=4,552)") + xlab("Specificity") + ylab("Sensitivity") + 
    scale_color_manual("Models: ", values = colors_4) +
    
    annotate("text", x = 0.0, y = 0.25, hjust = 1, vjust = 1, col = colors_4[1], size = 5,
             label = paste0("Model-1: ", format(round(as.numeric(ROC_test_BWHft_1[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_BWHft_1[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_BWHft_1[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    annotate("text", x = 0.0, y = 0.18, hjust = 1, vjust = 1, col = colors_4[2], size = 5,
             label = paste0("Model-2: ", format(round(as.numeric(ROC_test_BWHft_2[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_BWHft_2[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_BWHft_2[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    annotate("text", x = 0.0, y = 0.11, hjust = 1, vjust = 1, col = colors_4[3], size = 5,
             label = paste0("Model-3: ", format(round(as.numeric(ROC_test_BWHft_3[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_BWHft_3[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_BWHft_3[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    annotate("text", x = 0.0, y = 0.04, hjust = 1, vjust = 1, col = colors_4[4], size = 5,
             label = paste0("Model-DL: ", format(round(as.numeric(ROC_test_BWHft_DL[[i]]$ci)[2], 2), nsmall = 2),
                            " [", format(round(as.numeric(ROC_test_BWHft_DL[[i]]$ci)[1], 2), nsmall = 2), "; ",
                            format(round(as.numeric(ROC_test_BWHft_DL[[i]]$ci)[3], 2), nsmall = 2), "]")) + 
    
    geom_abline(slope=1, intercept = 1, linetype = "dashed", color = "grey") +
    theme_bw() + coord_fixed(ratio = 1) + #Theme
    theme(legend.position = "none",
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 8),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 12))
}
# =========


## Save specific groups -----
p_roc_test_MGH_G0 <- p_roc_test_MGH
p_roc_test_MGH_G1 <- p_roc_test_MGH
p_roc_test_MGH_G2 <- p_roc_test_MGH

p_roc_test_BWH_G0 <- p_roc_test_BWH
p_roc_test_BWH_G1 <- p_roc_test_BWH
p_roc_test_BWH_G2 <- p_roc_test_BWH

p_roc_test_BWHft_G0 <- p_roc_test_BWHft
p_roc_test_BWHft_G1 <- p_roc_test_BWHft
p_roc_test_BWHft_G2 <- p_roc_test_BWHft

# Combine plots ====
library(gridExtra); library(grid)

## Composite endpoint -----
layout <- rbind(c(1, 2, 3))

fig_1 <- arrangeGrob(top = textGrob(paste0("Diagnostic accuracy to predict composite endpoint"),
                                    gp = gpar(fontsize = 20, fontface = "bold", alpha = 1), vjust = 0.5),
                     p_roc_test_MGH[[1]], p_roc_test_BWH[[1]], p_roc_test_BWHft[[1]],
                     layout_matrix = layout)

ggsave(plot = fig_1, filename = paste0(folder_image, "Fig_1.pdf"), device = "pdf",
       dpi = 600, width = 18, height = 6, units = "in")


## Each individual endpoint -----
layout <- rbind(c(1, 2, 3),
                c(4, 5, 6),
                c(7, 8, 9),
                c(10,11,12))

fig_4 <- arrangeGrob(top = textGrob(paste0("Diagnostic accuracy to predict each individual endpoint"),
                                    gp = gpar(fontsize = 30, fontface = "bold", alpha = 1), vjust = 0.5),
                     left = textGrob(paste0("Death", paste0(rep(" ", 44), collapse = ""), "Aortic dissection", paste0(rep(" ", 38), collapse = ""),
                                            "Pulmonary embolism", paste0(rep(" ", 36), collapse = ""), "Myocardial infarction"),
                                     gp = gpar(fontsize = 22, fontface = "bold", alpha = 1), vjust = 0.5, hjust = 0.5, rot = 90),
                     p_roc_test_MGH[[4]], p_roc_test_BWH[[4]], p_roc_test_BWHft[[4]],
                     p_roc_test_MGH[[5]], p_roc_test_BWH[[5]], p_roc_test_BWHft[[5]],
                     p_roc_test_MGH[[6]], p_roc_test_BWH[[6]], p_roc_test_BWHft[[6]],
                     p_roc_test_MGH[[3]], p_roc_test_BWH[[3]], p_roc_test_BWHft[[3]],
                     layout_matrix = layout)

ggsave(plot = fig_4, filename = paste0(folder_image, "Fig_4.pdf"), device = "pdf",
       dpi = 600, width = 18, height = 24, units = "in")



## Based on median values
layout <- rbind(c(1, 2, 3),
                c(4, 5, 6))

fig_2 <- arrangeGrob(top = textGrob(paste0("Diagnostic accuracy to predict composite endpoint - stratified by sex"),
                                    gp = gpar(fontsize = 25, fontface = "bold", alpha = 1), vjust = 0.5),
                     left = textGrob(paste0("Male", paste0(rep(" ", 20), collapse = ""), "Female", paste0(rep(" ", 38), collapse = "")),
                                     gp = gpar(fontsize = 22, fontface = "bold", alpha = 1), vjust = 0.5, hjust = 0.5, rot = 90),
                     p_roc_test_MGH_G0[[1]], p_roc_test_BWH_G0[[1]], p_roc_test_BWHft_G0[[1]],
                     p_roc_test_MGH_G1[[1]], p_roc_test_BWH_G1[[1]], p_roc_test_BWHft_G1[[1]],
                     layout_matrix = layout)

ggsave(plot = fig_2, filename = paste0(folder_image, "Fig_3.pdf"), device = "pdf",
       dpi = 600, width = 18, height = 12, units = "in")


## Based on race
layout <- rbind(c(1, 2, 3),
                c(4, 5, 6),
                c(7, 8, 9))

fig_2 <- arrangeGrob(top = textGrob(paste0("Diagnostic accuracy to predict composite endpoint - stratified by race and ethnicity"),
                                    gp = gpar(fontsize = 25, fontface = "bold", alpha = 1), vjust = 0.5),
                     left = textGrob(paste0("Hispanic white", paste0(rep(" ", 20), collapse = ""), "Non-Hispanic white", paste0(rep(" ", 38), collapse = ""), "Black"),
                                     gp = gpar(fontsize = 22, fontface = "bold", alpha = 1), vjust = 0.5, hjust = 0.5, rot = 90),
                     p_roc_test_MGH_G0[[1]], p_roc_test_BWH_G0[[1]], p_roc_test_BWHft_G0[[1]],
                     p_roc_test_MGH_G1[[1]], p_roc_test_BWH_G1[[1]], p_roc_test_BWHft_G1[[1]],
                     p_roc_test_MGH_G2[[1]], p_roc_test_BWH_G2[[1]], p_roc_test_BWHft_G2[[1]],
                     layout_matrix = layout)

ggsave(plot = fig_2, filename = paste0(folder_image, "Fig_4.pdf"), device = "pdf",
       dpi = 600, width = 18, height = 18, units = "in")
# CLOSE =====
rm(list = ls()); gc(); dev.off()


