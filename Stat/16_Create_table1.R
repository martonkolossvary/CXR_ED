######README#####
# CREATE Table 1

# Initialize -----
library(parseRPDR); library(data.table); library("gtsummary")
theme_gtsummary_journal(journal = "nejm")
set.seed(42)

folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/CXR_ED/"
folder_cache <- paste0(folder_wd, "CACHE/")
folder_codes <- paste0(folder_wd, "CODES/")
folder_image <- paste0(folder_wd, "IMAGES/")
folder_pred  <- paste0(folder_wd, "MODELS/")
folder_stat  <- paste0(folder_wd, "STAT/")
nThread      <- 10

valid_col          <- "Tuning"
valid_col_finetune <- "Tuning_BWH"
valid_col_all      <- "Tuning_ALL"
hosp      <- "enc_hosp"

# Load data and modify datasets ----
load(paste0(folder_cache, "FINAL_ALL.RData"))

d_final$race <- factor(d_final$race, levels = c("African American", "American Indian", "Asian", "Asian American Indian", "Asian Hawaiian", "Asian Pacific Islander",
                                                "Black", "European", "Hawaiian", "Hispanic", "Hispanic Black", "Hispanic White", "Not Recorded", "White", "<NA>"),
                       labels = c("Black", "Non-Hispanic white", "Non-Hispanic white", "Non-Hispanic white", "Non-Hispanic white", "Non-Hispanic white",
                                  "Black", "Non-Hispanic white", "Non-Hispanic white", "Hispanic white", "Black", "Hispanic white", NA, "Non-Hispanic white", NA))
d_final$time_between_rdt_study_hours <- as.numeric(d_final$time_between_rdt_study_hours)

# Create datasets ----
d_train <- d_final[get(valid_col) == 0 & !is.na(get(valid_col)), ]
d_tune  <- d_final[get(valid_col) == 1 & !is.na(get(valid_col)), ]
d_test_MGH <- d_final[is.na(get(valid_col)) & get(hosp) == "MGH", ]
d_test_BWH <- d_final[is.na(get(valid_col)) & get(hosp) == "BWH", ]

d_int <- data.table::rbindlist(list(d_train, d_tune, d_test_MGH))
d_int[ , (valid_col) := lapply(.SD, nafill, fill=2), .SDcols = valid_col]

d_train_ft <- d_final[get(valid_col_finetune) == 0 & !is.na(get(valid_col_finetune)), ]
d_tune_ft  <- d_final[get(valid_col_finetune) == 1 & !is.na(get(valid_col_finetune)), ]
d_test_BWH_ft <- d_final[is.na(get(valid_col_finetune)) & get(hosp) == "BWH", ]

d_ext <- data.table::rbindlist(list(d_train_ft, d_tune_ft, d_test_BWH_ft))
d_ext[ , (valid_col_finetune) := lapply(.SD, nafill, fill=2), .SDcols = valid_col_finetune]

# Create table 1 ----
## Variables =====
v_cli       <- c("time_age_at_ED", "gender", "race",
                 "time_between_rdt_study_hours", "ViewPosition",
                 "BIOMARKER_POS", "TROPONIN_POS", "DDIMER_POS",
                 "ANY_ENDPOINT_30days", "MI_30days", "PE_30days", "AD_30days", "DEATH_30days")
v_cli_names <- c("Age [y]", "Natal sex", "Race",
                 "Time between ED admission and CXR [h]", "CXR view position",
                 "Biomarker positivity", "Troponin positivity", "D-dimer positivity",
                 "Composite endpoint within 30 days", "Myocardial infarction within 30 days",
                 "Pulmonary embolism within 30 days", "Aortic dissection within 30 days",
                 "All-cause mortality within 30 days")
p_compare <- valid_col_finetune  #valid_col / valid_col_finetune


## Create table =====
d_table1 <- d_ext[, c(v_cli, p_compare, hosp), with = FALSE] #d_final / d_int / d_ext
  
  
t1_all <- tbl_summary(d_table1, by = p_compare, statistic = list(all_continuous() ~ "{mean} ± {sd}",                 #by = hosp / by = p_compare
                                                                          all_categorical() ~ "{n} ({p}%)"),
                      digits = list(all_continuous() ~ 1),
                      include = any_of(v_cli), sort = list(everything() ~ "alphanumeric"), missing = "ifany") %>%
  add_overall() %>%
  add_p(list(all_continuous() ~ "aov", all_categorical() ~ "chisq.test"),   #"t.test" / "aov"
        pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  modify_caption("**Table 1. Patient Characteristics**") %>%
  modify_header(label ~ "**Variable**")

t1_all$table_body$label[t1_all$table_body$label %in% v_cli] <- v_cli_names

## Save table =====
flextable::save_as_docx(as_flex_table(t1_all), path = paste0(folder_stat, "Table_1_BWH.docx"))

