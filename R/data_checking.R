# Load required libraries
rm(list = ls())

library(tidyverse)
library(cleaningtools)

# read data ---------------------------------------------------------------
df_raw_data <- cleaningtools::cleaningtools_raw_data

# tool
df_survey <- cleaningtools::cleaningtools_survey
df_choices <- cleaningtools::cleaningtools_choices

# check pii ---------------------------------------------------------------
pii_cols <- c("telephone", "contact", "name", "gps", "latitude", "longitude", "contact", "geopoint")
pii_from_data <- cleaningtools::check_pii(dataset = df_raw_data, 
                                          element_name = "checked_dataset", 
                                          uuid_column = "X_uuid")
pii_from_data$potential_PII

# duration ----------------------------------------------------------------
# add duration from audit
df_raw_data_with_audit_time <-df_raw_data  %>% 
  cleaningtools::add_duration(uuid_column = "X_uuid", start_column = "X.U.FEFF.start", end_column = "end")

# check duration
df_check_duration <- cleaningtools::check_duration(dataset = df_raw_data_with_audit_time, 
                                                   column_to_check = "duration",
                                                   uuid_column = "X_uuid",
                                                   log_name = "duration_log",
                                                   lower_bound = 20,
                                                   higher_bound = 120)

df_check_duration$duration_log %>% View()

# combine cleaning tools checks
list_log <- df_check_duration
  
# outliers ----------------------------------------------------------------
outlier_cols_not_4_checking <- df_raw_data %>% 
  select(matches("geopoint|gps|_index|_submit|submission|_sample_|^_id$")) %>% 
  colnames()

df_list_check_outliers <- cleaningtools::check_outliers(dataset = df_raw_data, 
                                                        uuid_column = "X_uuid",
                                                        sm_separator = ".",
                                                        strongness_factor = 3,
                                                        columns_not_to_check = outlier_cols_not_4_checking) 

df_list_check_outliers$potential_outliers %>% View()

# add other checks to the list
list_log$outliers <- df_list_check_outliers$potential_outliers

# Check for value ---------------------------------------------------------
df_list_check_value <- cleaningtools::check_value(dataset = df_raw_data, 
                                                  uuid_column = "X_uuid", 
                                                  element_name = "checked_dataset", 
                                                  values_to_look = c(666, 99, 999, 9999, 98, 88, 888, 8888))

df_list_check_value$flaged_value %>% View()

df_potential_check_value <- df_list_check_value$flaged_value
df_check_value <- df_potential_check_value %>% 
  filter(!str_detect(string = question, pattern = "geopoint|gps|_index|uuid|_submit|submission|_sample_|^_id$|\\/"))

# add other checks to the list
list_log$potential_outliers <- df_check_value

# check logics ------------------------------------------------------------
df_logical_check_list <- readxl::read_excel("inputs/01 - example - check_list.xlsx") 

df_logical_check <- cleaningtools::check_logical_with_list(dataset = df_raw_data,
                                                           uuid_column = "X_uuid",
                                                           list_of_check = df_logical_check_list,
                                                           check_id_column = "check_id",
                                                           check_to_perform_column = "check_to_perform",
                                                           columns_to_clean_column = "columns_to_clean",
                                                           description_column = "description",
                                                           bind_checks = TRUE)
df_logical_check$logical_all %>% View()

# add other checks to the list
list_log$logical_check <- df_logical_check$logical_all

# check for duplicates ----------------------------------------------------
# With the gower distance (soft duplicates)
df_soft_duplicates <- cleaningtools::check_soft_duplicates(dataset = df_raw_data,
                                                           kobo_survey = df_survey,
                                                           uuid_column = "X_uuid",
                                                           idnk_value = "dont_know",
                                                           sm_separator = ".",
                                                           log_name = "soft_duplicate_log",
                                                           threshold = 7,
                                                           return_all_results = FALSE)

df_soft_duplicates$soft_duplicate_log %>% View()

# add other checks to the list
list_log$soft_duplicate <- df_soft_duplicates$soft_duplicate_log

# check others values -----------------------------------------------------
df_other_checks <- cleaningtools::check_others(dataset = df_raw_data,
                                               uuid_column = "X_uuid",
                                               columns_to_check = names(df_raw_data %>%
                                                                            dplyr::select(ends_with("_other")) %>%
                                                                            dplyr::select(-contains(".")))
                                               )

df_other_checks$other_log %>% view()

# add other checks to the list
list_log$other_log <- df_other_checks$other_log

# create combined log -----------------------------------------------------
#Combined cleaning log
df_combined_log <- create_combined_log(list_of_log = list_log, dataset_name = "checked_dataset")

df_combined_log$cleaning_log %>% view()

# prep data
cols_to_add_to_log <- c("enumerator_num", "date_assessment")

# Exporting the flags in excel --------------------------------------------
# create_combined_log()

# # add_info_to_cleaning_log()
add_with_info <- add_info_to_cleaning_log(list_of_log = df_combined_log,
                                          dataset = "checked_dataset",
                                          cleaning_log = "cleaning_log",
                                          dataset_uuid_column = "X_uuid",
                                          cleaning_log_uuid_column = "uuid",
                                          information_to_add = cols_to_add_to_log) 
 
# # create_xlsx_cleaning_log()
add_with_info %>%
    create_xlsx_cleaning_log(cleaning_log_name = "cleaning_log",
                           change_type_col = "change_type",
                           column_for_color = "check_binding",
                           kobo_survey = df_survey,
                           kobo_choices = df_choices,
                           use_dropdown = TRUE,
                           sm_dropdown_type = "logical",
                           output_path = paste0("outputs/", butteR::date_file_prefix(), 
                                                "_combined_checks_testing.xlsx"))
 