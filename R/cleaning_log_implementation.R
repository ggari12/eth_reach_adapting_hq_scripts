# Load required libraries
library(tidyverse)
library(cleaningtools)
library(openxlsx)
library(readxl)
library(writexl)

# -------------------------------
# Load Data & Tools
# -------------------------------
df_raw_data <- cleaningtools::cleaningtools_raw_data
df_survey <- cleaningtools::cleaningtools_survey
df_choices <- cleaningtools::cleaningtools_choices

# -------------------------------
# Perform Initial Checks
# -------------------------------
checks_logs <- df_raw_data |>
    check_duplicate(uuid_column = "X_uuid",
                    log_name = "duplicate_log") |>
    check_soft_duplicates(kobo_survey = df_survey,
                          uuid_column = "X_uuid",
                          idnk_value = "dont_know",
                          sm_separator = ".",
                          log_name = "soft_duplicate_log",
                          threshold = 7,
                          return_all_results = FALSE) |>
    check_pii(uuid_column = "X_uuid",
              words_to_look = NULL) |>
    check_outliers(uuid_column = "X_uuid",
                   sm_separator = ".",
                   strongness_factor = 3) |>
    check_value(uuid_column = "X_uuid",
                values_to_look = c(666, 99, 999, 9999, 98, 88, 888, 8888))

# -------------------------------
# Add Duration & Missing Data
# -------------------------------
checks_logs$checked_dataset <- checks_logs$checked_dataset |>
  add_duration(uuid_column = "X_uuid", start_column = "X.U.FEFF.start", end_column = "end") |>
  add_percentage_missing(kobo_survey = df_survey)

# Load logical checks list
logical_list <- read_excel("inputs/01 - example - check_list.xlsx")

# Identify "Others specify" fields
other_specify <- df_survey |> 
    filter(type == "text", name %in% names(df_raw_data)) |> 
    pull(name)

# -------------------------------
# Additional Checks
# -------------------------------
checks_logs <- checks_logs |>
  check_duration(column_to_check = "duration",
                 uuid_column = "X_uuid",
                 log_name = "duration_log",
                 lower_bound = 20,
                 higher_bound = 120) |>
  check_percentage_missing(uuid_column = "X_uuid",
                           column_to_check = "percentage_missing",
                           strongness_factor = 2,
                           log_name = "percentage_missing_log") |>
  check_others(uuid_column = "X_uuid", 
               columns_to_check = other_specify) |>
  check_logical_with_list(uuid_column = "X_uuid",
                          list_of_check = logical_list,
                          check_id_column = "check_id",
                          check_to_perform_column = "check_to_perform",
                          columns_to_clean_column = "columns_to_clean",
                          description_column = "description",
                          bind_checks = TRUE)

# -------------------------------
# Create Cleaning Log
# -------------------------------
cleaning_log <- checks_logs |>
  create_combined_log() |>
  add_info_to_cleaning_log(dataset_uuid_column = "X_uuid",
                           information_to_add = c("enumerator_num", "date_assessment"))

cleaning_log |>
  create_xlsx_cleaning_log(cleaning_log_name = "cleaning_log",
                           change_type_col = "change_type",
                           column_for_color = "check_binding",
                           kobo_survey = df_survey,
                           kobo_choices = df_choices,
                           use_dropdown = TRUE,
                           sm_dropdown_type = "logical",
                           output_path = paste0("outputs/", 
                                                "combined_cleaning_log.xlsx"))

openXL(file = paste0("outputs/", "combined_cleaning_log.xlsx"))

# -------------------------------
# Process Filled Cleaning Log
# -------------------------------
filled_cleaning_log <- read_excel("inputs/combined_cleaning_log_filled.xlsx", sheet = "cleaning_log") |> 
  filter(!question %in% c("X_index"), !uuid %in% c("all"))

# -------------------------------
# Apply Cleaning to Dataset
# -------------------------------
clean_data <- create_clean_data(raw_dataset = df_raw_data,
                                raw_data_uuid_column = "X_uuid",
                                cleaning_log = filled_cleaning_log,
                                cleaning_log_question_column = "question",
                                cleaning_log_new_value_column = "new_value",
                                cleaning_log_change_type_column = "change_type",
                                change_response_value = "change_response",
                                NA_response_value = "blank_response",
                                no_change_value = "no_action",
                                remove_survey_value = "remove_survey")

# Correct select multiple parents
clean_data <- recreate_parent_column(dataset = clean_data,
                                     uuid_column = "X_uuid",
                                     kobo_survey = df_survey,
                                     kobo_choices = df_choices,
                                     sm_separator = ".",
                                     cleaning_log_to_append = filled_cleaning_log)

# -------------------------------
# Review Cleaning & Others
# -------------------------------
others_review <- review_others(clean_data$data_with_fix_concat,
                               uuid_column = "X_uuid",
                               kobo_survey = df_survey,
                               sm_separator = ".")

only_deletion <- clean_data$cleaning_log |> 
  filter(change_type == "remove_survey")

only_cleaning <- clean_data$cleaning_log |> 
  filter(change_type != "remove_survey",
         !uuid %in% only_deletion$uuid)

# reviewing cleaning ------------------------------------------------------
cleaning_review <- review_cleaning(raw_dataset = df_raw_data,
                                   raw_dataset_uuid_column = "X_uuid",
                                   clean_dataset = clean_data$data_with_fix_concat,
                                   clean_dataset_uuid_column = "X_uuid",
                                   cleaning_log = only_cleaning,
                                   cleaning_log_change_type_column = "change_type",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_old_value_column = "old_value",
                                   cleaning_log_no_change_value = "no_action",
                                   deletion_log = only_deletion,
                                   deletion_log_uuid_column = "uuid")

# -------------------------------
# Export Clean Dataset to Excel
# -------------------------------
clean_info_list <- list(dataset = clean_data$data_with_fix_concat,
                        cleaning_log = only_cleaning,
                        deletion_log = only_deletion)

write_xlsx(x = clean_info_list, "outputs/df_clean_data.xlsx")
