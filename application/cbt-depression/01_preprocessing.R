rm(list = ls())

library(tidyr)
library(dplyr)

raw <- readxl::read_excel("application/cbt-depression/data-depression-psyctr-2022.xlsx")

dat <- raw |>
  mutate(
    yi = .g,
    vi = .g_se^2,
    id = row_number()
  ) |>
  filter(!is.na(yi)) |>
  group_by(study) |>
  mutate(study_id = cur_group_id()) |>
  ungroup() |>
  mutate(
    n_exp = as.numeric(ifelse(is.na(n_arm1), totaln_arm1, n_arm1)),
    n_ctrl = as.numeric(ifelse(is.na(n_arm2), totaln_arm2, n_arm2)),
    percent_women = ifelse(percent_women <= 1, percent_women * 100, percent_women),
    condition = case_when(
      condition_arm1 == "cbt" ~ "CBT",
      condition_arm1 == "dyn" ~ "Psychodynamic",
      condition_arm1 == "ipt" ~ "Interpersonal",
      condition_arm1 == "pst" ~ "Problem-Solving Therapy",
      condition_arm1 == "3rd" ~ "Third-Wave CBT",
      condition_arm1 == "bat" ~ "Behavioral Activation",
      condition_arm1 == "lrt" ~ "Life Review Therapy",
      condition_arm1 == "sup" ~ "Supportive Therapy",
      condition_arm1 == "other psy" ~ "Other Psychotherapy",
      TRUE ~ condition_arm1
    ),
    control_condition = condition_arm2,
    control_label = case_when(
      condition_arm2 == "wl" ~ "Waitlist",
      condition_arm2 == "cau" ~ "Care as usual",
      condition_arm2 == "other ctr" ~ "Other control",
      TRUE ~ as.character(condition_arm2)
    ),
    format = case_when(
      format == "ind" ~ "Individual",
      format == "grp" ~ "Group",
      format == "gsh" ~ "Guided self-help",
      format %in% c("ush", "tel", "oth", "cpl") ~ "Other formats",
      TRUE ~ format
    ),
    diagnosis = case_when(
      diagnosis %in% c("mdd", "mood", "chr") ~ "Diagnosis",
      diagnosis == "cut" ~ "Cut-off score",
      diagnosis == "sub" ~ "Subclinical depression",
      TRUE ~ diagnosis
    ),
    target_group = case_when(
      target_group == "4 & 5" & study == "Zhao, 2019" ~ "Perinatal depression",
      target_group %in% c("adul", "yadul") ~ "Adults",
      target_group == "old" ~ "Older adults",
      target_group == "stud" ~ "Student population",
      target_group == "ppd" ~ "Perinatal depression",
      target_group == "med" ~ "General medical",
      target_group == "oth" ~ "Other target groups",
      TRUE ~ target_group
    ),
    region = case_when(
      country %in% c("us", "can") ~ "North America",
      country %in% c("uk", "eu") ~ "Europe",
      country == "au" ~ "Australia",
      country == "eas" ~ "East Asia",
      country == "oth" ~ "Other Region",
      TRUE ~ country
    ),
    rob = case_when(
      rob == 4 ~ "Low",
      rob == 0 ~ "High",
      rob %in% c(1, 2, 3) ~ "Some concern",
      TRUE ~ NA_character_
    )
  ) |>
  select(
    study_id,
    id,
    author_year = study,
    year,
    condition,
    control_condition,
    control_label,
    format,
    region,
    diagnosis,
    target_group,
    mean_age,
    percent_women,
    n_exp,
    n_ctrl,
    instrument,
    rating,
    outcome_type,
    rob,
    yi,
    vi
  )

saveRDS(dat, "application/cbt-depression/cbt-dep-clean.rds")
