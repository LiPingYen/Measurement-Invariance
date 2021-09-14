library(tidyverse)


# import environment data -------------------------------------------------

load("./outcome/env_rep500_obsn800.RData")


# analysis ----------------------------------------------------------------

# tau value in raw data
# invariance
# predict_true
outcome_list[[1]] |>
  select(-c("lower", "upper")) |>
  pivot_wider(names_from = Type,
              values_from = mean_est,
              names_sep = "_") |>
  slice_max(n = 1, order_by = `predict true between`) |> View()

outcome_list[[1]] |>
  select(-c("lower", "upper")) |>
  pivot_wider(names_from = Type,
              values_from = mean_est,
              names_sep = "_") |>
  slice_min(n = 1, order_by = `predict true between`) |> View()

# true_sum
outcome_list[[1]] |>
  select(-c("lower", "upper")) |>
  pivot_wider(names_from = Type,
              values_from = mean_est,
              names_sep = "_") |>
  slice_max(n = 1, order_by = `true sum between`) |> View()

outcome_list[[1]] |>
  select(-c("lower", "upper")) |>
  pivot_wider(names_from = Type,
              values_from = mean_est,
              names_sep = "_") |>
  slice_min(n = 1, order_by = `true sum between`) |> View()

# calculate difference between removing noninvariance or not


dta <- outcome_list[[1]] |>
  select(-c("lower", "upper")) |>
  pivot_wider(names_from = Type,
              values_from = mean_est,
              names_sep = "_") |>
  mutate(
    diff_predict_true = (`predict true between` - `predict true between invariance`) |> round(5),
    diff_true_sum = (`true sum between` - `true sum between invariance`) |> round(5)
  ) |>
  select(-c(5, 6, 7, 8))

# invariance
# diff_predict_true range

dta |>
  filter(Effect == "none") |>
  slice_max(n = 1, order_by = diff_predict_true) |> View()

dta |>
  filter(Effect == "none") |>
  slice_min(n = 1, order_by = diff_predict_true) |> View()

# diff_true_sum range

dta |>
  filter(Effect == "none") |>
  slice_max(n = 1, order_by = diff_true_sum) |> View()

dta |>
  filter(Effect == "none") |>
  slice_min(n = 1, order_by = diff_true_sum) |> View()


# noninvariance
# diff_predict_true range

dta |>
  filter(Effect != "none") |>
  slice_max(n = 1, order_by = diff_predict_true) |> View()

dta |>
  filter(Effect != "none") |>
  slice_min(n = 1, order_by = diff_predict_true) |> View()

# diff_true_sum range

dta |>
  filter(Effect != "none") |>
  slice_max(n = 1, order_by = diff_true_sum) |> View()

dta |>
  filter(Effect != "none") |>
  slice_min(n = 1, order_by = diff_true_sum) |> View()
