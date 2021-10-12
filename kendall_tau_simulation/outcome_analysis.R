library(tidyverse)


# import environment data -------------------------------------------------

load("./outcome/env/env_rep500_obsn1200.RData")


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


# difference between noninvariance and invariance -------------------------


dta2 <- outcome_list[[1]] |>
  select(-c("lower", "upper")) |>
  filter(Type != "predict true between invariance" &
           Type != "true sum between invariance") |>
  pivot_wider(
    names_from = c(Type, Effect),
    values_from = mean_est,
    names_sep = "_"
  )

# calculate tau between noninvariance and invariance 
# among all situations without removing any items.


dta2 |>
  rowwise(Indicator, Loading, Proportion) |>
  mutate(
    predict_true_between_noninv_min = min(
      c(
        `predict true between_small`,
        `predict true between_medium`,
        `predict true between_large`
      )
    ),
    true_sum_between_noninv_min = min(
      c(
        `true sum between_small`,
        `true sum between_medium`,
        `true sum between_large`
      )
    )
  ) |> group_by(Indicator,Loading,Proportion) |> 
  mutate(diff_inv_noninvmin_predict_true = `predict true between_none` - predict_true_between_noninv_min,
            diff_inv_noninvmin_true_sum = `true sum between_none` - true_sum_between_noninv_min) |> 
  arrange(desc(diff_inv_noninvmin_predict_true)) |> View()

