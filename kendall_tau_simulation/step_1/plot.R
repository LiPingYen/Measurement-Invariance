library(readr)
library(plotly)
library(dplyr)
library(forcats)

cor_outcome <-
  read_csv("./new_simulation/step1_correlation_outcome.csv",
           col_names = TRUE)

cor_outcome$condition <- fct_inorder(cor_outcome$condition)

fig <-
  plot_ly(
    cor_outcome,
    x = ~ condition,
    y = ~ predict_true,
    name = 'predict factor and true factor',
    type = 'scatter',
    mode = 'lines'
  ) %>%
  add_trace(
    cor_outcome,
    x = ~ condition,
    y = ~ predict_sum,
    name = 'predict factor and sum score',
    type = 'scatter',
    mode = 'lines'
  ) %>%
  add_trace(
    cor_outcome,
    x = ~ condition,
    y = ~ true_sum,
    name = 'true factor and sum score',
    type = 'scatter',
    mode = 'lines'
  ) %>% 
  layout(title = "Correlation between predict factor score, true factor score and sum score",
        xaxis = list(title = "condition"),
        yaxis = list (title = "correlation coefficient"))
fig
