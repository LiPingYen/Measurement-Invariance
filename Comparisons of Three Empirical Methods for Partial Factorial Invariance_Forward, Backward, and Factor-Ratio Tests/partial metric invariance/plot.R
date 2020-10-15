
# load package ------------------------------------------------------------

library(ggplot2)
library(plotly)
library(dplyr)

# data wrangling ----------------------------------------------------------

dta <- read.csv("result.csv", header = T)
dta$condition <-
  factor(
    dta$condition,
    levels = c(
      "small_difference",
      "large_difference",
      "mixed_difference",
      "nonuniform_difference"
    ),
    labels = c(
      "small_difference",
      "large_difference",
      "mixed_difference",
      "nonuniform_difference"
    )
  )

dta$sample_size <-
  factor(
    dta$sample_size,
    levels = c("250", "500", "1000"),
    labels = c("250", "500", "1000")
  )

dta$confidence_intervel <-
  factor(
    dta$confidence_intervel,
    levels = c("0.95", "0.99"),
    labels = c("0.95", "0.99")
  )


#  plot -------------------------------------------------------
#perfect rate
p1 <- dta %>% ggplot(aes(x = condition,
                         y = perfect_rate,
                         group = sample_size)) +
  geom_point(aes(shape = sample_size),
             size = 2,
             fill = "red") +
  geom_line(aes(linetype = sample_size)) +
  facet_grid(rows  = vars(confidence_intervel))
ggplotly(p1)

#type I error rate
p2 <- dta %>% ggplot(aes(x = condition,
                         y = typei_rate,
                         group = sample_size)) +
  geom_point(aes(shape = sample_size),
             size = 2,
             fill = "red") +
  geom_line(aes(linetype = sample_size)) +
  facet_grid(rows  = vars(confidence_intervel))
ggplotly(p2)

#type II error rate
p3 <- dta %>% ggplot(aes(x = condition,
                         y = typeii_rate,
                         group = sample_size)) +
  geom_point(aes(shape = sample_size),
             size = 2,
             fill = "red") +
  geom_line(aes(linetype = sample_size)) +
  facet_grid(rows  = vars(confidence_intervel))
ggplotly(p3)
