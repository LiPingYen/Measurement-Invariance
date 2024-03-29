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
      "baseline",
      "small_difference",
      "large_difference",
      "mixed_difference",
      "nonuniform_difference"
    ),
    labels = c(
      "baseline",
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
  facet_grid(rows  = vars(confidence_intervel)) +
  ylim(0, 1)
ggplotly(p1)

#type I error rate
p2 <- dta %>% ggplot(aes(x = condition,
                         y = typei_rate,
                         group = sample_size)) +
  geom_point(aes(shape = sample_size),
             size = 2,
             fill = "red") +
  geom_line(aes(linetype = sample_size)) +
  facet_grid(rows  = vars(confidence_intervel)) +
  ylim(0, 1)
ggplotly(p2)

#type II error rate
p3 <- dta %>% ggplot(aes(x = condition,
                         y = typeii_rate,
                         group = sample_size)) +
  geom_point(aes(shape = sample_size),
             size = 2,
             fill = "red") +
  geom_line(aes(linetype = sample_size)) +
  facet_grid(rows  = vars(confidence_intervel)) +
  ylim(0, 1)
ggplotly(p3)

#difference from paper in perfect recovery rate
p4 <- dta %>% ggplot(aes(x = condition,
                         y = d_perfect_rate,
                         group = sample_size)) +
  geom_point(aes(shape = sample_size),
             size = 2,
             fill = "red") +
  geom_line(aes(linetype = sample_size)) +
  facet_grid(rows  = vars(confidence_intervel)) +
  ylim(-0.2, 0.2)
ggplotly(p4)

#difference from paper in type I error rate
p5 <- dta %>% ggplot(aes(x = condition,
                         y = d_typei_rate,
                         group = sample_size)) +
  geom_point(aes(shape = sample_size),
             size = 2,
             fill = "red") +
  geom_line(aes(linetype = sample_size)) +
  facet_grid(rows  = vars(confidence_intervel)) +
  ylim(-0.1, 0.1)
ggplotly(p5)

#difference from paper in type II error rate
p6 <- dta %>% ggplot(aes(x = condition,
                         y = d_typeii_rate,
                         group = sample_size)) +
  geom_point(aes(shape = sample_size),
             size = 2,
             fill = "red") +
  geom_line(aes(linetype = sample_size)) +
  facet_grid(rows  = vars(confidence_intervel)) +
  ylim(-0.2, 0.2)
ggplotly(p6)