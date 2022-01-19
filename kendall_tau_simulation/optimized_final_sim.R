library(lavaan)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(pcaPP)
library(tictoc)
library(dtplyr)
library(latex2exp)
library(ggpubr)
library(dqrng)
options(dplyr.summarise.inform = FALSE)


# parameters setting ------------------------------------------------------


seed <- 123
rep <- 500
obs_n <- 400
eta_n <- 1
ind_n <- c(5, 10, 15)
lambda_value <-
  list(c(0.5, 0.3, 0.4, 0.4, 0.4),
       c(0.6, 0.4, 0.5, 0.5, 0.5),
       c(0.7, 0.5, 0.6, 0.6, 0.6))
lambda_label <- c("small", "medium", "large")
epsilon_mean <- 0
eta_mean <- 0
eta_sd <- 1
nu1_value <- 0
nu2_value <- c(0, 0.1, 0.3, 0.5)
non_inv_label <- c("none", "small", "medium", "large")
pror_non_inv_label <- c("0.2", "0.4")

source("./kendall_tau_simulation/final_function.R")


# all outcome -------------------------------------------------------------


outcome_list <- sim(
  seed = seed,
  rep = rep,
  obs_n = obs_n,
  eta_n = eta_n,
  ind_n = ind_n,
  lambda_value = lambda_value,
  nu_value = nu_value,
  epsilon_mean = epsilon_mean,
  eta_mean = eta_mean,
  eta_sd = eta_sd
)


# plot --------------------------------------------------------------------


outcome_proportion_1 <- outcome_list[[1]] %>%
  filter(Proportion == 0.2)
outcome_proportion_2 <- outcome_list[[1]] %>%
  filter(Proportion == 0.4)

labels <-
  unname(TeX(
    c(
      "$\\tau_{\\hat{\\eta}\\eta}$ (All items)",
      "$\\tau_{x \\eta}$ (All items)",
      "$\\tau_{\\hat{\\eta}\\eta}$ (Fair items)",
      "$\\tau_{x \\eta}$ (Fair items)"
    )
  ))
p1 <- ggplot(outcome_proportion_1,
             aes(Effect, mean_est,
                 group = Type)) +
  coord_cartesian(ylim = c(0.3, 0.9)) +
  geom_point(
    aes(shape = Type, colour = Type),
    position = position_dodge(width = .7),
    size = 1,
    show.legend = c(colour = TRUE)
  ) +
  scale_shape_manual(values = c(17, 15, 4, 16), labels = labels) +
  scale_colour_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#E69F00"),
                      labels = labels) +
  scale_x_discrete(labels = c("None", "Small", "Medium", "Large")) +
  geom_linerange(
    aes(
      colour = Type,
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = .7),
    size = .5,
    show.legend = c(colour = FALSE)
  ) +
  labs(
    x = "Effect",
    y = "Parameter Estimate",
    title = TeX(r"(Kendall's $\tau$ between groups)", bold = TRUE),
    subtitle = "Proportion = 0.2"
  ) +
  scale_y_continuous(breaks = c(0.4, 0.6, 0.8)) +
  facet_grid(
    Loading ~ Indicator,
    labeller = labeller(
      .cols = label_both,
      Loading = as_labeller(c(
        `small` = "Small",
        `medium` = "Medium",
        `large` = "Large"
      ),default = label_both)
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 35),
    plot.subtitle = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    strip.background = element_rect(fill = "white", color = "white", size = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(angle = -90, size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

p2 <- ggplot(outcome_proportion_2,
             aes(Effect, mean_est,
                 group = Type)) +
  coord_cartesian(ylim = c(0.3, 0.9)) +
  geom_point(
    aes(shape = Type, colour = Type),
    position = position_dodge(width = .7),
    size = 1,
    show.legend = c(colour = TRUE)
  ) +
  scale_shape_manual(values = c(17, 15, 4, 16), labels = labels) +
  scale_colour_manual(values = c("#F8766D", "#619CFF", "#00BA38", "#E69F00"),
                      labels = labels) +
  scale_x_discrete(labels = c("None", "Small", "Medium", "Large")) +
  geom_linerange(
    aes(
      colour = Type,
      ymin = lower,
      ymax = upper
    ),
    position = position_dodge(width = .7),
    size = .5,
    show.legend = c(colour = FALSE)
  ) +
  labs(x = "Effect", y = "Parameter Estimate",  subtitle = "Proportion = 0.4") +
  scale_y_continuous(breaks = c(0.4, 0.6, 0.8)) +
  facet_grid(
    Loading ~ Indicator,
    labeller = labeller(
      .cols = label_both,
      Loading = as_labeller(c(
        `small` = "Small",
        `medium` = "Medium",
        `large` = "Large"
      ),default = label_both)
    )
  ) +
  theme_bw() +
  theme(
    plot.subtitle = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    strip.background = element_rect(fill = "white", color = "white", size = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(angle = -90, size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )


final_plot <- ggarrange(
  p1 + rremove("xlab") + rremove("ylab"),
  p2 +  rremove("ylab"),
  nrow = 2,
  common.legend = TRUE,
  legend = "bottom"
) %>%
  annotate_figure(left = text_grob("Parameter Estimate",
                                   size = 15,
                                   rot = 90))

ggsave(
  final_plot,
  filename = paste0("simulation_outcome_plot_rep", rep, "_obns", obs_n, ".png"),
  path = "./outcome/plot",
  width = 21,
  height = 27,
  units = "cm",
  device = "png",
  dpi = 400
)


save.image(file=paste0("./outcome/env/env_rep", rep,"_obsn", obs_n,".RData"))