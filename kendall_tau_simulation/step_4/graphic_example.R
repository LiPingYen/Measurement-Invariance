library(lslx)
library(sem)
library(lavaan)
library(ggplot2)
#EXAMPLE 1A
data(HS.data)

model_fa <- "spatial :=> visual + cubes + paper + flags
verbal :=> general + paragrap + sentence + wordc + wordm 
speed :=>  addition + code + counting + straight
memory :=> wordr + numberr + figurer + object + numberf + figurew
spatial <=> 1 * spatial
verbal <=> 1 * verbal
speed <=> 1 * speed
memory <=> 1 * memory"

lslx_fa <- lslx$new(model = model_fa, data = as.data.frame(scale(HS.data[,7:32])))
lslx_fa$penalize_block(block = "y<-f", type = "fixed")
lslx_fa$fit_lasso(lambda_grid = exp(seq(log(0.01), log(1), length.out = 100)))

lslx_fa$summarize(selector = "aic",
                  debias = "one_step",
                  inference = "naive")

lslx_fa$summarize(selector = "aic",
                  debias = "one_step",
                  inference = "polyhedral")


test_none <- 
  lslx_fa$test_coefficient(selector = "aic",
                           debias = "one_step",
                           inference = "naive")[20:95, c("estimate", "lower", "upper")]
test_none$type <- lslx_fa$extract_specification()$type[20:95]
test_none$left <- sapply(strsplit(rownames(test_none), split = c("<-|/")),
                         FUN = function(x) x[1])
test_none$factor <- sapply(strsplit(rownames(test_none), split = c("<-|/")),
                           FUN = function(x) x[2])
test_none$method <- "naive"



test_ph <- 
  lslx_fa$test_coefficient(selector = "aic",
                           debias = "one_step",
                           inference = "polyhedral")[20:95, c("estimate", "lower", "upper")]
test_ph$type <- lslx_fa$extract_specification()$type[20:95]
test_ph$left <- sapply(strsplit(rownames(test_ph), split = c("<-|/")),
                       FUN = function(x) x[1])
test_ph$factor <- sapply(strsplit(rownames(test_ph), split = c("<-|/")),
                         FUN = function(x) x[2])

test_ph$method <- "polyhedral"

test_scheffe <- 
  lslx_fa$test_coefficient(selector = "aic",
                           debias = "one_step",
                           inference = "scheffe")[20:95, c("estimate", "lower", "upper")]
test_scheffe$type <- lslx_fa$extract_specification()$type[20:95]
test_scheffe$left <- sapply(strsplit(rownames(test_scheffe), split = c("<-|/")),
                            FUN = function(x) x[1])
test_scheffe$factor <- sapply(strsplit(rownames(test_scheffe), split = c("<-|/")),
                              FUN = function(x) x[2])

test_scheffe$method <- "PoSI"


set.seed(9487)
idc_selected <- sample(c(T, F), dim(HS.data)[1], replace = T)
lslx_fa_cv <- lslx$new(model = model_fa, data = as.data.frame(scale(HS.data[idc_selected, 7:25])))
lslx_fa_cv$penalize_block(block = "y<-f", type = "fixed")
lslx_fa_cv$fit_lasso(lambda_grid = exp(seq(log(0.01), log(1), length.out = 100)))

lslx_fa_cv$summarize(selector = "aic",
                     debias = "one_step",
                     inference = "naive")

model_fa_cv <- "spatial :=> visual + cubes + paper + flags + sentence + addition + straight + figurer + object + numberf
verbal :=> visual + cubes + wordm + general + paragrap + sentence + wordc + code + straight + numberr + figurew
speed :=> paper + paragrap + counting + addition + code + straight + object + numberf
memory :=> flags + sentence + wordc + wordr + numberr + figurer + object + numberf + figurew
spatial <=> 1 * spatial
verbal <=> 1 * verbal
speed <=> 1 * speed
memory <=> 1 * memory"

lslx_fa_cv <- lslx$new(model = model_fa_cv, data = scale(HS.data[!idc_selected, 7:25]))
lslx_fa_cv$penalize_block(block = "y<-f", type = "fixed")
lslx_fa_cv$fit_lasso(lambda_grid = 100)
lslx_fa_cv$summarize(selector = "aic", debias = "one_step", inference = "naive")

test_ds <- 
  lslx_fa_cv$test_coefficient(selector = "aic",
                              debias = "one_step",
                              inference = "naive")[20:95, c("estimate", "lower", "upper")]
test_ds$type <- lslx_fa$extract_specification()$type[20:95]
test_ds$left <- sapply(strsplit(rownames(test_ds), split = c("<-|/")),
                       FUN = function(x) x[1])
test_ds$factor <- sapply(strsplit(rownames(test_ds), split = c("<-|/")),
                         FUN = function(x) x[2])

test_ds$method <- "data splitting"



test_all <- rbind(test_none, test_ph, test_scheffe, test_ds)
test_all$factor <- factor(test_all$factor, levels = c("spatial", "verbal", "speed", "memory"),
                          labels = c("spatial (f1)", "verbal (f2)", "speed (f3)", "memory (f4)"))
test_all$method <-factor(test_all$method, levels = c("naive","data splitting", "PoSI", "polyhedral"),
                         labels = c("naive", "DS", "PoSI", "PH"))
rownames(test_all) <- NULL

var_name <-
  c("visual perception ($y_1$)", "cubes ($y_2$)", "paper form board ($y_3$)", "flags ($y_4$)", 
    "general information ($y_5$)", "paragraph comprehension ($y_6$)", "sentence completion ($y_7$)", 
    "word classification ($y_8$)", "word meaning ($y_9$)", "addition ($y_{10}$)", 
    "code ($y_{11}$)", "counting groups of dots ($y_{12}$)", "straight//curved capitals ($y_{13}$)", 
    "word recognition ($y_{14}$)", "number recognition ($y_{15}$)", "figure recognition ($y_{16}$)", 
    "object number ($y_{17}$)", "number-figure ($y_{18}$)", "figure-word ($y_{19}$)")


var_name <-
  c("visual perception (y1)", "cubes (y2)", "paper form board (y3)", "flags (y4)", 
    "general information (y5)", "paragraph comprehension (y6)", "sentence completion (y7)", 
    "word classification (y8)", "word meaning (y9)", "addition (y10)", 
    "code (y11)", "counting groups of dots (y12)", "straight and curved capitals (y13)", 
    "word recognition (y14)", "number recognition (y15)", "figure recognition (y16)", 
    "object number (y17)", "number-figure (y18)", "figure-word (y19)")

ggplot(test_all, aes(left, estimate,
                     group = factor(method))) + 
  geom_hline(yintercept = 0, linetype = 1, colour = "grey80", size = 0.3)+
  coord_cartesian(ylim = c(-1, 1)) + 
  geom_point(aes(shape = method, colour = method), position = position_dodge(width = .7), size = 1,
             show.legend = c(colour = F)) +
  scale_shape_manual(values=c(17, 15, 4, 3)) +
  scale_colour_manual(values=c("#F8766D", "#619CFF", "#00BA38", "#E69F00")) +
  scale_x_discrete(limits = colnames(HS.data[, 7:25]), labels = var_name) + 
  geom_linerange(aes(colour = method, ymin = lower, ymax = upper),  position = position_dodge(width = .7), 
                 size = .5, show.legend = c(colour = F)) +
  labs(x = "Psychological Test", y = "Parameter Estimate", title = "Naive, Data Splitting, PoSI, and Polyhedral Intervals")+
  scale_y_continuous(breaks=c(-1,-.5,0,.5,1)) +
  facet_grid(factor ~ .) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill = "white", color = "white", size = 1),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))
ggplot2::ggsave("tex/example.pdf", plot = last_plot(), 
                width = 7, height = 7)
ggplot2::ggsave("tex/figure_1.tiff", plot = last_plot(), 
                width = 7, height = 7)
