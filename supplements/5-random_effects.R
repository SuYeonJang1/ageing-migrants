
# packages ----------------------------------------------------------------

library(plm)
library(RColorBrewer)
library(ggplot2)
library(extrafont)
library(grid)
library(gridExtra)
library(gtable)


# bring data --------------------------------------------------------------

data <- read.csv("mypath/main.csv", header = T)


# variables ---------------------------------------------------------------

data$agegr <- cut(data$age, c(seq(50,75,5), Inf), right = F)
data$agegr <- factor(data$agegr)
data$woman <- factor(data$woman, levels = 0:1)
data$imm <- factor(data$imm, levels = 0:1)
data$edu <- factor(data$edu, levels = 1:3)
data$income <- factor(data$income, levels = 1:3)
data$employed <- factor(data$employed, levels = 0:1)
data$marry <- factor(data$marry, levels = 0:1)
data$ndisease <- data$ncond1
head(data)


# panel data --------------------------------------------------------------

panel <- pdata.frame(data, index = c("mergeid","year"))
rm(data)


# models ------------------------------------------------------------------

## random-effects models

mre1 <- plm(ndisease ~ agegr + imm + woman, data = panel, model = "random")
mre2 <- plm(ndisease ~ agegr * imm + woman, data = panel, model = "random")
mre3 <- plm(ndisease ~ agegr * imm + woman + edu + income + employed + marry, 
            data = panel, m = "random")


# prediction --------------------------------------------------------------

source("function/pfitted.R")
source("function/pdiff.R")

lab <- c(paste0(seq(50,75,5),"-",seq(54,79,5)))
l1 <- levels(panel$agegr)
l2 <- levels(panel$imm)

pred_re <- pfitted(mre3, at = list(agegr = l1, imm = l2))
pred_re$imm <- as.character(pred_re$imm)
pred_re$agegr <- as.numeric(pred_re$agegr)
pred_re


# differential ------------------------------------------------------------

diff_re <- pdiff(mre3, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_re <- diff_re[diff_re$imm == "1",]
row.names(diff_re) <- NULL

diff_re


# figures -----------------------------------------------------------------

## colors

Set1 <- brewer.pal(7, "Set1")
Dark2 <- brewer.pal(7,"Dark2")


## prediction

fig_a <- ggplot(pred_re, aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "a",
       title = "Predicted number of\nchronic diseases") +
  theme_bw(base_family = "Arial") +
  scale_x_continuous(breaks = 1:6, labels = lab, minor_breaks = NULL)

## differential

fig_b <- ggplot(diff_re, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "b",
       title = "Immigrant-native\ndifferential") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))


# results -----------------------------------------------------------------

fig <- grid.arrange( # random effects
  arrangeGrob(fig_a, fig_b, ncol = 2, widths = c(1.0, 1.0)),
  top=textGrob("Random-effects", gp = gpar(
    fontsize = 8, fontfamily = "Arial", fontface = "bold")))

