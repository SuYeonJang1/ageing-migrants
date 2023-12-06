
# packages ----------------------------------------------------------------

library(plm)
library(texreg)
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


# FE data -----------------------------------------------------------------

i1 <- table(data$mergeid) > 1
data2 <- subset(data,mergeid %in% names(i1)[i1])
rm(i1)


# panel data --------------------------------------------------------------

p1 <- pdata.frame(data, index = c("mergeid","year"))
p2 <- pdata.frame(data2, index = c("mergeid","year"))
rm(data, data2)


# models ------------------------------------------------------------------

## ols models

mols1 <- plm(ndisease ~ agegr + imm + woman, data = p1, model = "pooling")
mols2 <- plm(ndisease ~ agegr * imm + woman, data = p1, model = "pooling")
mols3 <- plm(ndisease ~ agegr * imm + woman + edu + income + employed + marry, 
             data = p1, m = "pooling")

## fixed-effects models

mfe1 <- plm(ndisease ~ agegr + imm, data = p2, model = "within")
mfe2 <- plm(ndisease ~ agegr * imm, data = p2, model = "within")
mfe3 <- plm(ndisease ~ agegr * imm + income + employed + marry, 
            data = p2, model = "within")


# prediction --------------------------------------------------------------

source("function/pfitted.R")
source("function/pdiff.R")

l1 <- levels(p1$agegr)
l2 <- levels(p1$imm)

pred_ols <- pfitted(mols3, at = list(agegr = l1, imm = l2))
pred_ols$imm <- as.character(pred_ols$imm)
pred_ols$agegr <- as.numeric(pred_ols$agegr)

pred_fe <- pfitted(mfe3, at = list(agegr = l1, imm = l2))
pred_fe$imm <- as.character(pred_fe$imm)
pred_fe$agegr <- as.numeric(pred_fe$agegr)

pred_ols

pred_fe


# differential ------------------------------------------------------------

diff_ols <- pdiff(mols3, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_ols <- diff_ols[diff_ols$imm == "1",]
row.names(diff_ols) <- NULL

diff_fe <- pdiff(mfe3, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_fe <- diff_fe[diff_fe$imm == "1" & diff_fe$agegr != l1[1],]
row.names(diff_fe) <- NULL

diff_ols

diff_fe


# figures -----------------------------------------------------------------

## colors

Set1 <- brewer.pal(7, "Set1")
Dark2 <- brewer.pal(7,"Dark2")

## ols
### prediction

fig1a <- ggplot(pred_ols, aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "a",
       title = "Predicted number of\nchronic diseases") +
  theme_bw(base_family = "Arial") + theme +
  scale_x_continuous(breaks = 1:6, labels = lab, minor_breaks = NULL)

### differential

fig1b <- ggplot(diff_ols, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "b",
       title = "Immigrant-native\ndifferential") +
  theme_bw(base_family = "Arial") + theme +
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))

## fe
### prediction

fig2a <- ggplot(pred_fe, aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "a",
       title = "Chronic disease development") +
  theme_bw(base_family = "Arial") + theme +
  scale_x_continuous(breaks = 1:6, labels = lab, minor_breaks = NULL)

### differential

fig2b <- ggplot(diff_fe, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "b",
       title = "Immigrant-native\ndifferential") +
  theme_bw(base_family = "Arial") + theme +
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))


# results -----------------------------------------------------------------

fig1 <- grid.arrange( # OLS
  arrangeGrob(fig1a, fig1b, ncol = 2, widths = c(1.0, 1.0)),
  top = textGrob("OLS", gp = gpar(fontsize = 8, 
                                  fontfamily = "Arial",
                                  fontface = "bold")))

fig2 <- grid.arrange( # fixed effects
  arrangeGrob(fig2a, fig2b, ncol = 2, widths = c(1.0, 1.0)),
  top = textGrob("Fixed-effects", gp = gpar(fontsize = 8, 
                                  fontfamily = "Arial",
                                  fontface = "bold")))


