
# packages ----------------------------------------------------------------

library(plm)
library(ggplot2)
library(extrafont)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)


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
data$smoke <- factor(data$smoke, levels = 1:0)
data$alcohol <- factor(data$alcohol, levels = 1:0)
data$pa <- factor(data$pa, levels = 0:1)
data$ndisease <- data$ncond1
data <- data[!is.na(data$smoke) & !is.na(data$alcohol) & !is.na(data$pa),]
head(data)


# FE data -----------------------------------------------------------------

i1 <- table(data$mergeid) > 1
data2 <- subset(data,mergeid %in% names(i1)[i1])
rm(i1)


# panel data --------------------------------------------------------------

panel <- pdata.frame(data2,index=c("mergeid","year"))
rm(data, data2)


# models ------------------------------------------------------------------

model1 <- plm(ndisease ~ agegr + imm, data = panel, model = "within")
model2 <- plm(ndisease ~ agegr * imm, data = panel, model = "within")
model3 <- update(model2, . ~ . + income + employed + marry)
model4 <- update(model3, . ~ . + smoke)
model5 <- update(model4, . ~ . + alcohol + pa)


# prediction --------------------------------------------------------------

source("function/pfitted.R")
source("function/pdiff.R")

lab <- c(paste0(seq(50,75,5),"-",seq(54,79,5)))

l1 <- levels(panel$agegr)
l2 <- levels(panel$imm)

pred_fe1 <- pfitted(model3, at = list(agegr = l1, imm = l2))
pred_fe1$imm <- as.character(pred_fe1$imm)
pred_fe1$agegr <- as.numeric(pred_fe1$agegr)

pred_fe2 <- pfitted(model4, at = list(agegr = l1, imm = l2))
pred_fe2$imm <- as.character(pred_fe2$imm)
pred_fe2$agegr <- as.numeric(pred_fe2$agegr)

pred_fe3 <- pfitted(model5, at = list(agegr = l1, imm = l2))
pred_fe3$imm <- as.character(pred_fe3$imm)
pred_fe3$agegr <- as.numeric(pred_fe3$agegr)

pred_fe1

pred_fe2

pred_fe3


# differential ------------------------------------------------------------

diff_fe1 <- pdiff(model3, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_fe1 <- diff_fe1[diff_fe1$imm == "1" & diff_fe1$agegr != l1[1],]
row.names(diff_fe1) <- NULL

diff_fe2 <- pdiff(model4, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_fe2 <- diff_fe2[diff_fe2$imm == "1" & diff_fe2$agegr != l1[1],]
row.names(diff_fe2) <- NULL

diff_fe3 <- pdiff(model5, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_fe3 <- diff_fe3[diff_fe3$imm == "1" & diff_fe3$agegr != l1[1],]
row.names(diff_fe3) <- NULL

diff_fe1

diff_fe2

diff_fe3


# figures -----------------------------------------------------------------

## colors

Set1 <- brewer.pal(7, "Set1")
Dark2 <- brewer.pal(7,"Dark2")

## prediction
### 

figa <- ggplot(pred_fe1, aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "a",
       title = "No health behaviour variables") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(breaks = 1:6, labels = lab, minor_breaks = NULL)

figb <- ggplot(pred_fe2, aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "b", 
       title = "Smoking") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(breaks = 1:6, labels = lab, minor_breaks = NULL)

figc <- ggplot(pred_fe3, aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "c", 
       title = "Smoking, alcohol, and physical activity") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(breaks = 1:6, labels = lab, minor_breaks = NULL)

figd <- ggplot(diff_fe1, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "d",
       title = "No health behaviour variables") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))

fige <- ggplot(diff_fe2, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "e", 
       title = "Smoking") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))
  
figf <- ggplot(diff_fe3, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "f", 
       title = "Smoking, alcohol, and physical activity") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))
  

# results -----------------------------------------------------------------

fig <- grid.arrange(arrangeGrob(
  
  arrangeGrob( # prediction
    arrangeGrob(figa, figb, figc, ncol = 3, widths = c(1.0, 1.0, 1.0)),
    top=textGrob("Chronic disease development", 
                 gp = gpar(fontsize = 8, 
                           fontfamily = "Arial", 
                           fontface = "bold"))),
  
  arrangeGrob( # differential
    arrangeGrob(figd, fige, figf, ncol = 3, widths = c(1.0, 1.0, 1.0)),
    top=textGrob("Immigrant-native differential", 
                 gp = gpar(fontsize = 8, 
                           fontfamily = "Arial", 
                           fontface = "bold"))),
  
  nrow = 2, heights = c(1.0, 1.0)))


