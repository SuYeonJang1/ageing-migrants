
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
data$region <- factor(data$region,levels=c("E","N","S","W"))
data$ndisease <- data$ncond1
head(data)


# FE data -----------------------------------------------------------------

i1 <- table(data$mergeid) > 1
data2 <- subset(data,mergeid%in%names(i1)[i1])
rm(i1)


# panel data --------------------------------------------------------------

pE <- pdata.frame(subset(data2,region == "E"), index = c("mergeid","year"))
pN <- pdata.frame(subset(data2,region == "N"), index = c("mergeid","year"))
pS <- pdata.frame(subset(data2,region == "S"), index = c("mergeid","year"))
pW <- pdata.frame(subset(data2,region == "W"), index = c("mergeid","year"))
rm(data, data2)


# models ------------------------------------------------------------------

mE <- plm(ndisease ~ agegr * imm + income + employed + marry, pE, m = "within")
mN <- plm(ndisease ~ agegr * imm + income + employed + marry, pN, m = "within")
mS <- plm(ndisease ~ agegr * imm + income + employed + marry, pS, m = "within")
mW <- plm(ndisease ~ agegr * imm + income + employed + marry, pW, m = "within")


# prediction --------------------------------------------------------------

source("function/pfitted.R")
source("function/pdiff.R")

lab1 <- c(paste0(seq(50,75,5),"-",seq(54,79,5)))
lab2 <- c("Eastern Europe", "Northern Europe", 
          "Southern Europe", "Western Europe")

l1 <- levels(pE$agegr)
l2 <- levels(pE$imm)

pred <- rbind(
  cbind(region = lab2[1], pfitted(mE, at = list(agegr = l1, imm = l2))),
  cbind(region = lab2[2], pfitted(mN, at = list(agegr = l1, imm = l2))),
  cbind(region = lab2[3], pfitted(mS, at = list(agegr = l1, imm = l2))),
  cbind(region = lab2[4], pfitted(mW, at = list(agegr = l1, imm = l2))))

pred$imm <- as.character(pred$imm)
pred$region <- factor(pred$region, levels = lab2)
pred$agegr <- as.numeric(pred$agegr)
pred


# differential ------------------------------------------------------------

diff <- rbind(
  cbind(region = lab2[1], 
        pdiff(mE, 
              at = list(agegr = l1, imm = l2),
              by = list(imm = "0"))),
  cbind(region = lab2[2],
        pdiff(mN,
              at = list(agegr = l1, imm = l2), 
              by = list(imm = "0"))),
  cbind(region = lab2[3],
        pdiff(mS,
              at = list(agegr = l1, imm = l2), 
              by = list(imm = "0"))),
  cbind(region = lab2[4],
        pdiff(mW,
              at = list(agegr = l1, imm = l2), 
              by = list(imm = "0"))))

diff <- diff[diff$imm == "1" & diff$agegr!= l1[1],]
diff$region <- factor(diff$region, levels = lab2)
row.names(diff) <- NULL

diff


# figures -----------------------------------------------------------------

## colors

Set1 <- brewer.pal(7, "Set1")
Dark2 <- brewer.pal(7, "Dark2")

## prediction

fig1a <- ggplot(pred, aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm), alpha = 0.2) +
  scale_colour_manual(labels = c("Native", "Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native", "Immigrant"), values = Set1[2:1]) + 
  facet_grid(. ~ region, labeller = label_wrap_gen(width = 26)) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "a",
       title = "Chronic health condition development") +
  theme_bw(base_family="Arial") + 
  scale_x_continuous(breaks = 1:6, lab = lab1, minor_breaks = NULL)

## differential

fig1b <- ggplot(diff, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  facet_grid(. ~ region, labeller = label_wrap_gen(width = 26))  +
  labs(x = "Age", y = NULL, colour = NULL, linetype = NULL, tag = "b",
       title = "Immigrant-native differential") +
  theme_bw(base_family = "Arial") +
  scale_x_continuous(b = 1:6, lab = lab1, minor_breaks=NULL, limits = c(1,6))


# results -----------------------------------------------------------------

fig1 <- grid.arrange( 
  arrangeGrob(fig1a, fig1b, nrow = 2, heights = c(1.0, 1.0)),
  top = textGrob("Receiving country group", 
                 gp=gpar(fontsize = 8, 
                         fontfamily = "Arial",
                         fontface = "bold")))




