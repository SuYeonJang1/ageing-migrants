
# packages ----------------------------------------------------------------

library(plm)
library(RColorBrewer)
library(ggplot2)
library(extrafont)
library(grid)
library(gridExtra)
library(gtable)


# bring data --------------------------------------------------------------

data <- read.csv("mypath/supplements/main_cvd.csv", header = T)


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

## random-effects models

mre1 <- plm(ndisease ~ agegr + imm + woman, data = p1, model = "random")
mre2 <- plm(ndisease ~ agegr * imm + woman, data = p1, model = "random")
mre3 <- plm(ndisease ~ agegr * imm + woman + edu + income + employed + marry, 
            data = p1, m = "random")

## fixed-effects models

mfe1 <- plm(ndisease ~ agegr + imm, data = p2, model = "within")
mfe2 <- plm(ndisease ~ agegr * imm, data = p2, model = "within")
mfe3 <- plm(ndisease ~ agegr * imm + income + employed + marry, 
            data = p2, model = "within")


# prediction --------------------------------------------------------------

source("function/pfitted.R")
source("function/pdiff.R")

lab <- c(paste0(seq(50,75,5),"-",seq(54,79,5)))

l1 <- levels(p1$agegr)
l2 <- levels(p1$imm)

pred_ols <- pfitted(mols3, at = list(agegr = l1, imm = l2))
pred_ols$imm <- as.character(pred_ols$imm)
pred_ols$agegr <- as.numeric(pred_ols$agegr)

pred_fe <- pfitted(mfe3, at = list(agegr = l1, imm = l2))
pred_fe$imm <- as.character(pred_fe$imm)
pred_fe$agegr <- as.numeric(pred_fe$agegr)

pred_re <- pfitted(mre3, at = list(agegr = l1, imm = l2))
pred_re$imm <- as.character(pred_re$imm)
pred_re$agegr <- as.numeric(pred_re$agegr)

pred_ols

pred_re

pred_fe


# differential ------------------------------------------------------------

diff_ols <- pdiff(mols3, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_ols <- diff_ols[diff_ols$imm == "1",]
row.names(diff_ols) <- NULL

diff_fe <- pdiff(mfe3, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_fe <- diff_fe[diff_fe$imm == "1" & diff_fe$agegr != l1[1],]
row.names(diff_fe) <- NULL

diff_re <- pdiff(mre3, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_re <- diff_re[diff_re$imm == "1",]
row.names(diff_re) <- NULL

diff_ols

diff_re

diff_fe



