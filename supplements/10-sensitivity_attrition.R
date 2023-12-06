
# packages ----------------------------------------------------------------

library(plm)
library(ipw)
library(RColorBrewer)
library(ggplot2)
library(extrafont)
library(grid)
library(gridExtra)
library(gtable)


# bring data --------------------------------------------------------------

data <- read.csv("mypath/main.csv", header = T)
drop <- read.csv("mypath/supplements/dropouts.csv", header = T)
drop <- drop[c("mergeid","wave","drop")]


# merge -------------------------------------------------------------------

data <- merge(data, drop, by = c("mergeid", "wave"), all.x=TRUE)
rm(drop)


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
data$time <- data$nwave - 1
data <- data[order(data$mergeid, -data$wave),]
data$drop <- ifelse(duplicated(paste0(data$mergeid, data$drop)), NA, data$drop)
data$drop_all <- ifelse(!is.na(data$drop) & data$drop != "int", 1, 0)
data$drop_all <- factor(data$drop_all, levels = 0:1)
data$drop_death <- ifelse(!is.na(data$drop) & data$drop == "dead", 1, 0)
data$drop_death <- factor(data$drop_death, levels = 0:1)
data <- data[with(data, !rep(tapply(is.na(drop),mergeid,all), table(mergeid))),]
data <- data[order(data$mergeid, data$wave),]
data <- data[c("mergeid","wave","year","agegr","woman","imm","edu","income",
               "employed","marry","ndisease","drop_all","drop_death","time")]
head(data)


# FE data -----------------------------------------------------------------

i1 <- table(data$mergeid) > 1
data2 <- subset(data, mergeid %in% names(i1)[i1])
rm(i1)


# panel data --------------------------------------------------------------

p1 <- pdata.frame(data, index = c("mergeid","year"))
p2 <- pdata.frame(data2, index = c("mergeid","year"))


# ipw ---------------------------------------------------------------------

p1$ipw1 <- ipwtm( ## ols models, attrition due to all reasons
  exposure = drop_all, family = "binomial", link="logit", id = mergeid,
  denominator =~ agegr * imm + woman + edu + income + employed + marry,
  timevar = time, type = "cens", data = data)$ipw.weights

p1$ipw2 <- ipwtm( ## ols models, attrition due to death
  exposure = drop_death, family = "binomial", link="logit", id = mergeid,
  denominator =~ agegr * imm + woman + edu + income + employed + marry,
  timevar = time, type = "cens", data = data)$ipw.weights

p2$ipw1 <- ipwtm( ## fe models, attrition due to all reasons
  exposure = drop_all, family = "binomial", link="logit", id = mergeid,
  denominator =~ agegr * imm + woman + edu + income + employed + marry,
  timevar = time, type = "cens", data = data2)$ipw.weights

p2$ipw2 <- ipwtm( ## fe models, attrition due to death
  exposure = drop_death, family = "binomial", link="logit", id = mergeid,
  denominator=~agegr * imm + woman + edu + income + employed + marry,
  timevar = time, type = "cens", data = data2)$ipw.weights

rm(data,data2)


# not weighted: all dropouts ----------------------------------------------

m1ols <- plm(ndisease ~ agegr * imm + woman + edu + income + employed + 
               marry + drop_all, p1, model = "pooling")
m1re <- update(m1ols, model = "random")
m1fe <- plm(ndisease ~ agegr * imm + income + employed + marry + drop_all, 
            data = p2, model = "within")


# not weighted: dropouts due to death -------------------------------------

m2ols <- plm(ndisease ~ agegr * imm + woman + edu + income + employed + 
               marry + drop_death, p1, model = "pooling")
m2re <- update(m2ols, model = "random")
m2fe <- plm(ndisease ~ agegr * imm + income + employed + marry + drop_death, 
            data = p2, model = "within")


# weighted: all dropouts --------------------------------------------------

w1ols <- plm(ndisease ~ agegr * imm + woman + edu + income + employed + 
               marry + drop_all, weights = ipw1, p1, model = "pooling")
w1re <- update(w1ols, model = "random")
w1fe <- plm(ndisease ~ agegr * imm + income + employed + marry + drop_all, 
            weights = ipw1, data = p2, model = "within")


# weighted: dropouts due to death -----------------------------------------

w2ols <- plm(ndisease ~ agegr * imm + woman + edu + income + employed + 
               marry + drop_death, weights = ipw2, p1, model = "pooling")
w2re <- update(w2ols, model = "random")
w2fe <- plm(ndisease ~ agegr * imm + income + employed + marry + drop_death, 
            weights = ipw2, data = p2, model = "within")


# prediction --------------------------------------------------------------

source("function/pfitted.R")
source("function/pdiff.R")

lab <- c(paste0(seq(50,75,5),"-",seq(54,79,5)))

l1 <- levels(p2$agegr)
l2 <- levels(p2$imm)

## all dropouts
### not weighted

pred_fe1 <- pfitted(m1fe, at = list(agegr = l1, imm = l2))
pred_fe1$imm <- as.character(pred_fe1$imm)
pred_fe1$agegr <- as.numeric(pred_fe1$agegr)

### weighted

pred_fe2 <- pfitted(w1fe, at = list(agegr = l1, imm = l2))
pred_fe2$imm <- as.character(pred_fe2$imm)
pred_fe2$agegr <- as.numeric(pred_fe2$agegr)

## dropouts due to death
### not weighted

pred_fe3 <- pfitted(m2fe, at = list(agegr = l1, imm = l2))
pred_fe3$imm <- as.character(pred_fe3$imm)
pred_fe3$agegr <- as.numeric(pred_fe3$agegr)

### weighted

pred_fe4 <- pfitted(w2fe, at = list(agegr = l1, imm = l2))
pred_fe4$imm <- as.character(pred_fe4$imm)
pred_fe4$agegr <- as.numeric(pred_fe4$agegr)


# differential ------------------------------------------------------------

## all dropouts
### not weighted

diff_fe1 <- pdiff(m1fe, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_fe1 <- diff_fe1[diff_fe1$imm == "1" & diff_fe1$agegr != l1[1],]
row.names(diff_fe1) <- NULL

### weighted

diff_fe2 <- pdiff(w1fe, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_fe2 <- diff_fe2[diff_fe2$imm == "1" & diff_fe2$agegr != l1[1],]
row.names(diff_fe2) <- NULL

## dropouts due to death
### not weighted

diff_fe3 <- pdiff(m2fe, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_fe3 <- diff_fe3[diff_fe3$imm == "1" & diff_fe3$agegr != l1[1],]
row.names(diff_fe3) <- NULL

### weighted

diff_fe4 <- pdiff(w2fe, at = list(agegr = l1, imm = l2), by = list(imm = "0"))
diff_fe4 <- diff_fe4[diff_fe4$imm == "1" & diff_fe4$agegr != l1[1],]
row.names(diff_fe4) <- NULL


# figures -----------------------------------------------------------------

## colors

Set1 <- brewer.pal(7, "Set1")
Dark2 <- brewer.pal(7,"Dark2")

## all dropouts
### prediction

fig1a <- ggplot(pred_fe1,aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "a",
       title = "Baseline model") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL) 

fig1b <- ggplot(pred_fe2,aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "b",
       title = "Weighted model") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL) 

### differential

fig1c <- ggplot(diff_fe1, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "c",
       title = "Baseline model") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))

fig1d <- ggplot(diff_fe2, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "d",
       title = "Weighted model") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))


## dropouts due to death
### prediction

fig2a <- ggplot(pred_fe3,aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "a",
       title = "Baseline model") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL) 

fig2b <- ggplot(pred_fe4,aes(x = agegr, y = fitted)) +
  geom_line(aes(colour = imm), linewidth = 0.5) +
  geom_ribbon(aes(ymin = upr, ymax = lwr, fill = imm, colour = imm),
              alpha = 0.2, linewidth = 0.3) +
  scale_colour_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  scale_fill_manual(labels = c("Native","Immigrant"), values = Set1[2:1]) +
  labs(x = "Age", y = "", colour = NULL, fill = NULL, tag = "b",
       title = "Weighted model") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL) 

### differential

fig2c <- ggplot(diff_fe3, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "c",
       title = "Baseline model") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))

fig2d <- ggplot(diff_fe4, aes(x = as.numeric(as.factor(agegr)), y = diff)) +
  geom_hline(yintercept = 0, color = "dark gray", alpha = 0.8) +
  geom_line(aes(color = "differential", linetype = "differential")) +
  geom_line(aes(y = upr, color = "95% ci", linetype = "95% ci")) +
  geom_line(aes(y = lwr, color = "95% ci", linetype = "95% ci")) + 
  scale_colour_manual(values = Dark2[2:1]) +
  scale_linetype_manual(values = c("21","solid")) +
  labs(x = "Age", y = "",colour = NULL, linetype = NULL, tag = "d",
       title = "Weighted model") +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(b = 1:6, lab = lab, minor_breaks = NULL, limits = c(1, 6))


# results -----------------------------------------------------------------

fig1 <- grid.arrange(arrangeGrob( # all dropouts
  
  arrangeGrob( ## prediction
    arrangeGrob(fig1a, fig1b, ncol = 2, widths = c(1.0, 1.0)),
    top = textGrob("Chronic disease\ndevelopment", 
                   gp = gpar(fontsize = 8, 
                             fontfamily = "Arial",
                             fontface = "bold"))),
  
  arrangeGrob( ## differential
    arrangeGrob(fig1c, fig1d, ncol = 2, widths = c(1.0, 1.0)),
    top = textGrob("Chronic disease\ndevelopment", 
                   gp = gpar(fontsize = 8, 
                             fontfamily = "Arial",
                             fontface = "bold"))),
  
  nrow = 2, heights = c(1.0, 1.0)))

fig2 <- grid.arrange(arrangeGrob( # dropouts due to death
  
  arrangeGrob( ## prediction
    arrangeGrob(fig2a, fig2b, ncol = 2, widths = c(1.0, 1.0)),
    top = textGrob("Chronic disease\ndevelopment", 
                   gp = gpar(fontsize = 8, 
                             fontfamily = "Arial",
                             fontface = "bold"))),
  
  arrangeGrob( ## differential
    arrangeGrob(fig2c, fig2d, ncol = 2, widths = c(1.0, 1.0)),
    top = textGrob("Chronic disease\ndevelopment", 
                   gp = gpar(fontsize = 8, 
                             fontfamily = "Arial",
                             fontface = "bold"))),
  
  nrow = 2, heights = c(1.0, 1.0)))



