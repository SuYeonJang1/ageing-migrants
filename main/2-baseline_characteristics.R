
# packages ----------------------------------------------------------------

library(tableone)
library(dummy)
library(ggplot2)
library(extrafont)
library(dplyr)
library(tidyverse)


# bring data --------------------------------------------------------------

data <- read.csv("mypath/baseline.csv", header = T)


# format data -------------------------------------------------------------

data$agegr <- cut(data$age, c(seq(50,75,5), Inf), right = F)
data$ndisease <- data$ncond1
data$ndisease[data$ndisease >= 5] <- "5"
data$ndisease <- factor(data$ndisease, levels = c("0","1","2","3","4","5"))
data$wave <- factor(data$wave)
data$woman <- factor(data$woman, levels = 0:1, lab = c("Men","Women"))
data$agegr <- factor(data$agegr,lab=c(paste0(seq(50,75,5),"-",seq(54,79,5))))
data$imm <- factor(data$imm, levels = 0:1, labels = c("Natives","Immigrants"))
data$edu <- factor(data$edu, levels = 1:3, labels = c("Low","Medium","High"))
data$income <- factor(data$income, levels = 1:3, labels = c("Low","Medium","High"))
data$employed <- factor(data$employed, levels = 0:1, labels = c("NotWorking","Employed"))
data$marry <- factor(data$marry, levels = 0:1, labels = c("NotMarried","Married"))
data$region <- factor(data$region, levels = c("E","N","S","W"))
data$origin <- factor(data$origin, levels = c("Native","Africa","Americas","AsiaPacific",
                                              "EastEurope","OtherEurope"))

data <- data[c("mergeid","wave","agegr","woman","imm","edu",
               "income","employed","marry","region","origin","ndisease")]

data2 <- cbind(data[c("mergeid","imm")],
               dummy(data[c("wave","woman","agegr","edu","income","employed",
                            "marry","region","origin","ndisease")]))


# table 1 -----------------------------------------------------------------

n <- names(data2)[-which(names(data2) %in% c("mergeid","imm"))]
table <- CreateCatTable(vars = n, data = data2, strata = "imm")
table <- cbind(data.frame(print(table, printToggle = F, format = "f", pDigits = 3))[1],
               data.frame(print(table, printToggle = F, format = "p", pDigits = 3))[1],
               data.frame(print(table, printToggle = F, format = "f", pDigits = 3))[2],
               data.frame(print(table, printToggle = F, format = "p", pDigits = 3))[2],
               c(0, attributes(table)$pValues[,1]))
table[1,2] <- "100.0"
table[1,4] <- "100.0"
table[,2] <- as.numeric(table[,2])
table[,4] <- as.numeric(table[,4])
table[,2] <- sapply(table[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
table[,4] <- sapply(table[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
table[,1] <- format(as.numeric(table[,1]), big.mark = ",")
table[,3] <- format(as.numeric(table[,3]), big.mark = ",")
table[,5] <- ifelse(table[,5] == 0, "", ifelse(table[,5] < 0.001, "***", ifelse(
  table[,5] < 0.01, "**", ifelse(table[,5] < 0.05, "*", ""))))
rownames(table) <- gsub(" = 1", "", row.names(table))
table$var <- row.names(table)
table <- table[,c(6,1:5)]
colnames(table) <- c("var","Natives_n","Natives_p",
                     "Immigrants_n","Immigrants_p","pval")
rownames(table) <- NULL
table


# figure table 1 ----------------------------------------------------------

data$imm <- relevel(data$imm, ref = "Natives")
data$region <- factor(data$region, 
                      levels = c("E","N","S","W"), 
                      labels = c("Eastern Europe","Northern Europe",
                               "Southern Europe","Western Europe"))
data$origin <- factor(data$origin, 
                      levels = c("Native","Africa","Americas","AsiaPacific",
                               "EastEurope","OtherEurope"),
                      labels = c("Natives","Africa","The Americas",
                               "Asia and Oceania","Eastern Europe", 
                               "Other European countries"))
data$ndisease <- factor(data$ndisease, 
                        levels = c("0","1","2","3","4","5"),
                        labels = c("0","1","2","3","4","\u22655 "))

figtab1 <- data %>%
  group_by(imm, agegr, ndisease) %>%
  summarize(n = length(ndisease), .groups = "drop") %>%
  group_by(imm, agegr) %>%
  mutate(total = sum(n), percentage = n/total) %>%
  select(-total)

figtab1 <- merge(data.frame(
  imm = rep(levels(data$imm), each = 6 * 6),
  agegr = rep(rep(levels(data$agegr), each = 6), 2),
  ndisease = rep(levels(data$ndisease), 2 * 6)),
  figtab1,  all.x = TRUE)

figtab1$percentage[is.na(figtab1$percentage)] <- 0
figtab1$agegr <- as.numeric(as.factor(figtab1$agegr))
figtab1$ndisease <- factor(figtab1$ndisease, levels = levels(data$ndisease))
figtab1$imm <- factor(figtab1$imm, levels = levels(data$imm))
figtab1 <- figtab1[order(
  as.numeric(figtab1$imm), as.numeric(figtab1$agegr),
  as.numeric(figtab1$ndisease)),]
row.names(figtab1) <- NULL


# figure table 2 -----------------------------------------------------

figtab2 <- data %>%
  group_by(woman, imm, agegr, ndisease) %>%
  summarize(n = length(ndisease), .groups = "drop") %>%
  group_by(woman, imm, agegr) %>%
  mutate(total = sum(n), percentage = n/total) %>%
  select(-total)

figtab2 <- merge(data.frame(
  woman = rep(levels(data$woman), each = 2*6*6),
  imm = rep(rep(levels(data$imm), each = 6*6), 2),
  agegr = rep(rep(levels(data$agegr), each = 6), 2*2),
  ndisease = rep(levels(data$ndisease), 2*6*2)),
  figtab2,  all.x = TRUE)

figtab2$percentage[is.na(figtab2$percentage)] <- 0
figtab2$agegr <- as.numeric(as.factor(figtab2$agegr))
figtab2$ndisease <- factor(figtab2$ndisease, levels = levels(data$ndisease))
figtab2$imm <- factor(figtab2$imm, levels = levels(data$imm))
figtab2$woman <- factor(figtab2$woman, levels = levels(data$woman))
figtab2 <- figtab2[order(
  as.numeric(figtab2$woman), as.numeric(figtab2$imm),
  as.numeric(figtab2$agegr), as.numeric(figtab2$ndisease)),]
row.names(figtab2) <- NULL


# figure table 3 -----------------------------------------------------

figtab3 <- data %>%
  group_by(region, imm, agegr, ndisease) %>%
  summarize(n = length(ndisease), .groups = "drop") %>%
  group_by(region, imm, agegr) %>%
  mutate(total = sum(n), percentage = n/total) %>%
  select(-total)

figtab3 <- merge(data.frame(
  region = rep(levels(data$region), each = 2 * 6 * 6),
  imm = rep(rep(levels(data$imm), each = 6 * 6), 4),
  agegr = rep(rep(levels(data$agegr), each = 6), 2 * 4),
  ndisease = rep(levels(data$ndisease), 2 * 6 * 4)),
  figtab3,  all.x = TRUE)

figtab3$percentage[is.na(figtab3$percentage)] <- 0
figtab3$agegr <- as.numeric(as.factor(figtab3$agegr))
figtab3$ndisease <- factor(figtab3$ndisease, levels = levels(data$ndisease))
figtab3$imm <- factor(figtab3$imm, levels = levels(data$imm))
figtab3$region <- factor(figtab3$region, levels = levels(data$region))
figtab3 <- figtab3[order(
  as.numeric(figtab3$region), as.numeric(figtab3$imm),
  as.numeric(figtab3$agegr), as.numeric(figtab3$ndisease)),]
row.names(figtab3) <- NULL


# figure table 4 -----------------------------------------------------

figtab4 <- data %>%
  group_by(origin, imm, agegr, ndisease) %>%
  summarize(n = length(ndisease), .groups = "drop") %>%
  group_by(origin, imm, agegr) %>%
  mutate(total = sum(n), percentage = n/total) %>%
  select(-total)

figtab4 <- merge(data.frame(
  origin = rep(levels(data$origin), each = 6 * 6),
  agegr = rep(rep(levels(data$agegr), each = 6), 6),
  ndisease = rep(levels(data$ndisease), 6 * 6)),
  figtab4,  all.x = TRUE)

figtab4$percentage[is.na(figtab4$percentage)] <- 0
figtab4$agegr <- as.numeric(as.factor(figtab4$agegr))
figtab4$ndisease <- factor(figtab4$ndisease, levels = levels(data$ndisease))
figtab4$imm <- factor(figtab4$imm, levels = levels(data$imm))
figtab4$origin <- factor(figtab4$origin, levels = levels(data$origin))
figtab4 <- figtab4[order(
  as.numeric(figtab4$origin), as.numeric(figtab4$imm),
  as.numeric(figtab4$agegr), as.numeric(figtab4$ndisease)),]
row.names(figtab4) <- NULL


# figures -----------------------------------------------------------------

level <- paste0(seq(50,75,5), "-", seq(54,79,5))

fig1 <- ggplot(figtab1, aes(x = agegr, y = percentage, fill = ndisease)) + 
  geom_area(colour = "black", linewidth = 0.3, alpha = 0.3, outline.type = "full") +
  labs(x = "Age", y = NULL, title = NULL, fill = NULL) +
  facet_grid(. ~ imm) +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(breaks = 1:6, minor_breaks = NULL, lab = level, expand = c(0.05, 0)) +
  scale_y_continuous(breaks = seq(0,1,0.2), minor_breaks = NULL, 
                     lab = scales::percent, expand = c(0.05,0)) +
  guides(fill = guide_legend(nrow = 1))

fig2 <- ggplot(figtab2, aes(x = agegr, y = percentage, fill = ndisease)) + 
  geom_area(colour = "black", linewidth = 0.3, alpha = 0.3, outline.type = "full") +
  labs(x = "Age", y = NULL, title = NULL, fill = NULL) +
  facet_grid(woman ~ imm) +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(breaks = 1:6, minor_breaks = NULL, lab = level, expand = c(0.05, 0)) +
  scale_y_continuous(breaks = seq(0,1,0.2), minor_breaks = NULL, 
                     lab = scales::percent, expand = c(0.05,0)) +
  guides(fill = guide_legend(nrow = 1))

fig3 <- ggplot(figtab3, aes(x = agegr, y = percentage, fill = ndisease)) + 
  geom_area(colour = "black", linewidth = 0.3, alpha = 0.3, outline.type = "full") +
  labs(x = "Age", y = NULL, title = NULL, fill = NULL) +
  facet_grid(region ~ imm) +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(breaks = 1:6, minor_breaks = NULL, lab = level, expand = c(0.05, 0)) +
  scale_y_continuous(breaks = seq(0,1,0.2), minor_breaks = NULL, 
                     lab = scales::percent, expand = c(0.05,0)) +
  guides(fill = guide_legend(nrow = 1))

fig4 <- ggplot(figtab4, aes(x = agegr, y = percentage, fill = ndisease)) + 
  geom_area(colour = "black", linewidth = 0.3, alpha = 0.3, outline.type = "full") +
  labs(x = "Age", y = NULL, title = NULL, fill = NULL) +
  facet_wrap(. ~ origin) +
  theme_bw(base_family = "Arial") + 
  scale_x_continuous(breaks = 1:6, minor_breaks = NULL, lab = level, expand = c(0.05, 0)) +
  scale_y_continuous(breaks = seq(0,1,0.2), minor_breaks = NULL, 
                     lab = scales::percent, expand = c(0.05,0)) +
  guides(fill = guide_legend(nrow = 1))

