
# packages ----------------------------------------------------------------

library(plm)


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

