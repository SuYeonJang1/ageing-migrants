
# packages ----------------------------------------------------------------

library(plm)


# bring data --------------------------------------------------------------

data <- read.csv("mypath/supplements/main_missing.csv", header = T)
data <- data[!is.na(data$imm),]


# missing -----------------------------------------------------------------

data$edu <- ifelse(is.na(data$edu), 4, data$edu)
data$employed <- ifelse(is.na(data$employed), 2, data$employed)
data$marry <- ifelse(is.na(data$marry), 2, data$marry)


# variables ---------------------------------------------------------------

data$agegr <- cut(data$age,c(seq(50,75,5),Inf), right=F)
data$agegr <- factor(data$agegr)
data$woman <- factor(data$woman,levels=0:1)
data$imm <- factor(data$imm,levels=0:1)
data$edu <- factor(data$edu, levels=1:4, lab=c("low","med","high","na"))
data$income <- factor(data$income, levels=1:3, lab=c("low","med","high"))
data$employed <- factor(data$employed, levels=0:2, lab=c("no","yes","na"))
data$marry <- factor(data$marry, levels=0:2, lab=c("no","yes","na"))
data$ndisease <- data$ncond1
head(data)


# by nativity -------------------------------------------------------------

datan <- data[data$imm==0,]
datai <- data[data$imm==1,]


# FE data -----------------------------------------------------------------

i1 <- table(data$mergeid)>1
i2 <- table(datan$mergeid)>1
i3 <- table(datai$mergeid)>1

data2 <- subset(data,mergeid%in%names(i1)[i1])
datan2 <- subset(datan,mergeid%in%names(i2)[i2])
datai2 <- subset(datai,mergeid%in%names(i3)[i3])

rm(i1,i2,i3)


# panel data --------------------------------------------------------------

## pooled

p1 <- pdata.frame(data,index=c("mergeid","year"))
p2 <- pdata.frame(data2,index=c("mergeid","year"))

## natives

pn1 <- pdata.frame(datan,index=c("mergeid","year"))
pn2 <- pdata.frame(datan2,index=c("mergeid","year"))

## immigrants

pi1 <- pdata.frame(datai,index=c("mergeid","year"))
pi2 <- pdata.frame(datai2,index=c("mergeid","year"))

rm(data, data2, datan, datan2, datai, datai2)


# models ------------------------------------------------------------------

## ols models

mols1 <- plm(ndisease ~ agegr + imm + woman, data = p1, model = "pooling")
mols2 <- plm(ndisease ~ agegr * imm + woman, data = p1, model = "pooling")
mols3 <- update(mols2, . ~ . + edu + income + employed + marry)

## random-effects models

mre1 <- plm(ndisease ~ agegr + imm + woman, data = p1, model = "random")
mre2 <- plm(ndisease ~ agegr * imm + woman, data = p1, model = "random")
mre3 <- update(mre2, . ~ . + edu + income + employed + marry)

## fixed-effects models

mfe1 <- plm(ndisease ~ agegr + imm, data = p2, model = "within")
mfe2 <- plm(ndisease ~ agegr * imm, data = p2, model = "within")
mfe3 <- update(mfe2, . ~ . + income + employed + marry)


# models: natives ---------------------------------------------------------

## ols models

mols_n1 <- plm(ndisease ~ agegr + woman, data = pn1, model = "pooling")
mols_n2 <- update(mols_n1, . ~ . + edu + income + employed + marry)

## random-effects models

mre_n1 <- plm(ndisease ~ agegr + woman, data = pn1, model = "random")
mre_n2 <- update(mre_n1, . ~ . + edu + income + employed + marry)

## fixed-effects models

mfe_n1 <- plm(ndisease ~ agegr, data = pn2, model = "within")
mfe_n2 <- update(mfe_n1, . ~ . + income + employed + marry)


# models: immigrants ------------------------------------------------------

## ols models

mols_i1 <- plm(ndisease ~ agegr + woman, data = pi1, model = "pooling")
mols_i2 <- update(mols_i1, . ~ . + edu + income + employed + marry)

## random-effects models

mre_i1 <- plm(ndisease ~ agegr + woman, data = pi1, model = "random")
mre_i2 <- update(mre_i1, . ~ . + edu + income + employed + marry)

## fixed-effects models

mfe_i1 <- plm(ndisease ~ agegr, data = pi2, model = "within")
mfe_i2 <- update(mfe_i1, . ~ . + income + employed + marry)



