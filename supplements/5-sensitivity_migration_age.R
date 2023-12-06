
# packages ----------------------------------------------------------------

library(plm)


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
data$region <- factor(data$region, levels = c("E","N","S","W"))
data$origin[data$origin %in% c("Native","EastEurope")] <- "NativeEast"
data$origin <- relevel(factor(data$origin), ref = "NativeEast")
data$cohort[data$cohort == "Native"] <- NA
data$agemg <- as.numeric(data$cohort) - as.numeric(data$yrbirth)
data$agemg[!is.na(data$agemg) & data$agemg<0] <- NA
data$agemg <- cut(data$agemg,c(0,18,35,Inf), right = F)
data$agemg <- ifelse(data$imm == 0, "Native", data$agemg)
data$agemg <- factor(data$agemg, levels = c("Native",1:3))
data$ndisease <- data$ncond1
data <- data[!is.na(data$agemg),]


# FE data -----------------------------------------------------------------

i1 <- table(data$mergeid)>1
data2 <- subset(data,mergeid%in%names(i1)[i1])
rm(i1)


# panel data --------------------------------------------------------------

i <- levels(data$agemg)
i

p1_0 <- pdata.frame(subset(data, agemg %in% i[c(1,2)]), 
                    index = c("mergeid","year"))
p1_18 <- pdata.frame(subset(data,agemg %in% i[c(1,3)]), 
                     index = c("mergeid","year"))
p1_35 <- pdata.frame(subset(data,agemg %in% i[c(1,4)]), 
                     index = c("mergeid","year"))
p2_0 <- pdata.frame(subset(data2,agemg %in% i[c(1,2)]), 
                    index = c("mergeid","year"))
p2_18 <- pdata.frame(subset(data2,agemg %in% i[c(1,3)]), 
                     index = c("mergeid","year"))
p2_35 <- pdata.frame(subset(data2,agemg %in% i[c(1,4)]), 
                     index = c("mergeid","year"))
rm(i, data, data2)


# models ------------------------------------------------------------------

## ols models

mols0 <- plm(ndisease ~ agegr * imm + woman + edu + income + employed + marry, 
             data = p1_0, m = "pooling")
mols18 <- update(mols0, data = p1_18)
mols35 <- update(mols0, data = p1_35)

## random-effects models

mre0 <- plm(ndisease ~ agegr * imm + woman + edu + income + employed + marry, 
            data = p1_0, m = "random")
mre18 <- update(mre0, data = p1_18)
mre35 <- update(mre0, data = p1_35)

## fixed-effects models

mfe0 <- plm(ndisease ~ agegr * imm + income + employed + marry, 
            data = p2_0, model = "within")
mfe18 <- update(mfe0, data = p2_18)
mfe35 <- update(mfe0, data = p2_35)



