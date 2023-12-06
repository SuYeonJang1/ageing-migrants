
# packages ----------------------------------------------------------------

library(tableone)


# bring data --------------------------------------------------------------

data <- read.csv("mypath/baseline.csv", header = T)


# format data -------------------------------------------------------------

data$agegr <- cut(data$age, c(seq(50,75,5), Inf), right = F)
data$ndisease <- data$ncond1
data$ndisease0 <- ifelse(data$ndisease == 0, 1, 0)
data$ndisease1 <- ifelse(data$ndisease >= 1, 1, 0)
data$ndisease2 <- ifelse(data$ndisease >= 2, 1, 0)
data$ndisease3 <- ifelse(data$ndisease >= 3, 1, 0)
data$ndisease4 <- ifelse(data$ndisease >= 4, 1, 0)
data$ndisease5 <- ifelse(data$ndisease >= 5, 1, 0)
data$agegr <- factor(data$agegr, lab = c(paste0(seq(50,75,5),"-",seq(54,79,5))))
data$imm <- factor(data$imm, levels = 0:1, labels = c("Natives","Immigrants"))
data$region <- factor(data$region, levels = c("E","N","S","W"))
data$origin <- factor(data$origin, levels = c(
  "Native","Africa","Americas","AsiaPacific","EastEurope","OtherEurope"))
data <- data[c("mergeid","agegr","woman","imm","region","origin","ndisease0",
               "ndisease1","ndisease2","ndisease3","ndisease4","ndisease5")]
head(data)


# table 1 -----------------------------------------------------------------

## natives vs immigrants

t1 <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease", 0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0, sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

t1 <- cbind(rep(0:5, times = 6), t1)
t1 <- cbind(rep(paste0(seq(50,75,5),"-",seq(54,79,5)),e = 6), t1)
colnames(t1) <- c("agegr","ncond","nat_n","nat_p","imm_n","imm_p","pval")
rownames(t1) <- NULL
t1


# table 2 -----------------------------------------------------------------

## men vs women
### men

t2_m <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, woman == 0 & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease",0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0,sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

### women

t2_f <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, woman == 1 & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease", 0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0,sapply(1:6, function(x) attr(res, "pValues")[x,ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

t2 <- cbind(t2_m, t2_f)
t2 <- cbind(rep(0:5, times = 6), t2)
t2 <- cbind(rep(paste0(seq(50,75,5), "-", seq(54,79,5)), e = 6), t2)
colnames(t2) <- c(
  "agegr","ncond","m_nat_n","m_nat_p","m_imm_n","m_imm_p","m_pval",
  "w_nat_n","w_nat_p","w_imm_n","w_imm_p","w_pval")
rownames(t2) <- NULL
rm(t2_m,t2_f)
t2


# table 3 -----------------------------------------------------------------

## by origin country

i <- levels(data$origin)

### Africa

t3_af <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, origin %in% i[c(1,2)] & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease", 0:5), strata = "imm", data=df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0, sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

### The Americas

t3_am <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, origin %in% i[c(1,3)] & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease", 0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0, sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall=1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall=1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

### Asia and Oceania

t3_ap <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, origin %in% i[c(1,4)] & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease", 0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0, sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall=1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall=1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

## Eastern Europe

t3_ee <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, origin %in% i[c(1,5)] & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease", 0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0, sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

### Other Europe

t3_oe <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, origin %in% i[c(1,6)] & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease", 0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3 ))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0, sapply(1:6, function(x) attr(res,"pValues")[x, ifelse(fisher[x],2,1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

t3 <- cbind(t3_af,t3_am[3:5],t3_ap[3:5],t3_ee[3:5],t3_oe[3:5])
t3 <- cbind(rep(0:5, times = 6), t3)
t3 <- cbind(rep(paste0(seq(50,75,5),"-",seq(54,79,5)),e = 6), t3)
colnames(t3) <- c(
  "agegr","ncond","nat_n","nat_p","af_n","af_p","af_pval",
  "am_n","am_p","am_pval","as_n","as_p","as_pval",
  "ee_n","ee_p","ee_pval","oe_n","oe_p","oe_pval")
rownames(t3) <- NULL
rm(t3_af,t3_am,t3_ap,t3_ee,t3_oe,i)
t3


# table 4 -----------------------------------------------------------------

## receiving country
### Eastern Europe

t4_e <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, region == "E" & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease",0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0, sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x, nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x, nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5]<0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

### Northern Europe

t4_n <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, region == "N" & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars=paste0("ndisease",0:5), strata="imm", data=df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0, sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

### Southern Europe

t4_s <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, region == "S" & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease",0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p=c(0,sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

### Western Europe

t4_w <- Reduce("rbind", sapply(1:6, function(x){
  df <- subset(data, region == "W" & agegr == levels(data$agegr)[x])
  res <- CreateCatTable(vars = paste0("ndisease",0:5), strata = "imm", data = df)
  fisher <- suppressWarnings(sapply(attributes(res)$xtabs, function(x){
    length(which(chisq.test(x)$expected < 5))/length(x) > 0.2}))
  res <- cbind(
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[1],
    data.frame(print(res, printToggle = F, format = "f", pDigits = 3))[2],
    data.frame(print(res, printToggle = F, format = "p", pDigits = 3))[2],
    p = c(0, sapply(1:6, function(x) attr(res, "pValues")[x, ifelse(fisher[x], 2, 1)])))
  res <- res[2:7,]
  res[,2] <- as.numeric(res[,2])
  res[,4] <- as.numeric(res[,4])
  res[,2] <- sapply(res[,2], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,4] <- sapply(res[,4], function(x) paste0("(", format(x,nsmall = 1), ")"))
  res[,1] <- format(as.numeric(res[,1]), big.mark = ",")
  res[,3] <- format(as.numeric(res[,3]), big.mark = ",")
  res[,5] <- as.numeric(as.character(res[,5]))
  res[,5] <- ifelse(res[,5] == 0, "", ifelse(res[,5] < 0.001, "***", ifelse(
    res[,5] < 0.01, "**", ifelse(res[,5] < 0.05, "*", ""))))
  res[,5] <- ifelse(fisher, paste0(res[,5],"a"), res[,5])
  return(res)}, simplify = F))

t4 <- cbind(t4_e,t4_n,t4_s,t4_w)
t4 <- cbind(rep(0:5, times = 6), t4)
t4 <- cbind(rep(paste0(seq(50,75,5),"-",seq(54,79,5)), e = 6), t4)
colnames(t4) <- c(
  "agegr","ncond","e_nat_n","e_nat_p","e_imm_n","e_imm_p","e_pval",
  "n_nat_n","n_nat_p","n_imm_n","n_imm_p","n_pval",
  "s_nat_n","s_nat_p","s_imm_n","s_imm_p","s_pval",
  "w_nat_n","w_nat_p","w_imm_n","w_imm_p","w_pval")
rownames(t4) <- NULL
rm(t4_e,t4_n,t4_s,t4_w)
t4







