
# library -----------------------------------------------------------------

library(haven)
library(labelled)


# bring dat --------------------------------------------------------------

## mergeid variable from the cleaned data

id <- unique(read.csv("mypath/main.csv", header = T)$mergeid)

## "gv_allwaves_cv_r" raw data file

dat <- read_sav("mypath/sharewX_rel8-0-0_gv_allwaves_cv_r.sav")
dat <- dat[dat$mergeid %in% id,]
rm(id)


# clean data --------------------------------------------------------------

dat <- cbind(mergeid = dat$mergeid, 
              country = dat$country,
              dage = dat$deceased_age,
              dat[grepl("hhid\\d$", names(dat))],
              dat[grepl("interview_w\\d$", names(dat))],
              dat[grepl("deadoralive_w\\d$", names(dat))])

names(dat)[grepl("hhid", names(dat))] <- paste0("hhid_w",1:8)

dat <- rbind(
  cbind(dat[,1:3], wave=1, unname(dat[grep("w1$",names(dat))])),
  cbind(dat[,1:3], wave=2, unname(dat[grep("w2$",names(dat))])),
  cbind(dat[,1:3], wave=3, unname(dat[grep("w3$",names(dat))])),
  cbind(dat[,1:3], wave=4, unname(dat[grep("w4$",names(dat))])),
  cbind(dat[,1:3], wave=5, unname(dat[grep("w5$",names(dat))])),
  cbind(dat[,1:3], wave=6, unname(dat[grep("w6$",names(dat))])),
  cbind(dat[,1:3], wave=7, unname(dat[grep("w7$",names(dat))])),
  cbind(dat[,1:3], wave=8, unname(dat[grep("w8$",names(dat))])))

names(dat) <- c("mergeid","country","dage","wave","hhid","int","doa")

dat <- dat[dat$int != -91 & dat$doa != -91,] # not yet part of the sample
dat <- dat[dat$int != -99 & dat$doa != -99,] # country skipped this wave
dat <- dat[dat$int != -101,] # inactive longitudinal sample batches (AT w6)
dat <- dat[dat$int != -102,] # inactive longitudinal sample batches (GR w7)
dat <- dat[dat$int != -103,] # inactive longitudinal sample batches (PT w7)
dat <- dat[dat$int != -104,] # inactive longitudinal sample batches (SE w8c)
dat <- dat[dat$int != -105,] # inactive longitudinal sample batches (NL w8c)
dat$doa[dat$doa == -109] <- 0 # out of sample & contact deleted (DE)
dat <- dat[dat$doa != -101,] # inactive longitudinal sample batches (AT w6)
dat <- dat[dat$doa != -102,] # inactive longitudinal sample batches (GR w7)
dat <- dat[dat$doa != -103,] # inactive longitudinal sample batches (PT w7)
dat <- dat[dat$doa != -104,] # inactive longitudinal sample batches (SE w8c)
dat <- dat[dat$doa != -105,] # inactive longitudinal sample batches (NL w8c)
dat <- dat[order(dat$mergeid,-dat$wave),]
dat <- remove_labels(dat)
rownames(dat) <- NULL

head(dat)


# staying in panel & drop outs --------------------------------------------

dat$drop <- NA
dat$drop[!duplicated(dat$mergeid) & dat$int==-97] <- 1
dat$drop[dat$mergeid %in% subset(dat,!is.na(drop))$mergeid] <- 1
dat$drop[is.na(dat$drop) & !duplicated(dat$mergeid) & dat$int==-94] <- 2
dat$drop[dat$mergeid %in% subset(dat,!is.na(drop) & drop==2)$mergeid] <- 2
dat$drop[dat$mergeid %in% subset(dat,is.na(drop) & int==2)$mergeid] <- 2
dat$drop[dat$mergeid %in% subset(dat,is.na(drop) & doa==2)$mergeid] <- 2
dat$drop[is.na(dat$drop) & !duplicated(dat$mergeid) & dat$int==-92] <- 3
dat$drop[is.na(dat$drop) & !duplicated(dat$mergeid) & dat$int==-93] <- 3
dat$drop[is.na(dat$drop) & !duplicated(dat$mergeid) & dat$int==0] <- 3
dat$drop[dat$mergeid %in% subset(dat,!is.na(drop) & drop==3)$mergeid] <- 3
dat$drop[is.na(dat$drop)] <- 0
dat$drop <- factor(dat$drop, 0:3, labels=c("int","inelig","dead","non-int"))
dat <- dat[order(dat$mergeid,dat$wave),]
rownames(dat) <- NULL

write.csv(dat, "mypath/supplements/dropouts.csv", row.names = F)


