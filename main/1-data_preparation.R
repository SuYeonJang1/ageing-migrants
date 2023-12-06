
# path --------------------------------------------------------------------

## create "data" folder in your project
## under the "data" folder, create 8 wave-specific folders and name them "wave1"-"wave8"
## and also create 7 wave-specific imputation folders (except for wave 3) named "imputation1"-"imputation8"
## in the wave-specific folders (e.g., "data/wave1"), put raw data files for that wave
## in the wave-specific imputation folders (e.g., "data/imputation1"), put raw data for "gv_imputation"

path1 <- paste0("data/wave", c(1:8))
path2 <- paste0("data/imputations", c(1:2, 4:8))


# bring data --------------------------------------------------------------

v1 <- c("mergeid", "wave",   "country", "gender",   "int_year", "interview",
        "age_int", "mn024_", "mn101_",  "mn103_",   "dn004_",   "dn005c", 
        "dn006_",  "dn007_", "dn014_", "dn044_",    "dn503_",   "isced1997_r", 
        "yrbirth", "ep005_", paste0("ph006d",1:21), paste0("ph011d",1:15), 
        paste0("ph008d",c(1:22,"ot")), paste0("sl_ac006_",1:29), 
        paste0("sl_ac013_",1:29), "sl_ac014c_1_iso", "br001_", "br002_",
        "br005d1", "br006_", "br011_","br012_","br013_","br010_","br019_",
        "br039_","br040_","br015_","hhsize")

v2 <- c("mergeid", "country", "implicat", "thinc")

wave_Table <- function(path, variable){
  require(haven, quietly = TRUE)
  list <- lapply(path, function(x) paste0(x, "/", list.files(x)))
  wave <- lapply(list, function(x) lapply(x, function(y) 
    read_sav(y, col_select = any_of(variable))))
  wave.merge <- lapply(wave, function(z){
    res <- Reduce(function(x,y) merge(x, y, all=TRUE), z)})
  return(wave.merge)
}

share <- wave_Table(path1,v1)
share <- mapply(cbind, share, "wave" = c(1:8), SIMPLIFY=FALSE)
share <- lapply(share, function(x){
  data <- data.frame(
    sapply(x, function(d){
      attributes(d) <- NULL
      return(d)
    }, simplify=F))
  v <- setNames(rep(list(NA), length(v1) - ncol(data)), 
                setdiff(v1, names(data)))
  data <- cbind(data, v)
  row.names(data) <- NULL
  data <- data[v1]
  return(data)
})

share_imp <- wave_Table(path2, v2)
share_imp <- mapply(cbind, share_imp, "wave" = c(1:2, 4:8), SIMPLIFY = FALSE)

share_hhsize <- lapply(share[c(1:2,4:8)], function(x){
  data <- x
  data[c("mergeid","hhsize")]})
share_imp <- lapply(1:7, function(x){
  merge(share_imp[[x]], share_hhsize[[x]], by = "mergeid", all.x = TRUE)})

share_imp <- lapply(share_imp, function(x){
  data <- data.frame(
    sapply(x, function(d){
      attributes(d) <- NULL
      return(d)
    }, simplify = F))
  income <- cbind(sapply(1:5, function(x){
    sub <- data[data$implicat == x,]
    inc <- sub$thinc/sqrt(sub$hhsize)
    }))
  income <- rowSums(income)/5
  data <- data[data$implicat == 1, c("mergeid","wave","country")]
  data <- cbind(data, thinc = income)
  row.names(data) <- NULL
  incomedat <- lapply(unique(data$country), function(c){
    codat <- data[data$country == c, ]
    q <- quantile(codat["thinc"], probs = seq(0, 1, 1/3), na.rm = TRUE)
    income <- cbind(codat, list("l" = q[2], "m" = q[3], "h" = q[4]))})
  res <- Reduce("rbind", incomedat)
  res$income <- with(res, ifelse(thinc <= l, 1, ifelse(thinc <= m, 2, 3)))
  return(res[c("mergeid","wave","income")])
})

share_imp <- c(share_imp[1:2], list(NULL), share_imp[3:7])
share_imp[[3]] <- data.frame(matrix(nrow = nrow(share[[3]]), ncol = 3))
share_imp[[3]][1] <- share[[3]][1]
share_imp[[3]][2] <- 3
names(share_imp[[3]]) <- names(share_imp[[1]])
rm(path1, path2, v1, v2, wave_Table, share_hhsize)


# id and waves ------------------------------------------------------------

id <- Reduce("rbind", lapply(share, function(x) x[c("mergeid", "wave")]))
id <- id[order(id$mergeid, id$wave), ]
id$first <- with(id, wave[!duplicated(mergeid)][cumsum(!duplicated(mergeid))])
id <- id[c("mergeid", "wave", "first")]
row.names(id) <- NULL


# number of chronic conditions --------------------------------------------

share <- lapply(share, function(x){
  data <- x
  i1 <- grep("^ph\\w", names(data))
  data[i1][is.na(data[i1]) | data[i1] < 0] <- 0
  data$mn103_ <- ifelse(!is.na(data$mn103_) & data$mn103_==1, 1, 0)
  nd <- matrix(data = NA, ncol = 6, nrow = nrow(data))
  nd[,1] <- rowSums(data[paste0(c("ph006d","ph011d"),rep(1:4,e=2))]==1) > 0
  nd[,2] <- rowSums(data[c("ph006d5","ph011d6")]==1) > 0
  nd[,3] <- rowSums(data[c("ph006d6","ph006d7","ph011d5","ph011d14")]==1) > 0
  nd[,4] <- rowSums(data[c("ph006d8","ph006d19","ph006d20")]==1) > 0
  nd[,5] <- rowSums(data[c("ph006d9","ph006d14","ph011d11","ph011d12")]==1) > 0
  nd[,6] <- rowSums(data[c("ph006d18","ph011d9","ph011d10")]==1) > 0
  nd <- cbind(nd,data[,c("ph006d11","ph006d12","ph006d10")]==1)
  nd2 <- cbind(nd,data$ph006d16==1)
  nd3 <- cbind(nd[,1:(ncol(nd)-1)],data[grep("^ph008",names(data))]==1)
  nd4 <- cbind(data[paste0("ph006d",c(1:5,11:12,14))]==1,nd[,4],
               rowSums(data[c("ph006d6","ph006d7")]==1)>0,data$ph006d10==1)
  nd5 <- cbind(nd4,data$ph006d16==1)
  nd6 <- cbind(nd4[,1:(ncol(nd4)-1)],data[grep("^ph008",names(data))]==1)
  data$ncond1 <- rowSums(nd)
  data$ncond2 <- ifelse(data$wave==1,NA,rowSums(nd2))
  data$ncond3 <- ifelse(data$mn103_==1,NA,rowSums(nd3))
  data$ncond4 <- rowSums(nd4)
  data$ncond5 <- ifelse(data$wave==1,NA,rowSums(nd5))
  data$ncond6 <- ifelse(data$mn103_==1,NA,rowSums(nd6))
  data <- data[-i1]
  return(data)
})


# employment --------------------------------------------------------------

share <- lapply(share, function(x){
  data <- x
  data$ep005_[is.na(data$ep005_) & !is.na(data$mn024_) & data$mn024_==2] <- 99
  data$ep005_[!is.na(data$ep005_) & data$ep005_ < 0] <- NA
  return(data)
})


# immigrant status --------------------------------------------------------

## wave 3

share <- sapply(1:8, function(x){
  dat <- merge(share[[x]], id, by=c("mergeid","wave"), all.x=T)
  dat1 <- dat[!is.na(dat$wave),]
  if(mean(dat1$wave)==3){
    dat1$dn004_ <- with(dat1, ifelse(first==3, sl_ac013_1, NA))
    dat1$dn005c <- with(dat1, ifelse(first==3, sl_ac014c_1_iso, NA))
    dat1$dn006_ <- ifelse(
      !is.na(dat1$sl_ac013_1) & dat1$sl_ac013_1 == 1, NA, ifelse(
        dat1$first != 3, NA, apply(sapply(1:29, function(x){
          ac006 <- dat1[grep("ac006",names(dat1))]
          ac013 <- dat1[grep("ac013",names(dat1))]
          ifelse(!is.na(ac013[,x]) & ac013[,x] == 1, ac006[,x], NA)
        }), 1, function(y)ifelse(all(is.na(y)), NA, min(y, na.rm = T)))))
  } 
  dat1[-grep("^sl",names(dat1))]
}, simplify=F)

share <- sapply(1:8, function(x){
  dat <- merge(share[[x]], share_imp[[x]], by=c("mergeid","wave"), all.x=T)
  return(dat)
}, simplify=FALSE)

rm(share_imp)

## ever coded as immigrants

fill_down <- function(v1, id){
  keep <- !(duplicated(id) & is.na(v1))
  v1[keep][cumsum(keep)]
}

dat1 <- Reduce("rbind", share)
dat1 <- dat1[order(dat1$mergeid, dat1$wave),]
dat1$dn004_1 <- dat1$dn004_
i1 <- which(names(dat1) %in% c("dn004_1","dn005c","dn006_"))
dat1[i1] <- apply(dat1[i1], 2, function(x) ifelse(!is.na(x) & x < 0, NA, x))
dat1[i1] <- apply(dat1[i1], 2, function(x) fill_down(x, dat1$mergeid))
dat1 <- dat1[order(dat1$mergeid, -dat1$wave),]
dat1[i1] <- apply(dat1[i1], 2, function(x) fill_down(x, dat1$mergeid))
dat1$dn004_1[dat1$dn004_1 == 1 & !is.na(dat1$dn005c)] <- 5
dat1 <- dat1[order(dat1$mergeid, dat1$wave),]
id <- merge(id, dat1[c("mergeid","wave","interview")])
dat1 <- dat1[dat1$wave %in% c(1:2,4:8),]
dat1 <- dat1[dat1$interview == 1,]
rownames(dat1) <- NULL
rm(share, i1)


# education ---------------------------------------------------------------

dat1$isced1997_r[duplicated(dat1$mergeid)] <- NA
dat1$isced1997_r[!is.na(dat1$isced1997_r) & dat1$isced1997_r < 0] <- NA
dat1$isced1997_r[!is.na(dat1$isced1997_r) & dat1$isced1997_r > 6] <- NA
dat1$isced1997_r <- fill_down(dat1$isced1997_r, dat1$mergeid)


# marital status ----------------------------------------------------------

## current marital status

fill_down <- function(v1, v2, id){
  keep <- !(duplicated(id) & is.na(v1) & !is.na(v2) & v2 == 5)
  v1[keep][cumsum(keep)]
}

## change in marital status

fill_down2 <- function(v1, v2, id){
  keep <- which(!(duplicated(id) & is.na(v1)))
  keep <- sapply(keep, function(x){
    ifelse(!(!is.na(v2[x]) & v2[x] %in% c(1, 5)), x, ifelse(
      v2[x] == 1 & is.na(v1[x]) & x < length(keep) & id[x] == id[x+1], x+1, x))})
  keep <- unique(keep)[order(unique(keep))]
  v1[keep][cumsum(1:length(v1) %in% keep)]
}

dat1$dn014_[!is.na(dat1$dn014_) & dat1$dn014_ < 0] <- NA
dat1$dn044_[!is.na(dat1$dn044_) & dat1$dn044_ < 0] <- NA
dat1$dn044_[is.na(dat1$dn044_)] <- 5

dat1$dn014_ <- fill_down(dat1$dn014_, dat1$dn044_, dat1$mergeid)
dat1 <- dat1[order(dat1$mergeid, -dat1$wave),]
dat1$dn014_ <- fill_down2(dat1$dn014_, dat1$dn044_, dat1$mergeid)

dat1 <- dat1[order(dat1$mergeid, dat1$wave),]
row.names(dat1) <- NULL
rm(fill_down,fill_down2)


# smoking ----------------------------------------------------------------

fill_down <- function(v1, id){
  keep <- !(duplicated(id) & is.na(v1))
  v1[keep][cumsum(keep)]
}

fill_down2 <- function(v1, id){
  keep <- !(duplicated(id) & is.na(v1))
  res <- v1[keep][cumsum(keep)]
  res <- sapply(
    1:length(id), function(x){ 
      ifelse((is.na(v1[x])) & (res[x] == 5), NA, res[x]) })
  v1[keep][cumsum(keep)]
}

fill_down3 <- function(v1, id){
  keep <- !(duplicated(id) & is.na(v1))
  res <- v1[keep][cumsum(keep)]
  res <- sapply(
    1:length(id), function(x){ 
      ifelse((is.na(v1[x])) & (res[x] == 0), NA, res[x]) })
  v1[keep][cumsum(keep)]
}

dat1$br001_ <- ifelse(!is.na(dat1$br001_) & dat1$br001_ < 0, NA, dat1$br001_)
dat1$br002_ <- ifelse(!is.na(dat1$br002_) & dat1$br002_ < 0, NA, dat1$br002_)
dat1$br005d1 <- ifelse(!is.na(dat1$br005d1) & dat1$br005d1 < 0, NA, dat1$br005d1)
dat1$br006_ <- ifelse(!is.na(dat1$br006_) & dat1$br006_ < 0, NA, dat1$br006_)
dat1$br006_2 <- ifelse(is.na(dat1$br006_), NA, ifelse((dat1$br006_ > 0), 1, 0))

dat1 <- dat1[order(dat1$mergeid, -dat1$wave),]
dat1$br001_2 <- fill_down(dat1$br001_, dat1$mergeid)
dat1$br005d1_2 <- fill_down(dat1$br005d1, dat1$mergeid)
dat1$br006_3 <- fill_down(dat1$br006_2, dat1$mergeid)
dat1$br002_2 <- fill_down(dat1$br002_, dat1$mergeid)

dat1 <- dat1[order(dat1$mergeid, dat1$wave),]
dat1$br001_3 <- fill_down2(dat1$br001_2, dat1$mergeid)
dat1$br005d1_3 <- fill_down3(dat1$br005d1_2, dat1$mergeid)

dat1$evsmoke <- ifelse(
  (!is.na(dat1$br001_3)) & (dat1$br001_3 == 5), 0, ifelse( 
    (!is.na(dat1$br005d1_3)) & (dat1$br005d1_3 == 0), 0, ifelse(
      (!is.na(dat1$br001_3)) & (dat1$br001_3 == 1), 1, ifelse(
        (!is.na(dat1$br005d1_3)) & (dat1$br005d1_3 == 1), 1, NA))))

dat1$crsmoke <- ifelse(
  (!is.na(dat1$br002_2)) & (dat1$br002_2 == 5), 0, ifelse( 
    (!is.na(dat1$br006_3)) & (dat1$br006_3 == 0), 0, ifelse(
      (!is.na(dat1$br002_2)) & (dat1$br002_2 == 1), 1, ifelse(
        (!is.na(dat1$br006_3)) & (dat1$br006_3 == 1), 1, NA))))

dat1$smoke <- with(dat1, ifelse(
  !is.na(evsmoke) & evsmoke==0, 0, ifelse(
    !is.na(crsmoke) & crsmoke==0, 0, ifelse(
      !is.na(crsmoke) & crsmoke==1, 1, NA))))
  
dat1 <- dat1[order(dat1$mergeid, -dat1$wave),]
dat1$smoke <- fill_down(dat1$smoke, dat1$mergeid)

dat1 <- dat1[order(dat1$mergeid, dat1$wave),]
row.names(dat1) <- NULL
rm(fill_down, fill_down2, fill_down3)


# alcohol -----------------------------------------------------------------

fill_down <- function(v1, id){
  keep <- !(duplicated(id) & is.na(v1))
  v1[keep][cumsum(keep)]
}

dat1$br011_ <- ifelse(!is.na(dat1$br011_) & dat1$br011_ < 0, NA, dat1$br011_)
dat1$br012_ <- ifelse(!is.na(dat1$br012_) & dat1$br012_ < 0, NA, dat1$br012_)
dat1$br013_ <- ifelse(!is.na(dat1$br013_) & dat1$br013_ < 0, NA, dat1$br013_)
dat1$br010_ <- ifelse(!is.na(dat1$br010_) & dat1$br010_ < 0, NA, dat1$br010_)
dat1$br019_ <- ifelse(!is.na(dat1$br019_) & dat1$br019_ < 0, NA, dat1$br019_)
dat1$br039_ <- ifelse(!is.na(dat1$br039_) & dat1$br039_ < 0, NA, dat1$br039_)
dat1$br040_ <- ifelse(!is.na(dat1$br040_) & dat1$br040_ < 0, NA, dat1$br040_)

dat1$alcohol <- with(dat1, ifelse(
  wave == 1 & (is.na(br011_) & is.na(br012_) & is.na(br013_)), NA, ifelse(
    wave==1 & (!is.na(br011_) & br011_==1), 1, ifelse(
      wave == 1 & (!is.na(br012_) & br012_==1), 1, ifelse(
        wave == 1 & (!is.na(br013_) & br013_==1), 1, ifelse(
          wave==1, 0, NA))))))

dat1$alcohol <- ifelse(!is.na(dat1$br010_) & dat1$br010_ == 7, 0, dat1$alcohol)
dat1$alcohol <- with(dat1, ifelse(
  wave %in% 2:5 & is.na(br010_), NA, ifelse(
    wave %in% 2:5 & ((br010_==1 & !is.na(br019_) & (br019_ > 2)) |
                       (br010_==2 & !is.na(br019_) & (br019_ > 2.8)) |
                       (br010_==3 & !is.na(br019_) & (br019_ > 4.7)) |
                       (br010_==4 & !is.na(br019_) & (br019_ > 14)) |
                       (br010_==5 & !is.na(br019_) & (br019_ > 60))),
    1, ifelse(wave %in% 2:5, 0, dat1$alcohol))))

dat1$br040_ <- ifelse(!is.na(dat1$br039_) & (dat1$br039_ == 5), 0, dat1$br040_)
dat1$alcohol <- with(dat1, ifelse(
  wave %in% 6:8 & !is.na(br040_) & (br040_ > 14), 1, ifelse(
    wave %in% 6:8 & is.na(br040_), NA, ifelse(
      wave %in% 6:8, 0, dat1$alcohol))))

dat1 <- dat1[order(dat1$mergeid, -dat1$wave),]
dat1$alcohol <- fill_down(dat1$alcohol, dat1$mergeid)

dat1 <- dat1[order(dat1$mergeid, dat1$wave),]
row.names(dat1) <- NULL


# physical activity -------------------------------------------------------

dat1$br015_ <- ifelse(!is.na(dat1$br015_) & dat1$br015_ < 0, NA, dat1$br015_)
dat1$pa <- ifelse(is.na(dat1$br015_), NA, ifelse(dat1$br015_==4, 0, 1))
dat1 <- dat1[order(dat1$mergeid, -dat1$wave),]
dat1$pa <- fill_down(dat1$pa, dat1$mergeid)
dat1 <- dat1[order(dat1$mergeid, dat1$wave),]
row.names(dat1) <- NULL
rm(fill_down)


# immigrant-related variables ---------------------------------------------

dat1[, 3:ncol(dat1)] <- apply(
  dat1[, 3:ncol(dat1)], 2, function(x)ifelse(!is.na(x) & x < 0, NA, x))
dat1$imm <- ifelse(is.na(dat1$dn004_1), NA, ifelse(dat1$dn004_1 == 5, 1, 0))
dat1$origin <- NA
dat1$origin[!is.na(dat1$imm)&dat1$imm==0] <- "Native"
dat1$origin[is.na(dat1$origin) & dat1$dn005c %in% c(
  31, 51, 112, 233, 268, 398, 417, 428, 440, 498, 643, 762, 
  795, 804, 810, 860)] <- "USSR"
dat1$origin[is.na(dat1$origin) & dat1$dn005c %in% c(
  2, 12, 24, 72, 86, 108, 120, 132, 140, 148, 174, 175, 178, 
  180, 204, 226, 230, 231, 232, 260, 262, 266, 270, 288, 324, 
  384, 404, 426, 430, 434, 450, 454, 466, 478, 480, 504, 508, 
  516, 562, 566, 624, 638, 646, 654, 678, 686, 690, 694, 706, 
  710, 716, 728, 729, 732, 748, 768, 788, 800, 818, 834, 854, 
  894, 1010, 1095)] <- "Africa"
dat1$origin[is.na(dat1$origin) & dat1$dn005c %in% c(
  4, 16, 31, 36, 48, 50, 51, 64, 90, 96, 104, 116, 128, 144,
  156, 158, 162, 166, 184, 242, 258, 268, 275, 296, 316, 334,
  344, 356, 360, 364, 368, 376, 392, 396, 398, 400, 408, 410,
  414, 417, 418, 422, 446, 458, 462, 488, 496, 512, 520, 524, 
  536, 540, 548, 554, 570, 574, 580, 581, 582, 583, 584, 585, 
  586, 598, 608, 612, 626, 634, 682, 698, 702, 704, 714, 720, 
  760, 762, 764, 772, 776, 784, 792, 795, 798, 849, 860, 872, 
  876, 882, 886, 887, 1024, 1025, 1050, 1060, 1080, 
  1090, 1104)] <- "AsiaPacific"
dat1$origin[is.na(dat1$origin) & dat1$dn005c %in% c(
  208, 233, 234, 246, 248, 352, 372, 428, 440, 578, 744, 752, 
  826, 830, 831, 832, 833)] <- "North"
dat1$origin[is.na(dat1$origin) & dat1$dn005c %in% c(
  100, 112, 200, 203, 278, 348, 498, 616, 642, 643, 703, 804, 
  810, 1031, 1070, 1100, 1103)] <- "East"
dat1$origin[is.na(dat1$origin) & dat1$dn005c %in% c(
  40, 56, 249, 250, 276, 280, 438, 442, 492, 528, 756, 1030, 
  1101, 1130)] <- "West"
dat1$origin[is.na(dat1$origin) & dat1$dn005c %in% c(
  8, 20, 70, 191, 196, 292, 300, 336, 380, 470, 499, 620, 674, 
  688, 705, 724, 807, 890, 891, 1020, 1040)] <- "South"
dat1$origin[is.na(dat1$origin) & dat1$dn005c %in% c(
  60, 124, 304, 666, 840)] <- "NorthAmerica"
dat1$origin[is.na(dat1$origin) & dat1$dn005c %in% c(
  5, 28, 32, 44, 52, 68, 74, 76, 84, 92, 136, 152, 170, 188, 
  192, 212, 214, 218, 222, 238, 239, 254, 308, 312, 320, 328, 
  332, 340, 388, 474, 484, 500, 530, 531, 532, 533, 534, 535, 
  558, 590, 591, 594, 600, 604, 630, 658, 659, 660, 662, 663, 
  670, 740, 780, 796, 850, 858, 862)] <- "Latin"
dat1$origin[dat1$origin %in% c("NorthAmerica", "Latin")] <- "Americas"
dat1$origin[dat1$origin %in% c("North", "West", "South")] <- "OtherEurope"
dat1$origin[dat1$origin %in% c("East", "USSR")] <- "EastEurope"


# other variables ---------------------------------------------------------

dat1$age <- dat1$age_int
dat1$woman <- ifelse(dat1$gender == 2, 1, 0)
dat1$region <- NA
dat1$region[dat1$country %in% c(13,18,30,55)] <- "N"
dat1$region[dat1$country %in% c(11,12,14,17,20,23,31)] <- "W"
dat1$region[dat1$country %in% c(15,16,19,33,34,47,53,59)] <- "S"
dat1$region[dat1$country %in% c(28,29,32,35,48,51,57,61,63)] <- "E"
dat1$region[dat1$country==25] <- "israel"
dat1$marry <- ifelse(is.na(dat1$dn014_), NA, ifelse(dat1$dn014_ %in% 1:3, 1, 0))
dat1$edu <- dat1$isced1997_r
dat1$edu <- with(dat1, ifelse(
  is.na(edu), NA, ifelse(edu %in% 0:2, 1, ifelse(edu %in% 3:4, 2, 3))))
dat1$employed <- with(dat1, ifelse(is.na(ep005_), NA, ifelse(ep005_ == 2, 1, 0)))
dat1$cohort <- with(dat1, ifelse(!is.na(imm) & imm==0, "Native", dn006_))
dat1$year <- dat1$int_year
dat1[c("origin","cohort")] <- apply(
  dat1[c("origin","cohort")], 2, function(x){
    id <- unique(dat1$mergeid)[apply(table(dat1$mergeid, x)>0, 1, sum) > 1]
    x[dat1$mergeid %in% id] <- NA
    return(x)})

dat1 <- dat1[c(
  "mergeid", "wave", "year", "age", "woman", "imm", "country", 
  "origin", "region", "edu", "income", "employed", "marry", "ncond1", 
  "cohort", "yrbirth", "smoke","evsmoke","alcohol","pa")]

id <- id[id$interview==1,][c("mergeid","wave")]
dat1 <- merge(id, dat1, all.x = TRUE)
dat1$nwave <- unlist(sapply(table(dat1$mergeid), function(x)1:x))


# exclusion ---------------------------------------------------------------

i1 <- dat1$wave %in% c(1:2, 4:8)
i2 <- !is.na(dat1$age) & dat1$age >= 50
i3 <- !is.na(dat1$age) & dat1$age < 80
i4 <- !is.na(dat1$region) & dat1$region!="israel"
exclusion1 <- Reduce("rbind",sapply(
  list(TRUE,i1,i1&i2,i1&i2&i3,i1&i2&i3&i4), function(x){
    v1 <- nrow(dat1[x,])
    v2 <- length(unique(dat1$mergeid[x]))
    res <- cbind(v1,v2)
    names(res) <- c("v1","v2")
    return(res)},simplify = F))
rownames(exclusion1) <- c("all","wave3","age50","age80","region")
dat2 <- dat1[i1&i2&i3&i4,]
rm(i1,i2,i3,i4)

exclusion2 <- Reduce("rbind",sapply(
  1:which(names(dat2)=="ncond1"), function(x){
    cond <- apply(!is.na(dat2[1:x]),1,all)
    v1 <- length(dat2$mergeid[cond])
    v2 <- length(unique(dat2$mergeid[cond]))
    res <- cbind("person-years"=v1,"persons"=v2)
    return(res)}, simplify = F))
rownames(exclusion2) <- names(dat2)[1:which(names(dat2)=="ncond1")]
exclusion <- rbind(exclusion1, exclusion2)
exclusion <- cbind(row.names(exclusion), exclusion)
colnames(exclusion) <- c("v","person-years","persons")
rownames(exclusion) <- NULL
rm(exclusion1, exclusion2)
exclusion

main <- dat2[apply(!is.na(dat2[,1:which(names(dat2)=="ncond1")]),1,all),]
main <- main[order(main$mergeid,main$wave),]
base <- main[!duplicated(main$mergeid),]
row.names(base) <- NULL
rm(dat1,dat2,id)


# export ------------------------------------------------------------------

write.csv(base, "mypath/baseline.csv", row.names = F)
write.csv(main, "mypath/main.csv", row.names = F)
write.csv(exclusion, "mypath/exclusion.csv", row.names = F)

