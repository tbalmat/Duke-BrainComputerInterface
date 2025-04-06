# Duke university BCI Project
# Demographic data review

library(readr)

dr0 <- c("//lcollins-00.fs.egr.duke.edu/lcollins-00/data/data", "/hpc/group/mainsahlab/BCIData")[2]
indir <- paste(dr0, c("/data/BCI", "/BCI2000Data")[2], sep="")
outdir <- paste(dr0, c("/BCIsubjectDatabase/EDF-FileStruct-2022-08-31", "/EDFData")[2], sep="")
bciInfo <- paste(dr0,
                 c("1"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch1v3_13_17_19.csv",
                   "2"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch2v3_8_20_23_29combined.csv",
                   "3"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch3v3_18_26_28.csv",
                   "4"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch3v4_11.csv",
                   "5"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch4v1.csv",
                   "6"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch5v1.csv"), sep="")
fcol <- c("Destination", "Source", "File", "Study", "Subject", "Session", "Sex", "Race", "Ethnicity", "Age", "ALSFRS", "Amp")
fileBCI <- do.call(rbind, lapply(bciInfo, function(f) read.table(f, header=T, sep=",")[,fcol]))
fileBCI[,"Source"] <- gsub("\\\\", "/", fileBCI[,"Source"])
fileBCI[,"Source"] <- paste(indir, "/", sub("Y:/data/BCI/", "", fileBCI[,"Source"], fixed=T), sep="")
fileBCI[,"Destination"] <- gsub("\\\\", "/", fileBCI[,"Destination"])
fileBCI[,"Destination"] <- paste(outdir, "/", sub("./", "", fileBCI[,"Destination"], fixed=T), sep="")

# Age
table(fileBCI[,"Age"])
# Inspect a file
i <- 3763
x <- readr::read_file_raw(paste(fileBCI[i,"Destination"], "/", sub(".dat", ".edf", fileBCI[i,"File"], fixed=T), sep=""))
rawToChar(x[1:150])
# Design encoding rule
as.numeric(" ")
as.numeric("X")
patAge <- suppressWarnings(as.numeric(fileBCI[,"Age"]))
patAge[which(is.na(patAge))] <- 0
patAge[which(patAge>1000)] <- 10000
table(patAge, useNA="ifany")
patAgeYear <- paste("01-JAN-", 2020-patAge, sep="")
table(patAgeYear, useNA="ifany")
paste("01-JAN-", max(2020-patAge[sample(1:length(patAge), 1)], 1000), sep="")

# ALSFRS
patALS <- fileBCI[,"ALSFRS"]
table(patALS, useNA="ifany")
patALSx <- suppressWarnings(as.numeric(patALS))
table(patALSx, useNA="ifany")
patALS[which(!is.na(patALSx))] <- paste("ALS_", patALSx[which(!is.na(patALSx))], sep="")
table(patALS, useNA="ifany")

# Untransformed IDs
table(fileBCI[,"Subject"], useNA="ifany")
table(fileBCI[,"Sex"], useNA="ifany")
table(fileBCI[,"Race"], useNA="ifany")
table(fileBCI[,"Ethnicity"], useNA="ifany")

i <- sample(1:nrow(fileBCI), 1)
