# Duke University Brain-Computer Interface (BCI) Dataset Development Project
# BCI2000 Data File Management

# Resources:
# https://www.bci2000.org
# https://www.bci2000.org/mediawiki/index.php/Technical_Reference:BCI2000_File_Format
# https://www.edfplus.info/specs/edf.html

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(digits.secs=4)
options(device=c("windows", "RStudioGD")[2])

library(readr)
library(ggplot2)
library(parallel)

setwd(c("C:/Projects/Duke/BCI/BCI2000",
        "C:/Users/tjb48/Projects/BCI2000",
        "/hpc/group/mainsahlab/BCIData/BCI-EDF-Transfer")[3])

########################################################################################################
# Function to convert a BCI200.dat file to its equivalent EDF file
# The parameter f is a vector containing:
# Destination ... directory in which to place output EDF file
# fileEDF ....... name od EDF file to be created 
# Source ........ directory of source BCI.dat file
# File .......... name of source BCI.dat file
# Study, Subject, Session, Sex, Race, Ethnicity, Age, ALSFRS, Amp
########################################################################################################

convertBCI <- function(bciInfo) {
 
  library(readr)
  source(functionSource)
  
  # Compose full-path source and target EDF file names
  fBCI <- paste(bciInfo["Source"], "/", bciInfo["File"], sep="")
  fEDF <- paste(bciInfo["Destination"], "/", bciInfo["fileEDF"], sep="")
  print(paste("BCI file: ", fBCI, sep=""))
  
  # Compose EDF patient and recording IDs
  patientID <- paste(bciInfo["Subject"]," ", bciInfo["Sex"], " ", bciInfo["Age"], " ",
                     bciInfo["Race"], "_", bciInfo["Ethnicity"], " ", bciInfo["ALSFRS"], sep="")
  recordingID <- paste("Startdate 01-JAN-2020", " ", bciInfo["Study2"], " ", bciInfo["Session"], " ",
                       bciInfo["Amp"], sep="")
  
  # Compose list of BCI data components
  print("Call:  bciRetrieve()")
  bcidat <- bciRetrieve(fBCI)
  
  # Convert BCI2000 data to EDF byte vector and write to specified file
  print("Call:  bciToEDF()")
  print(paste("EDF file: ", fEDF, sep=""))
  edfdat <- bciToEDF(bcidat, patientID=patientID, recordingID=recordingID)
  readr::write_file(edfdat, fEDF)
 
}

#####################################################################################################
# Configure function source and input/output directories
# Read project info sheets
#####################################################################################################

functionSource <- paste(getwd(), "/BCIFunctions.r", sep="")
dr0 <- c("//lcollins-00.fs.egr.duke.edu/lcollins-00/data/data", "/hpc/group/mainsahlab/BCIData")[2]
indir <- paste(dr0, c("/data/BCI", "/BCI2000Data")[2], sep="")
outdir <- paste(dr0, c("/BCIsubjectDatabase/EDF-FileStruct-2022-08-31", "/EDFDataWork")[2], sep="")

# Read BCI project info sheets, including source file names and subject demographic identifiers
bciInfo <- paste(dr0,
                 c("1"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch1v3_13_17_19.csv",
                   "2"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch2v3_8_20_23_29combined.csv",
                   "3"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch3v3_18_26_28.csv",
                   "4"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch3v4_11.csv",
                   "5"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch4v1.csv",
                   "6"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch5v1.csv",
                   "7"="/BCI-EDF-Transfer/DAT2EDF_Information_Batch6v3.csv",
                   "8"="/BCI-EDF-Transfer/DAT2EDF_Information_frye2011v1.csv",
                   "9"="/BCI-EDF-Transfer/DAT2EDF_Information_jonesFL.csv",
                   "10"="/BCI-EDF-Transfer/DAT2EDF_Information_jonesLT.csv",
                   "11"="/BCI-EDF-Transfer/DAT2EDF_Information_kellicut2018v2.csv",
                   "12"="/BCI-EDF-Transfer/DAT2EDF_Information_ryan2011.csv",
                   "13"="/BCI-EDF-Transfer/DAT2EDF_Information_ryan2017v1.csv"), sep="")
fcol <- c("Destination", "Source", "File", "Study", "Subject", "Session", "Type", "Condition", "Sex", "Race", "Ethnicity", "Age", "ALSFRS", "Amp")
fileBCI <- do.call(rbind, lapply(bciInfo, function(f) data.frame(read.table(f, header=T, sep=",")[,fcol], "infoSource"=f)))

# Omit duplicate entries (these should be reported - see "Data integrity diagnostics, below - prior to collapsing)
fileBCI <- fileBCI[which(!duplicated(fileBCI)),]
# Convert all backslashes in source to a forward slash
fileBCI[,"Source"] <- gsub("\\\\", "/", fileBCI[,"Source"])

# Convert source file names from Y: to full network path
fileBCI[,"Source"] <- paste(indir, "/", sub("Y:/data/BCI/", "", fileBCI[,"Source"], fixed=T), sep="")

# Convert all backslashes in destination to a forward slash
fileBCI[,"Destination"] <- gsub("\\\\", "/", fileBCI[,"Destination"])

# Strip leading ./ from desitnation
fileBCI[,"Destination"] <- sub("./", "", fileBCI[,"Destination"], fixed=T)

#####################################################################################################
# Compare Study, Subject, Session, Type (Mode), and Condition (Paradigm) to corresponding info sheet columns
# Although these descriptors generally appear in the source directory names, this has not been verified
# Also, parsing elements from source directory names is problematic due to an indeterminant number of subelement names 
# Comparison of stated directories is useful in verifying target directory names
#####################################################################################################

d <- strsplit(fileBCI[,"Destination"], "/")
# Verify uniform number of directory elements
table(unlist(lapply(d, length)), useNA="ifany")
# Convert to array
d <- tolower(do.call(rbind, d))
colnames(d) <- c("study", "subject", "session", "type", "condition")
which(d[,"study"]!=tolower(fileBCI[,"Study"]))
which(d[,"subject"]!=tolower(fileBCI[,"Subject"]))
k <- which(d[,"session"]!=tolower(fileBCI[,"Session"]))
cbind(fileBCI[k,c("Source", "Destination", "Session", "infoSource")])
k <- which(d[,"type"]!=tolower(fileBCI[,"Type"]))
cbind(fileBCI[k,c("Source", "Destination", "Type", "infoSource")])
which(d[,"condition"]!=tolower(fileBCI[,"Condition"]))

#####################################################################################################
# Modify info sheet entries according to rules supplied by BCI project team
#####################################################################################################

datasetVer <- "v1.0.0"

# Study
fileBCI[which(is.na(fileBCI[,"Study"]) | nchar(sub(" ", "", fileBCI[,"Study"]))==0),"Study"] <- "X"
table(fileBCI[,"Study"], useNA="ifany")
study <- tolower(c("data_2010fall_RC_CB_Random",
                   "data_2010summer_longitudinalALS_extended",
                   "data_2011fall_ERN",
                   "data_2012summer_dsLanguageModel",
                   "data_2013summer_DSLM_DictionaryCorrection",
                   "data_2013summer_Dslm_Als",
                   "data_2014spring_DS w NGram LM",
                   "data_2015fall_EyeGaze", 
                   "data_2016fall_CodebookSelectCBP",
                   "data_2016winter_CodebookSelect",
                   "data_2017summer_Adaptive",
                   "data_2012fall_RC_CB_CL_ALS",
                   "data_2022spring_CB_AdaptiveDiffuse",
                   "data_2014summer_AlsWetDry",
                   "Frye et. al 2011",
                   "Ryan et al. 2011",
                   "Ryan et al. 2017",
                   "Kellicut-Jones, Sellers 2018",
                   "Jones, Sellers 2019 - Face & Location",
                   "Jones, Sellers 2019 - Location & Tool"))
studyCode <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S1", "S2")
# Convert study to include data set version and study code
kstudy <- match(tolower(fileBCI[,"Study"]), study)
unique(fileBCI[which(is.na(kstudy)),"Study"])
fileBCI[,"Study2"] <- paste("bigP3BCI_", datasetVer, "_Study", studyCode[kstudy], sep="")
table(fileBCI[,"Study2"], useNA="ifany")
grep("NA", fileBCI[,"Study2"])
# Subject - convert empties and strip prefixes
fileBCI[which(is.na(fileBCI[,"Subject"]) | nchar(sub(" ", "", fileBCI[,"Subject"]))==0),"Subject"] <- "X"
fileBCI[,"Subject"] <- sub("subject", "", fileBCI[,"Subject"], ignore.case=T)
fileBCI[,"Subject"] <- sub("n", "", fileBCI[,"Subject"], ignore.case=T)
fileBCI[,"Subject"] <- paste(studyCode[kstudy], "_", fileBCI[,"Subject"], sep="")
table(fileBCI[,"Subject"], useNA="ifany")
grep("NA", fileBCI[,"Subject"])
# Session - convert empties and strip prefixes
fileBCI[which(is.na(fileBCI[,"Session"]) | nchar(sub(" ", "", fileBCI[,"Session"]))==0),"Session"] <- "X"
fileBCI[,"Session"] <- sub("session", "", fileBCI[,"Session"], ignore.case=T)
fileBCI[,"Session"] <- sub("se", "", fileBCI[,"Session"], ignore.case=T)
table(fileBCI[,"Session"], useNA="ifany")
# Type (mode)
fileBCI[which(is.na(fileBCI[,"Type"]) | nchar(sub(" ", "", fileBCI[,"Type"]))==0),"Type"] <- "X"
table(fileBCI[,"Type"], useNA="ifany")
# Condition (paradigm)
fileBCI[which(is.na(fileBCI[,"Condition"]) | nchar(sub(" ", "", fileBCI[,"Condition"]))==0),"Condition"] <- "X"
table(fileBCI[,"Condition"], useNA="ifany")
paradigm <- tolower(c("Checkerboard", "Random", "RowColumn", "Checkerboard_forERN",
                      "Checkerboard_forP300", "Checkerboard_Color", "DynamicStopping",
                      "LanguageModel", "Dynamic", "Static", "Bigram_LMCP", "Ngram_LMCP",
                      "Checkerboard_0_1_Gaze", "Checkerboard_1_0_Gaze", "Checkerboard_No_Gaze",
                      "Checkerboard_Real_Gaze", "PerformanceBasedParadigm", "Adaptive",
                      "AdaptiveDiffuse", "Wet", "Dry", "CBP", "SUP", "Face", "House", "Tool",
                      "CrystalizeLarge", "CrystalizeSmall", "EinsteinLarge", "EinsteinSmall",
                      "Nonpredictive Spelling", "Predictive Spelling", "Color Intensification",
                      "Grey-to-Color", "Grey-to-White"))
paradigmCode <- c("CB", "RD", "RC", "CBERN", "CB", "CBCol", "Dyn", "DynBigram", "Dyn",
                  "Static", "DynBigram", "DynNgram", "CBGaze01", "CBGaze10", "CBGazeNo",
                  "CBGazeReal", "PB", "AD", "ADdiff", "Wet", "Dry", "CBP", "SUP", "Face",
                  "House", "Tool", "CrystalizeLarge", "CrystalizeSmall", "EinsteinLarge",
                  "EinsteinSmall", "NonpredictiveSpelling", "PredictiveSpelling",
                  "ColorIntensification", "Grey-to-Color", "Grey-to-White")
which(is.na(match(tolower(fileBCI[,"Condition"]), paradigm)))
# Sex
fileBCI[which(tolower(fileBCI[,"Sex"])=="female" | fileBCI[,"Sex"]=="f"),"Sex"] <- "F"
fileBCI[which(tolower(fileBCI[,"Sex"])=="male" | fileBCI[,"Sex"]=="m"),"Sex"] <- "M"
fileBCI[which(is.na(fileBCI[,"Sex"]) | nchar(sub(" ", "", fileBCI[,"Sex"]))==0),"Sex"] <- "X"
table(fileBCI[,"Sex"], useNA="ifany")
# Age
patAge <- suppressWarnings(as.numeric(fileBCI[,"Age"]))
k <- which(!is.na(patAge))
fileBCI[k,"Age"] <- ifelse(patAge[k]<150, paste("01-JAN-", 2020-patAge[k], sep=""), "01-JAN-1020")
fileBCI[which(is.na(patAge)),"Age"] <- "01-JAN-2020"
table(fileBCI[,"Age"], useNA="ifany")
# Race
fileBCI[which(is.na(fileBCI[,"Race"]) | nchar(sub(" ", "", fileBCI[,"Race"]))==0),"Race"] <- "X"
# Replace spaces with dashes, to conform to EDF standard (where spaces are field delimiters)
fileBCI[,"Race"] <- gsub(" ", "-", fileBCI[,"Race"])
# Modify race to conform to standard codes
k <- which(fileBCI[,"Race"]=="Caucasian")
fileBCI[k,"Race"] <- "White"
k <- which(fileBCI[,"Race"]=="Indian")
fileBCI[k,"Race"] <- "Asian"
k <- which(fileBCI[,"Race"]=="Black-or-African-American")
fileBCI[k,"Race"] <- "Black"
table(fileBCI[,"Race"], useNA="ifany")
# Ethnicity
fileBCI[which(is.na(fileBCI[,"Ethnicity"]) | nchar(sub(" ", "", fileBCI[,"Ethnicity"]))==0),"Ethnicity"] <- "X"
# Replace spaces with dashes
fileBCI[,"Ethnicity"] <- gsub(" ", "-", fileBCI[,"Ethnicity"])
# Modify ethnicity to conform to standard codes
k <- which(fileBCI[,"Ethnicity"] %in% c("Not-Hispanic-or-Latino", "NonHispanic"))
fileBCI[k,"Ethnicity"] <- "NotHispanicLatino"
k <- which(fileBCI[,"Ethnicity"] %in% c("Hispanic", "Hispanic-or-Latino-of-any-race"))
fileBCI[k,"Ethnicity"] <- "HispanicLatino"
table(fileBCI[,"Ethnicity"], useNA="ifany")
# ALS level
patALSx <- suppressWarnings(as.numeric(fileBCI[,"ALSFRS"]))
k <- which(!is.na(patALSx))
fileBCI[k,"ALSFRS"] <- paste("ALS_", patALSx[k], sep="")
fileBCI[which(is.na(fileBCI[,"ALSFRS"]) | nchar(sub(" ", "", fileBCI[,"ALSFRS"]))==0),"ALSFRS"] <- "X"
table(fileBCI[,"ALSFRS"], useNA="ifany")
# Amp
fileBCI[which(is.na(fileBCI[,"Amp"]) | nchar(sub(" ", "", fileBCI[,"Amp"]))==0),"Amp"] <- "X"
table(fileBCI[,"Amp"], useNA="ifany")

#####################################################################################################
# Compose EDF directory and file names
#####################################################################################################

# Validate info sheet elements
which(!fileBCI[,"Study2"] %in% paste("bigP3BCI_", datasetVer, "_Study", studyCode, sep=""))
grep("[0-9][0-9]", fileBCI[,"Subject"], ignore.case=T, invert=T)
grep("[0-9][0-9][0-9]", fileBCI[,"Session"], ignore.case=T, invert=T)
which(!fileBCI[,"Type"] %in% c("Train", "Test"))
kparadigm <- match(tolower(fileBCI[,"Condition"]), paradigm)
which(is.na(kparadigm))
# Correct known erroneous file names
k <- which(fileBCI[,"Destination"]=="data_2010summer_longitudinalALS_extended/Subject04/Session005/Train/Checkerboard" &
           fileBCI[,"File"]=="long04_S005R0test.dat")
fileBCI[k,"File"] <- "long04_S005R00.dat"
k <- which(fileBCI[,"Destination"]=="/hpc/group/mainsahlab/BCIData/EDFDataWork/StudyM/M_04/SE001/Train/ADdiff" &
           fileBCI[,"File"]=="Subject_04_AdaptDiff_TrainS001R01.dat")
fileBCI[k,"fileEDF"] <- "M_04_SE001_ADdiff_Train05.edf"
# Extract Rxx.dat, where xx is a one or two digit integer
rxx <- rep(NA, nrow(fileBCI))
p1 <- regexpr("R[0-9]\\.dat", fileBCI[,"File"])
k1 <- which(p1>0)
p2 <- regexpr("R[0-9][0-9]\\.dat", fileBCI[,"File"])
k2 <- which(p2>0)
rxx[k1] <- sprintf("%02d", as.integer(substring(fileBCI[k1,"File"], p1[k1]+1, p1[k1]+1)))
rxx[k2] <- substring(fileBCI[k2,"File"], p2[k2]+1, p2[k2]+2)
which(is.na(rxx))
table(rxx, useNA="ifany")
# Compose destination directory with full network path
fileBCI[,"Destination"] <- paste(outdir, "/", "Study", studyCode[kstudy], "/", fileBCI[,"Subject"], "/",
                                 "SE", fileBCI[,"Session"], "/", fileBCI[,"Type"], "/",
                                 paradigmCode[kparadigm], sep="")
# Compose EDF file name
fileBCI[,"fileEDF"] <- paste(fileBCI[,"Subject"], "_", "SE", fileBCI[,"Session"], "_",
                             paradigmCode[kparadigm], "_", fileBCI[,"Type"], rxx, ".edf", sep="")

#####################################################################################################
# Convert BCI.dat files to corresponding EDF versions
#####################################################################################################

# Delete all directories and files in output dir
unlink(dir(outdir, recursive=T, include.dirs=T, full.names=T), recursive=T)

# Delete all target files
#for(f in paste(fileBCI[,"Destination"], "/", fileBCI[,"File"], sep=""))
#  unlink(f)

# Create destination directories
for(d in unique(fileBCI[,"Destination"]))
  dir.create(d, recursive=T)

# Delete log files,
logF <- dir(path="Log", pattern="Msg..\\.txt")
if(length(logF)>0)
  file.remove(paste("Log/", logF, sep=""))

# Execute
t0 <- proc.time()

# Configure cluster
ncl <- 40
cl <- makePSOCKcluster(ncl)

# Export objects to be used by parallel sub-processes
clusterExport(cl, c("fileBCI", "functionSource", "indir", "outdir", "convertBCI"))

# Assign log file names to sub-processes
parApply(cl, as.matrix(paste(getwd(), "/Log/Msg", sprintf("%02d", 1:ncl), ".txt", sep="")), 1, function(f) msgFile <<- f)

# Convert BCI files 
cvtResult <- do.call(
  rbind,
  parLapplyLB(
    cl, 1:nrow(fileBCI),
    function(i) {
      t0 <- proc.time()
      # Divert messages [all should result from print()]
      # Note that the file name in global environment for the current sub-process is used
      sink(msgFile, "output", append=T)
      # Trap warnings and errors
      # withCallingHandlers() resumes execution at the statement in f() that generates a warning
      # tryCatch() terminates after a warning in f()
      # invokeRestart("muffleWarning") prevents warnings from being returned to the caller (tryCatch)
      # Error conditions are returned from withCallingHandlers() to tryCatch(), but error conditions
      # are consumed (not passed upstream to caller) by tryCatch
      # Therefore, warnings are held at withCallingHandlers() and errors are held at tryCatch()
      tryCatch(
        withCallingHandlers(
          convertBCI(fileBCI[i,]),
          warning=function(msg) {
            print(msg[["message"]])
            invokeRestart("muffleWarning")
          },
          error=function(err) print(err[["message"]])
        ),
        error=function(err) {}
      )
      # Close message diversion file
      sink()
      return(data.frame("i"=i, "f"=fileBCI[i,"Source"], "t"=proc.time()["elapsed"]-t0["elapsed"]))
    }))

stopCluster(cl)

print(proc.time()-t0)

quit(save="no")

########################################################
# Review log files
########################################################

# Read log entries
z <- unlist(lapply(dir("Log", full.names=T), function(f) scan(f, what="character", sep="\n")))

# Index errors of interest
err <- c("error-00", "error-10", "error-[2-9]0")[1]
k0 <- grep(err, z, ignore.case=T)

# Omit "NA values returned" messages
#k0 <- intersect(grep(err, z, ignore.case=T), grep("NA", z, ignore.case=T, invert=T))

#grep("warn", z, ignore.case=T)

# Index file names
k1 <- grep("BCI file", z)

# Combine file names with error message
z2 <- t(apply(as.matrix(k0), 1, function(i) c(sub(indir, "", z[max(k1[which(k1<i)])]), z[i])))

# Parse file name from error message
writeLines(substring(z2[,1], regexpr("/", z2[,1])+1, nchar(z2[,1])-1))

# List messages surrounding error messages
k2 <- unlist(lapply(k0, function(m) m:m))
writeLines(z[k2])

########################################################
# Data integrity diagnostics
########################################################

# Check for duplicate lines in info sheets
x <- aggregate(1:nrow(fileBCI), by=fileBCI, length)
colnames(x) <- c(colnames(fileBCI), "n")
k <- which(x[,"n"]>1)
View(x[k,])
write.table(x[k,], "DuplicateInfoSheetEntries.csv", row.names=T, col.names=T, sep=",", quote=F)

# Check for duplicate BCI source file names
f <- paste(fileBCI[,"Source"], "/", fileBCI[,"File"], sep="")
x <- aggregate(1:length(f), by=list(f), length)
colnames(x) <- c("f", "n")
k <- which(x[,"n"]>1)
length(f)-length(k)
fileBCI[grep(x[k[1],"f"], f),]
writeLines(sub(indir, "", x[k,"f"]))

# Verify existence of specified source files
f <- dir(unique(fileBCI[,"Source"]), full.names=T)
x <- merge(data.frame("f"=paste(fileBCI[,"Source"], "/", fileBCI[,"File"], sep=""), "a"=1),
           data.frame("f"=f, "b"=1), all=T)
k <- which(is.na(x[,"b"]))
View(x[k,])
writeLines(sub("/hpc/group/mainsahlab/BCIData/BCI2000Data/", "", x[k,"f"]))

# Examine EDF file metadata
edfInfo <- file.info(dir(outdir, recursive=T, full.names=T, pattern="\\.edf"))

# Test existence of expected EDF files
edfDir <- dir(unique(fileBCI[,"Destination"]), full.names=T)
length(edfDir)
f <- paste(fileBCI[,"Destination"], "/", sub(".dat", ".edf", fileBCI[,"fileEDF"], fixed=T), sep="")
iedf <- match(f, edfDir)
sort(iedf)
c(nrow(fileBCI), length(iedf))
k <- which(is.na(iedf))
fileBCI[k[1],]
writeLines(sub(outdir, "", f[k]))

# Check for duplicate destination EDF file names
x <- aggregate(1:length(f), by=list(f), length)
colnames(x) <- c("f", "n")
k <- which(x[,"n"]>1)
fileBCI[grep(x[k[1],"f"], f),]
writeLines(sub(indir, "", x[k,"f"]))

#############################################################
# List source directories appearing in info sheets
# Copy these from project files (bat cave) to DCC source dir
#############################################################

x <- strsplit(fileBCI[,"Source"], "/")
sort(unique(unlist(lapply(x, function(a) paste("/", paste(a[2:7], collapse="/", sep=""), sep="")))))

########################################################
# Delete extraneous files (any lacking ".dat")
########################################################

# Beware!  It appears that the max depth of recursion is 10
# Files at greater depth may not be reported (in fact, directories at level 10 are
# returned instead of file names within those directories)
f <- dir(indir, full.names=T, recursive=T)
k <- which(apply(as.matrix(f), 1, function(a) substring(a, nchar(a)-3, nchar(a))!=".dat"))
View(data.frame(f[k]))
View(data.frame(f))
#file.remove(f[k])

