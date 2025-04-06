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
options(device=c("windows", "RStudioGD")[1])

library(readr)
library(ggplot2)
library(parallel)

setwd(c("C:/Projects/Duke/BCI", "C:/Users/tjb48/Projects")[1])

source("BCI2000/BCIFunctions.r")

########################################################################################################
# Convert and evaluate lab files
# i is the row in the BCI info file to convert
########################################################################################################

convertBCI <- function(i) {

  library(readr)
  source(functionSource)

  t0 <- proc.time()

  # Retrieve file name components
  fBCI <- paste(sub("Y:\\data\\BCI", indir, fileBCI[i,"Source"], fixed=T), "/", fileBCI[i,"File"], sep="")
  fEDF <- paste(outdir, "/", sub(".\\", "", fileBCI[i,"Destination"], fixed=T), "/", sub(".dat", ".edf", fileBCI[i,"File"], fixed=T), sep="")
  print(paste(i, ", file:  ", fBCI, sep=""))
  
  # Compose EDF patient and recording IDs
  patID <- fileBCI[i,"Subject"]
  patSex <- fileBCI[i,"Sex"]
  patAge <- suppressWarnings(as.numeric(fileBCI[i,"Age"]))
  if(is.na(patAge))
    patAge <- 0
  patRace <- paste(fileBCI[i,"Race"], "_", fileBCI[i,"Ethnicity"], sep="")
  patALS <- fileBCI[i,"ALSFRS"]
  patientID <- paste(patID," ", patSex, " 01-JAN-", 2020-patAge, " ", patRace, "_", patALS, sep="")
  studyID <- fileBCI[i,"Study"]
  sessionID <-  fileBCI[i,"Session"]
  recEEGAmpModel <- fileBCI[i,"Amp"]
  recordingID <- paste("Startdate 01-JAN-2020", " ", studyID, " ", sessionID, " ", recEEGAmpModel, sep="")

  # Retrieve BCI file
  tryCatch(
    withCallingHandlers(
      {
        print("Call:  bciRetrieve()")
        bcidat <<- bciRetrieve(fBCI)
    
        # Select BCI recording start date and time parameters to be used in EDF file
        # At present, a constant date of 2020-0101 and time of 00:00:00 is used
        #datePar <- c("Storage:Documentation:BCI2000FileWriter:StorageTime",
        #             "Storage:BCI2000FileWriter:StorageTime")[2]
        #dateFmt <- c("%Y-%m-%dT%H:%M:%S", "%a %b %d %H:%M:%S %Y")[2]
    
        # Select sampling rate parameter from BCI data
        # Terminate if a known parameter does not exist in BCI data
        # Select first known parameter encountered
        #print(setNames(unlist(bcidat[["parameterDef"]][[k]))]][c("section", "name", "value")]), NULL))
        sampRatePar <- c("Source:SignalProperties:DataIOFilter:SamplingRate",
                         "Source:SignalProperties:SignalGeneratorADC:SamplingRate",
                         "Source:gUSBampADC:SamplingRate")
        isr <- grep(paste(sampRatePar, sep="", collapse="|"), names(bcidat[["parameterDef"]]))
    
        # Convert BCI2000 data to EDF byte vector and write to specified
        if(length(isr)>0) {
          print("Call:  bciToEDF()")
          edfdat <- bciToEDF(bcidat, patientID=patientID, recordingID=recordingID,
                             sampRatePar=names(bcidat[["parameterDef"]])[isr[1]])
          readr::write_file(edfdat, fEDF)
        } else {
          stop(paste("No recognized sampling rate parameter exists in BCI2000 input file (", fileBCI[ifile], sep=""))
        }
      },
      warning=function(err) print(paste("Warning:  ", err[["message"]], sep=""))
    ),
    error=function(err) print(paste("Error:  ", err[["message"]], sep=""))
  )

  return(data.frame("i"=i, "f"=fBCI, "t"=proc.time()["elapsed"]-t0["elapsed"]))

}

# Configure cluster
ncl <- 2
cl <- makePSOCKcluster(ncl)

# Configure directories and function source
functionSource <- paste(c("C:/Projects/Duke/BCI", "C:/Users/tjb48/Projects")[1], "/BCI2000/BCIFunctions.r", sep="")
dr0 <- "//lcollins-00.fs.egr.duke.edu/lcollins-00/data/data"
indir <- paste(dr0, "/data/BCI", sep="")
outdir <- paste(dr0, "/BCIsubjectDatabase/EDF_FileStruct_Demo", sep="")

# Retrieve BCI file names and identification data
# Export so that they are available to parallel cores
fileBCI <- read.table(paste(dr0, "/BCIsubjectDatabase/BCI_DAT2EDF/DATA/Demo_DAT2EDF_Information.csv", sep=""), header=T, sep=",")
clusterExport(cl, c("fileBCI", "functionSource", "indir", "outdir"))

# Execute
cvtResult <- do.call(rbind, parApply(cl, as.matrix(1:nrow(fileBCI)), 1, convertBCI))

stopCluster(cl)

# Examine files
finfo <- file.info(dir(outdir, recursive=T, full.names=T, pattern="\\.edf"))

########################################################################################################
# File names and parameter values for initial sample files
########################################################################################################

# Configure source file names
# Authentication required to access BCI server
fileBCI <- paste("\\\\lcollins-00.fs.egr.duke.edu/lcollins-00/data/data/data/BCI/",
                 c("data_pBrunner_covert_overt/data/A10-XMP/XMPS001R09.dat",
                   "data_2009spring_RC_CB/CBData/CBRC012_001/CBRC012_S001R05.dat",
                   "data_2009spring_RC_CB/RCData/CBRC001_003/CBRC001_S003R01.dat",
                   "data_2011spring_longitudinalChannelSelection/DukeLong_subject10/DukeLong_10_RC005/DukeLong_10_RCS005R02.dat",
                   "data_2010fall_RC_CB_Random/RC/Duke_017_RC002/Duke_017_RCS002R09.dat",
                   "data_2011fall_ERN/ERN_018001/ERN_018S001R06.dat",
                   "data_pBrunner_covert_overt/data/B01-EXE/EXES001R19.dat",
                   "data_2010summer_longitudinalALS/long001_004/long001_S004R05.dat",
                   "data_pBrunner_covert_overt/data/A07-PXB/PXBS001R04.dat",
                   "data_2013summer_DSLM_DictionaryCorrection/Subject_03_Test_001/Subject_03_Test_S001R02.dat",
                   "data_2016fall_CodebookSelectCBP/Subject_07/Subject_07_CBP_Test_001/Subject_07_CBP_Test_S001R01.dat",
                   "data_2016fall_CodebookSelectCBP/Subject_01/Subject_01_PBP_Test_001/Subject_01_PBP_Test_S001R01.dat",
                   "data_2012fall_RC_CB_CL_ALS/DukeRC/Long_001_RC001/Long_001_RCS001R01.dat",
                   "data_2016winter_CodebookSelect/Subject_05/Subject_05_RCP_Test_001/Subject_05_RCP_Test_S001R06.dat",
                   "data_2010summer_longitudinalALS_extended/long001_001/Data/long001_S001R01.dat",
                   "data_2010summer_longitudinalALS_extended/long004_005/Data/long04_S005R06.dat"), sep="")
x <- bciRetrieve(fileBCI[length(fileBCI)])

# Compose EDF patient and recording IDs
patID <- "068134"
patSex <- "M"
patAge <- 31
patRace <- "CaucasianNonHispanic"
patALS <- "NonALS"
patientID <- paste(patID," ", patSex, " 01-JAN-", 2020-patAge, " ", patRace, "_", patALS, sep="")
recType <- "X"
recID <- "Y"
recEEGAmpModel <- "Z"
recordingID <- paste("Startdate 01-JAN-2020", " ", recType, " ", recID, " ", recEEGAmpModel, sep="")

##################################################################################################
# Compare values exported from EDF browser to those read from BCI file
# To export from the EDF browser, first close all files (File, Close All), open the EDF file
# to the current contents of the bcidat list, then export (Tools, Export EDF to ASCII)
##################################################################################################

i <- 1
fBCI <- paste(sub("Y:\\data\\BCI\\", indir, fileBCI[i,"Path"], fixed=T), "/", fileBCI[i,"File"], sep="")

bcidat <- bciRetrieve(fBCI)

a <- strsplit(fBCI, "/")[[1]]
fileTXT <- paste("C:/Projects/Duke/BCI/EDF/BCIConversion/EDFExport/", gsub(".dat", "_data.txt", a[length(a)], fixed=T), sep="")
x <- read.table(fileTXT, header=T, sep=",")

# Omit time column
x <- x[,2:ncol(x)]

# EEG channels
# Retrieve offsets and gains
offset <- as.numeric(bcidat[["parameterDef"]][["Source:SignalProperties:DataIOFilter:SourceChOffset"]][["value"]])
gain <- as.numeric(bcidat[["parameterDef"]][["Source:SignalProperties:DataIOFilter:SourceChGain"]][["value"]])

# Review select channel
# Convert signal values using corresponding offset and gain
bcidat[["meta"]][["SourceCh"]]
i <- 1:10
j <- 1
if(bcidat[["meta"]][["DataFormat"]]!="float32") {
  # Convert source BCI file values to EDF
  (bcidat[["signalDat"]][["eegVal"]][i,j]-offset[j])*gain[j]
  x[i,j]
  # Convert EDF values to source BCI file values
  bcidat[["signalDat"]][["sigVal"]][i,j]
  x[i,j]/gain[j]+offset[j]
} else {
  dmin <- bcidat[["signalDat"]][["eegProp"]][j,"dMin"]
  dmax <- bcidat[["signalDat"]][["eegProp"]][j,"dMax"]
  pmin <- bcidat[["signalDat"]][["eegProp"]][j,"pMin"]
  pmax <- bcidat[["signalDat"]][["eegProp"]][j,"pMax"]
  # Convert source BCI file values to EDF
  pmin+(bcidat[["signalDat"]][["eegVal"]][i,j]-dmin)/(dmax-dmin)*(pmax-pmin)
  x[i,j]
  # Convert EDF values to source BCI file values
  bcidat[["signalDat"]][["eegVal"]][i,j]-dmin
  (x[i,j]-pmin)/(pmax-pmin)*(dmax-dmin)
}

# Review select eye tracker signal
# Convert signal data using corresponding digital and physical ranges
ncol(bcidat[["signalDat"]][["etVal"]])
i <- 200:220
j0 <- bcidat[["meta"]][["SourceCh"]]
j <- 2
dmin <- bcidat[["signalDat"]][["etProp"]][j,"dMin"]
dmax <- bcidat[["signalDat"]][["etProp"]][j,"dMax"]
pmin <- bcidat[["signalDat"]][["etProp"]][j,"pMin"]
pmax <- bcidat[["signalDat"]][["etProp"]][j,"pMax"]
# Convert source BCI file values to EDF
cbind(pmin+(bcidat[["signalDat"]][["etVal"]][i,j]-dmin)/(dmax-dmin)*(pmax-pmin), x[i,j0+j])
# Convert EDF values to source BCI file values
cbind(bcidat[["signalDat"]][["etVal"]][i,j]-dmin, (x[i,j0+j]-pmin)/(pmax-pmin)*(dmax-dmin))

# Review P300 signals
ncol(bcidat[["signalDat"]][["p300Val"]])
colnames(bcidat[["signalDat"]][["p300Val"]])
j0 <- ncol(bcidat[["signalDat"]][["eegVal"]])+ncol(bcidat[["signalDat"]][["etVal"]])
for(j in 1:ncol(bcidat[["signalDat"]][["p300Val"]])) {
  dmin <- bcidat[["signalDat"]][["p300Prop"]][j,"dMin"]
  dmax <- bcidat[["signalDat"]][["p300Prop"]][j,"dMax"]
  pmin <- bcidat[["signalDat"]][["p300Prop"]][j,"pMin"]
  pmax <- bcidat[["signalDat"]][["p300Prop"]][j,"pMax"]
  # Convert source BCI file values to EDF
  y1 <- pmin+(bcidat[["signalDat"]][["p300Val"]][,j]-dmin)/(dmax-dmin)*(pmax-pmin)
  y2 <- x[,j0+j]
  #print(table(y1))
  #print(table(y2))
  print(which(y1!=y2))
  # Convert EDF values to source BCI file values
  y1 <- bcidat[["signalDat"]][["p300Val"]][,j]-dmin
  y2 <- (x[,j0+j]-pmin)/(pmax-pmin)*(dmax-dmin)
  #print(table(y1))
  #print(table(y2))
  print(which(y1!=y2))
}

########################################################################################################
# Compare BCI and EDF values by reading files directly
########################################################################################################

f <- fileBCI[1]

# Read BCI meta information (first line)
x <- scan(f, what="character", n=1, sep="\n")

# Parse meta data line (header length, source signal count, and state vector length parameter values)
y <- gsub("=", "", x)
y <- strsplit(y, " ")[[1]]
y <- setNames(y[seq(2, length(y), 2)], y[seq(1, length(y), 2)])
# Elements referenced using name not in names(y) result in NA 
meta <- list("Version"=as.numeric(y["BCI2000V"]),
             "HeaderLen"=as.numeric(y["HeaderLen"]),
             "SourceCh"=as.numeric(y["SourceCh"]),
             "StatevectorLen"=as.numeric(y["StatevectorLen"]),
             "DataFormat"=y["DataFormat"])

# Read EEG signal data
x <- readr::read_file_raw(f)
x <- x[(meta[["HeaderLen"]]+1):length(x)]
if(is.na(meta[["DataFormat"]])) {
  sigfmt <- "integer"
  nbsig <- 2
} else if(meta[["DataFormat"]]=="int32") {
  sigfmt <- "integer"
  nbsig <- 4
} else if(meta[["DataFormat"]]=="float32") {
  sigfmt <- "numeric"
  nbsig <- 4
} else {
  sigfmt <- NA
  nbsig <- NA
}
nbsamp <- nbsig*meta[["SourceCh"]]+meta[["StatevectorLen"]]
nsamp <- length(x)/nbsamp
xBCI <- t(apply(as.matrix(1:nsamp), 1,
                function(i) {
                  p0 <- (i-1)*nbsamp
                  apply(as.matrix(1:meta[["SourceCh"]]), 1,
                        function(j) {
                          p1 <- p0+(j-1)*nbsig
                          readBin(x[p1+1:nbsig], what=sigfmt, size=nbsig, n=1, signed=T, endian="little")
                        })
                }))
dim(xBCI)

# Read signals from EDF file
a <- strsplit(f, "/")[[1]]
fileEDF <- paste("\\\\lcollins-00.fs.egr.duke.edu/lcollins-00/data/data/BCIsubjectDatabase/BCI_DAT2EDF/DATA/EDFConverted/",
                 gsub(".dat", ".edf", a[length(a)], fixed=T), sep="")

x <- readr::read_file_raw(fileEDF)
# Parse parameter values
# Bytes in header
nbh <- as.integer(rawToChar(x[185:192]))
# Number of signals
nsig <- as.integer(rawToChar(x[253:256]))
p0 <- 256+104*nsig
pmin <- apply(as.matrix(seq(p0, p0+8*nsig-8, 8)), 1, function(p1) as.numeric(rawToChar(x[(p1+1):(p1+8)])))
pmax <- apply(as.matrix(seq(p0+nsig*8, p0+16*nsig-8, 8)), 1, function(p1) as.numeric(rawToChar(x[(p1+1):(p1+8)])))
dmin <- apply(as.matrix(seq(p0+nsig*16, p0+24*nsig-8, 8)), 1, function(p1) as.integer(rawToChar(x[p1:(p1+8)])))
dmax <- apply(as.matrix(seq(p0+nsig*24, p0+32*nsig-8, 8)), 1, function(p1) as.integer(rawToChar(x[p1:(p1+8)])))
nsamp <- apply(as.matrix(seq(257+nsig*216, 256+nsig*224, 8)), 1,
               function(p0) as.integer(rawToChar(x[p0:(p0+7)])))
j <- 8
p0 <- nbh+2*nsamp[j]*(j-1)
y <- apply(as.matrix(seq(p0+1, p0+2*nsamp[j], 2)), 1,
           function(p1) readBin(x[p1:(p1+1)], what="integer", size=2, n=1, signed=T, endian="little"))
y[1:100]
pmin[j]+(y[1:100]-dmin[j])/(dmax[j]-dmin[j])*(pmax[j]-pmin[j])

########################################################################################################
# Evaluate performance-based P300 speller paradigm data
########################################################################################################

# For standard checkerboard method, targ rows with data to be treated (included in EDF output)
# contain values > 0
table(stimCode)

# jtg is the matrix of target values
dim(jtg)

# Review targ00 values
# Note that, for the standard checkerboard method, targ00 indicates the number of leading columns
# that contain a speller grid index
table(jtg[which(stimCode>0),1])

# Tabulate first n columns, where n=targ00
n <- 4
k <- which(stimCode>0 & jtg[,1]==n)
table(jtg[k,2:(n+1)])
View(jtg[k,1:(n+1)])

# Inspect speller definition
x[["parameterDef"]][["Application:SpellerTargets:P3SpellerTask:TargetDefinitions"]][["value"]]

# Verify that no targ values exceed the speler definition length
table(jtg[which(stimCode>0),2:ncol(jtg)])

########################################################################################################
# Archived material follows
########################################################################################################

##################################################################################################
# Review BCI data elements
##################################################################################################

bcidat[["parameterDef"]][[grep("TransmitChList", names(bcidat[["parameterDef"]]))]][["value"]]

########################################################################################################
# Read an EDF file and extract signal values (EEG and state vector variables)
# Read EEG and state vector values supplied by Duke BCI team
# Compare
# Note that the BCI and EDF viewers can also be used to make graphical comparisons
########################################################################################################

# Read EDF
x <- readr::read_file_raw(paste("EDF/BCIConversion/", gsub(".dat", ".edf", f0), sep=""))
#class(xedf)
#length(xedf)

# Parse parameter values
# Bytes in header
nbh <- as.integer(rawToChar(x[185:192]))
# Number of signals
nsig <- as.integer(rawToChar(x[253:256]))
# Physical and digital min and max
p0 <- 256+104*nsig
pmin <- apply(as.matrix(seq(p0, p0+8*nsig-8, 8)), 1, function(p1) as.numeric(rawToChar(x[(p1+1):(p1+8)])))
pmax <- apply(as.matrix(seq(p0+nsig*8, p0+16*nsig-8, 8)), 1, function(p1) as.numeric(rawToChar(x[(p1+1):(p1+8)])))
dmin <- apply(as.matrix(seq(p0+nsig*16, p0+24*nsig-8, 8)), 1, function(p1) as.integer(rawToChar(x[p1:(p1+8)])))
dmax <- apply(as.matrix(seq(p0+nsig*24, p0+32*nsig-8, 8)), 1, function(p1) as.integer(rawToChar(x[p1:(p1+8)])))
# Number of samples
nsamp <- apply(as.matrix(seq(257+nsig*216, 256+nsig*224, 8)), 1,
               function(p0) as.integer(rawToChar(x[p0:(p0+7)])))

# Scale signal data (two byte little-endian values following header)
# bytes 1..2*nsamp[1] correspond to first through nsamp[1] values of signal 1, (2*nsamp[1]+1)..2*nsamp for signal 2, etc.
# Result is a list of vectors of dimension nSamp (sample size may vary by signal)
yedf <- lapply(1:nsig,
               function(j) {
                 p0 <- ifelse(j>1, 2*sum(nsamp[1:(j-1)])+1, 1)
                 p1 <- p0+2*nsamp[j]-1
                 z <- apply(as.matrix(nbh+seq(p0, p1, 2)), 1,
                            function(p3) {
                              256*as.integer(x[p3+1])+as.integer(x[p3])
                            })
                 #return(z)
                 return(pmin[j]+(pmax[j]-pmin[j])*(z-dmin[j])/(dmax[j]-dmin[j]))
               })

# Read Duke supplied data
# Specify columns to retain from the EDF file
# Duke data should contain columns for 16 EEG signals
# Examine Duke sheets and EDF file to verify consistency
#eegCol0 <- c(2, 6, 11, 12, 13, 14, 15, 16)
#eegCol0 <- c(2, 6, 12, 13, 14, 15)
eegCol0 <- 1:8
#svCol <- c("eyeGazeX", "eyeGazeY")
svCol <- vector()

if(T) {
  # Read signal values from Duke sheets converted from source BCI .dat file
  yduke <- read.table(paste("BCI2000/SampleData/", gsub(".dat", ".csv", f0), sep=""), header=T, sep=",")
  #colnames(yduke)
  eegCol <- paste("EEG_", eegCol0, sep="")
  yduke <- yduke[,c(eegCol, svCol)]
  # Scale EEG values
  # Verify that position and scaling values are 0 and 0.006 for each signal
  # Examine Duke sheets and .dat file for consistency
  yduke[,eegCol] <- yduke[,eegCol]*c(0.006, 0.019)[2]
} else {
  # Read signals exported from .dat file in \\lcollins directory using BCI2000 export program
  # Note that BCI exported state vector values are known to vary from corresponding .edf values
  yduke <- read.table(paste("BCI2000/SampleData/", gsub(".dat", ".ascii", f0), sep=""), sep=" ", header=T)
  eegCol <- paste("Ch", eegCol0, sep="")
  yduke <- yduke[,c(eegCol, svCol)]
}

# Convert state vector values from two's complement to signed int
# Assume 16 bit integers
if(ncol(yduke)>length(eegCol))
  for(j in (length(eegCol)+1):ncol(yduke)) {
    k <- which(yduke[,j]>=32768)
    yduke[k,j] <- yduke[k,j]-65536
  }

# Compare subset of values
k <- 1:100
#j <- which(names(yduke)=="eyeGazeX")
j <- 8
cbind(yedf[[j]][k], yduke[k,j])
which(abs(yedf[[j]][k]-yduke[k,j])>1e-2)

# Plot subset of selected signals for comparison
ylim <- c(min(yedf[k,j]), max(yedf[k,j])) + (max(yedf[k,j])-min(yedf[k,j]))*c(-0.25, 0.25)
k2 <- sort(c(k, k))[2:(2*length(k)-1)]
gdat <- rbind(data.frame("source"="EDF", "x"=k2, "y"=yedf[sort(c(k, k))[1:(2*length(k)-2)],j]),
              data.frame("source"="Duke", "x"=k2, "y"=yduke[sort(c(k, k))[1:(2*length(k)-2)],j]))
ylim <- c(min(gdat[,"y"]), max(gdat[,"y"])) + (max(gdat[,"y"])-min(gdat[,"y"]))*c(-0.25, 0.25)
ggplot() +
  geom_line(data=gdat, aes(x=x, y=y)) +
  scale_y_continuous(limits=ylim) +
  facet_wrap(~source, ncol=1)


#################################################################################################
# Compare BCI state vector elements to corresponding EDF converted values
#################################################################################################

f0 <- c("01S001R04", "02S001R04", "16S001R01")[1]
f <- paste("BCI2000/SampleData/", f0, ".dat", sep="")

# Retrieve BCI file
bcidat <- bciRetrieve(f, stateVecVar=c("eyeGazeX", "eyeGazeY"), stateVecVarTwosComp=c(T, T))

# Extract state vector values
# SourceTime vector positions are length (bits), value (first sample), byte location, bit location
# Byte and bit positions are zero-based
sigv <- c("eyeGazeX", "eyeGazeY")[1]
stvpos <- bcidat[["stateVector"]][[sigv]]
v1 <- unlist(bciStateVectorExtract(signalDat=bcidat[["signalDat"]],
                                   bytePos=stvpos[3],
                                   bitPos=stvpos[4],
                                   bitLen=stvpos[1],
                                   convertInt=T,
                                   twosComp=T))

cbind(v1, v2)
which(v1<0 | v2<0)

#####################################################################################
# Retrieve signal values from ASCII version of BCI file
# File assumed to be generated by the BCI2000 export program
# https://www.bci2000.org/mediawiki/index.php/BCI2000_Binaries#Latest_Released_Build
# Warning! The BCI provided export program returns the correct number of rows,
# with the correct signal values and state vector values, but for some rows state
# vector values appear to be taken from subsequent samples in the data 
#####################################################################################

v2 <- read.table(paste("BCI2000/SampleData/", f0, ".ascii", sep=""), sep=" ", header=T)
v2 <- v2[,2:ncol(v2)]
dim(v2)
z <- data.frame(v1, v2[,sigv])

i <- 20064
sigOffset <- as.numeric(bcidat[["parameterDef"]][["Source:SignalProperties:DataIOFilter:SourceChOffset"]][["value"]])
sigGain <- as.numeric(bcidat[["parameterDef"]][["Source:SignalProperties:DataIOFilter:SourceChGain"]][["value"]])
(bcidat[["signalDat"]][[i]][["chInt"]]-sigOffset)*sigGain
v2[i,]
v1[i]
bcidat[["signalDat"]][[i]][["stateVector"]]

################################################################
# Retrieve state vector signal values supplied by Duke BCI team
################################################################

v2 <- as.integer(scan(paste(f0, "-", sigv, ".dat", what="integer"))
which(v1!=v2)
z <- data.frame(v1, v2)

####################################
# Plot signal values for comparison
####################################

k <- 1:100
ylim <- c(min(v1[k]), max(v1[k])) + (max(v1[k])-min(v1[k]))*c(-0.25, 0.25)
kx <- sort(c(k, k))[2:(2*length(k)-1)]
ky <- sort(c(k, k))[1:(2*length(k)-2)]
ggplot() +
  geom_line(data=data.frame("source"="stv", "x"=kx, "y"=v1[ky]), aes(x=x, y=y)) +
  geom_line(data=data.frame("source"="BCIexport", "x"=kx, "y"=v2[ky]), aes(x=x, y=y)) +
  scale_y_continuous(limits=ylim) +
  facet_wrap(~source, ncol=1)

#################################################################################################
# Examine BCI contents
#################################################################################################

bcidat[["meta"]]
names(bcidat[["stateVector"]])
names(bcidat[["parameterDef"]])
x <- sort(unique(unlist(lapply(strsplit(names(bcidat[["parameterDef"]]), ":"), function(a) a[1]))))
i <- 7
k <- which(substring(names(bcidat[["parameterDef"]]), 1, nchar(x[i]))==x[i])
data.frame("id"=names(bcidat[["parameterDef"]])[k],
           "val"=unlist(lapply(k, function(i) bcidat[["parameterDef"]][[i]][["value"]][1])))

# Review state vector SourceTime values
# From https://www.bci2000.org/mediawiki/index.php/Technical_Reference:State_Definition
# SourceTime vector positions are length (bits), value (first vec), byte location, bit location
# Byte and bit positions are zero-based
sourceTime <- bcidat[["stateVector"]][["SourceTime"]]
#sampRate <- as.numeric(bcidat[["parameterDef"]][["Source:SignalProperties:SignalGeneratorADC:SamplingRate"]][["value"]])
sampRate <- as.numeric(bcidat[["parameterDef"]][["Source:SignalProperties:DataIOFilter:SamplingRate"]][["value"]])
x <- unlist(bciStateVectorExtract(signalDat=bcidat[["signalDat"]],
                                  bytePos=sourceTime[3],
                                  bitPos=sourceTime[4],
                                  bitLen=sourceTime[1],
                                  convertInt=T))/sampRate
table(x)

#####################################################################################
# Retrieve signal values from ASCII version of BCI file
# File assumed to be generated by the BCI2000 export program
# https://www.bci2000.org/mediawiki/index.php/BCI2000_Binaries#Latest_Released_Build
#####################################################################################

v2 <- read.table(paste("BCI2000/SampleData/", f0, ".ascii", sep=""), sep=" ", header=T)
v2 <- v2[,2:ncol(v2)]

########################
# Compare signal values
########################

i <- 1000
sigOffset <- as.numeric(bcidat[["parameterDef"]][["Source:SignalProperties:DataIOFilter:SourceChOffset"]][["value"]])
sigGain <- as.numeric(bcidat[["parameterDef"]][["Source:SignalProperties:DataIOFilter:SourceChGain"]][["value"]])
(bcidat[["signalDat"]][[i]][["chInt"]]-sigOffset)*sigGain
v2[i,]

##################################################################################################
# Review state vector values
##################################################################################################

x <- unlist(bciStateVectorExtract(signalDat=bcidat[["signalDat"]],
                                  bytePos=33,
                                  bitPos=7,
                                  bitLen=8,
                                  convertInt=T,
                                  twosComp=T))
table(x)

##################################################################################################
# Report text to spell for select BCI files
##################################################################################################

f <- c("data_2010summer_longitudinalALS_extended/long004_005/Data/long04_S005R06.dat")[1]

writeLines(
  apply(as.matrix(f), 1,
        function(f2) {
          f <- paste("//lcollins-00.fs.egr.duke.edu/lcollins-00/data/data/data/BCI/", f, sep="")
          a <- scan(f, what="character", sep="\n")
          paste(f, ": ", 
                sub("Application:CopySpelling:StimulusTask string", "",
                    sub(" // character or string to spell in offline copy mode", "",
                        a[grep("TextToSpell", a)])),
                sep="")
        })
)

##################################################################################################
# Plot PhaseInSequence
# Report text to spell
##################################################################################################

source("BCI2000/BCIFunctions.r")

f <- c("data_2012summer_dsLanguageModel/Subject_05_Lm_001/Subject_05_Lm_S001R01.dat",
       "data_2010summer_longitudinalALS_extended/long004_005/Data/long04_S005R06.dat",
       "data_2010summer_longitudinalALS_extended/long003_001/Data/long003_S001R10.dat",
       "data_2010summer_longitudinalALS_extended/long003_001/Data/long003_S001R14.dat",
       "data_2010summer_longitudinalALS_extended/long002_001/Data/long002_S002R01.dat")[5]
x <- bciRetrieve(paste("//lcollins-00.fs.egr.duke.edu/lcollins-00/data/data/data/BCI/", f, sep=""))

ggplot() +
 geom_line(aes(x=1:nrow(x[["signalDat"]][["p300Val"]]), y=x[["signalDat"]][["p300Val"]][,"PhaseInSequence"])) +
 theme(plot.title=element_text(size=10), plot.subtitle=element_text(size=10)) +
 labs(title=f,
  subtitle=paste("Text to spell:  ", x[["parameterDef"]][[grep("texttospell", names(x[["parameterDef"]]), ignore.case=T)[1]]][["value"]], sep=""),
  x="\nsample index", y="phase in seq\n")
