source("BCI2000/BCIFunctions.r")

i <- 21
#f <- paste(sub("Y:\\data\\BCI", indir, fileBCI[i,"Source"], fixed=T), "/", fileBCI[i,"File"], sep="")
#f <- paste(indir, "/data_2011fall_ERN/ERN_018001/ERN_018S001R06.dat", sep="")
#f <- "\\\\lcollins-00.fs.egr.duke.edu/lcollins-00/data/data/data/BCI/data_2011spring_dynamicStopping/Dynamic Stopping Sessions (testing)/DDy_01_RC001/DDy_01_RCS001R01.dat"
#f <- "C:/Projects/Duke/BCI/BCI2000/SampleData/DDy_01_RCS001R01.dat"
f <- "\\\\lcollins-00.fs.egr.duke.edu/lcollins-00/data/data/data/BCI/data_2011fall_ERN/ERN_002001/ERN_002S001R02.dat"
bcidat <- bciRetrieve(f)

# Parse meta data line (header length, source signal count, and state vector length parameter values)
x <- scan(f, what="character", n=1, sep="\n")
y <- gsub("=", "", x)
y <- strsplit(y, " ")[[1]]
y <- setNames(y[seq(2, length(y), 2)], y[seq(1, length(y), 2)])
meta <- list("Version"=as.numeric(y["BCI2000V"]),
             "HeaderLen"=as.numeric(y["HeaderLen"]),
             "SourceCh"=as.numeric(y["SourceCh"]),
             "StatevectorLen"=as.numeric(y["StatevectorLen"]),
             "DataFormat"=y["DataFormat"])
if(is.na(meta[["Version"]])) {
  meta[["Version"]] <- 1.0
  meta[["DataFormat"]] <- "int16"
}

# Read binary section
x <- readr::read_file_raw(f)
x <- x[(meta[["HeaderLen"]]+1):length(x)]

# Extract state vector data
if(meta[["DataFormat"]]=="int16") {
  nbsig <- 2
} else if(meta[["DataFormat"]] %in% c("int32", "float32")) {
  nbsig <- 4
} else {
  nbsig <- -1
}
nbsamp <- nbsig*meta[["SourceCh"]]+meta[["StatevectorLen"]]
nsamp <- length(x)/nbsamp
stv <- t(apply(as.matrix(1:nsamp), 1, function(i)
               x[(i*nbsamp-meta[["StatevectorLen"]]+1):(i*nbsamp)]))

pd <- bcidat[["parameterDef"]]
sv <- bcidat[["stateVector"]]

# P300 speller configuration
pd[["Application:SpellerTargets:P3SpellerTask:NumMatrixRows"]][["value"]]
pd[["Application:SpellerTargets:P3SpellerTask:NumMatrixColumns"]][["value"]]
pd[["Application:SpellerTargets:P3SpellerTask:TargetDefinitions"]][["value"]]

# Row-column or checkerboard
pd[["Application:Checkerboard:P3SpellerTask:Standard"]][["value"]][1]
pd[["Application:StimulusGroupingMode:P3SpellerTask:UseCheckerboard"]][["value"]][1]

# Stimulus code
v <- sv[["StimulusCode"]]
stimCode <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(stimCode)

# Stimulus begin
v <- sv[["StimulusBegin"]]
stimBegin <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(stimBegin)

# Stimulus type
v <- sv[["StimulusType"]]
stimType <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(stimType)

# Parse targ00 through targ19
# Review state vector definitions to verify starting byte, bit position, and bit length
v <- sv[["Targ00"]]
targ <- apply(as.matrix(v[3]:(v[3]+19)), 1, function(i) bciStateVectorExtract(stv, i, v[4], v[1], convertInt=T))
dim(targ)
table(targ[,1])
table(targ[,2:20])

# Row-column method
k <- which(stimCode>0)
stimCode[k[200:300]]

# Compare number of targets with non-zero targets reported
y <- t(apply(targ, 1, function(x) if(x[1]>0) {c(x[1], length(which(x[2:length(x)]>0)))} else {c(0, 0)}))
dim(y)
which(y[,1]>0 & y[,1]!=y[,2])

# Non-zero stimulus code and number of targets
k <- which(stimCode>0)
targ[k[1:10],]
k <- which(stimCode>0 & targ[,1]>0)
targ[k[1:10],]

# Stimulus columns
setNames(bcidat[["signalDat"]][["p300Val"]][k[1],], NULL)

i <- 100
stimCode[k[i]]
which(bcidat[["signalDat"]][["p300Val"]][k[i],]==1)
setNames(which(bcidat[["signalDat"]][["p300Val"]][k[i],]==1), NULL)

# Enumerate non-zero targets by sample
y <- apply(targ, 1, function(x) length(which(x>0)))
table(y)
k <- which(y>0 & (selTarg>0 | selRow>0 | selCol>0))
y[k[1:10]]
selTarg[k[1:10]]
selRow[k[1:10]]
selCol[k[1:10]]

# Eye tracker
v <- sv[["EyetrackerLeftEyeValidity"]]
x <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(x)
v <- sv[["EyetrackerRightEyeValidity"]]
x <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(x)

v <- sv[["EyetrackerLeftEyePosX"]]
x <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
c(min(x), max(x))
v <- sv[["EyetrackerLeftEyePosY"]]
x <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
c(min(x), max(x))
v <- sv[["EyetrackerRightEyePosX"]]
x <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
c(min(x), max(x))
v <- sv[["EyetrackerRightEyePosY"]]
x <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
c(min(x), max(x))
v <- sv[["EyetrackerLeftPupilSize"]]
x <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
x[200:210]
c(min(x), max(x))
v <- sv[["EyetrackerRightPupilSize"]]
x <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
x[200:210]
c(min(x), max(x))

# Stimulus type
v <- sv[["StimulusType"]]
stimType <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(stimType)

# Selected target, row, and column
v <- sv[["SelectedTarget"]]
selTarg <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(selTarg)
v <- sv[["SelectedRow"]]
selRow <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(selRow)
v <- sv[["SelectedColumn"]]
selCol <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(selCol)

# Current target
currTarg <- bcidat[["signalDat"]][["p300Val"]][,"CurrentTarget"]
table(currTarg)

# Selected target
selTarg <- bcidat[["signalDat"]][["p300Val"]][,"SelectedTarget"]
table(selTarg)

# BCI selection
bciSelection <- bcidat[["signalDat"]][["p300Val"]][,"BCISelection"]
table(bciSelection)

# Phase in sequence
v <- sv[["PhaseInSequence"]]
ph <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
table(ph)
ggplot() + geom_line(aes(x=1:length(ph), y=ph, linetype="phseq")) +
           geom_line(aes(x=1:length(selTarg), y=selTarg, color="seltarg")) +
           geom_line(aes(x=1:length(currTarg), y=currTarg, color="currtarg")) +
           geom_line(aes(x=1:length(bciSelection), y=bciSelection, color="bcisel")) +
           scale_linetype_manual(values=c("phseq"="dotted")) +
           scale_color_manual(values=c("seltarg"="red3", "currtarg"="blue3", "bci"="green3"))

ggplot() + geom_line(aes(x=1:length(ph), y=ph, linetype="phseq")) +
           geom_line(aes(x=1:length(selTarg), y=selTarg, color="seltarg")) +
           geom_line(aes(x=1:length(currTarg), y=currTarg, color="currtarg")) +
           geom_line(aes(x=1:length(bciSelection), y=bciSelection, color="bcisel")) +
           scale_linetype_manual(values=c("phseq"="dotted")) +
           scale_color_manual(values=c("seltarg"="red3", "currtarg"="blue3", "bcisel"="green3"))

# Identify indices of samples where PhaseInSequence transitions to "2"
k <- 2:length(x)
which(x[k]==2 & x[k]!=x[k-1])
# Identify indices of samples where PhaseInSequence transitions from "2"
k <- 1:(length(x)-1)
which(x[k]==2 & x[k]!=x[k+1])


i <- 1:35000
ggplot() + geom_line(aes(x=i, y=bcidat[["signalDat"]][["p300Val"]][i,ncol(bcidat[["signalDat"]][["p300Val"]])]))

pd[["Application:SpellerTargets:P3SpellerTask:TargetDefinitions"]][["value"]]

# Intersection of phase in sequence and stimulus type
v <- sv[["PhaseInSequence"]]
phseq <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
v <- sv[["StimulusType"]]
stimType <- bciStateVectorExtract(svDat=stv, bytePos=v[3], bitPos=v[4], bitLen=v[1], convertInt=T)
k <- which(phseq==2 & stimType==1)
length(k)

spdef <- pd[["Application:SpellerTargets:P3SpellerTask:TargetDefinitions"]][["value"]]

pd[["Application:Speller:P3SpellerTask:TextToSpell"]][["value"]]

pd[["Application:Speller:P3SpellerTask:TextResult"]][["value"]]
