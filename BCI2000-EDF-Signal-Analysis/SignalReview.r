library(ggplot2)
options(device="RStudioGD")

source(functionSource)

i <- 1

x <- bciRetrieve(paste(fileBCI[i,"Source"], "/", fileBCI[i,"File"], sep=""))

dir.create(fileBCI[i,"Destination"], recursive=T)
patientID <- paste(fileBCI[i,"Subject"]," ", fileBCI[i,"Sex"], " ", fileBCI[i,"Age"], " ",
 fileBCI[i,"Race"], "_", fileBCI[i,"Ethnicity"], " ", fileBCI[i,"ALSFRS"], sep="")
recordingID <- paste("Startdate 01-JAN-2020", " ", fileBCI[i,"Study"], " ", fileBCI[i,"Session"], " ",
 fileBCI[i,"Amp"], sep="")
y <- bciToEDF(x, patientID=patientID, recordingID=recordingID)
readr::write_file(y, paste(fileBCI[i,"Destination"], "/", fileBCI[i,"fileEDF"], sep=""))

colnames(x[["signalDat"]][["etVal"]])
z <- x[["signalDat"]][["etVal"]][,"ETLeftPupilSize"]
ggplot() +
 geom_line(aes(x=1:length(z), y=z))

f <- c("data_2014summer_AlsWetDry/ETSU/Dry/Long_005_DryTrain001/Long_005_DryTrainS001R6.dat",
       "data_2014summer_AlsWetDry/Duke/Subject_05_DryTrain002/Subject_05_DryTrainS002R01.dat",
       "data_2014summer_AlsWetDry/Duke/Subject_06_DryTest001/Subject_0X_DryTestS001R01.dat",
       "data_2014summer_AlsWetDry/Duke/Subject_05_GelTest001/Subject_0X_GelTestS001R01.dat")[4]
x <- bciRetrieve(paste("/hpc/group/mainsahlab/BCIData/BCI2000Data/", f, sep=""))
a <- names(x[["parameterDef"]])
a[grep("targ", a, ignore.case=T)]
x[["parameterDef"]][["Application:CopySpelling:StimulusTask:TextToSpell"]]
z <- x[["signalDat"]][["p300Val"]][,"PhaseInSequence"]
ggplot() +
  geom_line(aes(x=1:length(z), y=z)) +
  scale_y_continuous(breaks=0:3) +
  labs(x="Sample", y="Phase")
table(z)

convertBCI(fileBCI[342,])

i <- which(fileBCI[,"File"]=="long04_S005R00.dat")
fileBCI[i,]
convertBCI(fileBCI[i,])

i <- match(c("N01_SE001Dry_Train06.edf", "N10_SE001Wet_Test01.edf", "N10_SE002Dry_Train01.edf", "N11_SE001Dry_Test01.edf"), fileBCI[,"fileEDF"])
fileBCI[i,]
j <- i[1]
x <- bciRetrieve(paste(fileBCI[j,"Source"], "/", fileBCI[j,"File"], sep=""))

k <- grep("InterpretMode", names(x[["parameterDef"]]))
x[["parameterDef"]][[k]]
