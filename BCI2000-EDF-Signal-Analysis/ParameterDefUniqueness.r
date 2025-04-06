# Duke University Brain-Computer Interface (BCI) Dataset Development Project
# Evaluate BCI file parameter definitions

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(digits.secs=4)
options(device=c("windows", "RStudioGD")[1])

setwd("C:/Users/tjb48/Projects")

# Executing in a Windows environment causes errors during scan of files with more than 259 characters in file name
# Note that the R dir() function returns complete file names (path included) even when greater than 259 characters in length
# An attempt was made to retrieve full file names, split into directory and file name, setwd(dir), then read files
# without prepended dir, but this fails ("No such file or directory")
# Attempts to use connection directly also fail
# Files with name length > 259 generate warning messages and no data are included for them
f <- dir("\\\\lcollins-00.fs.egr.duke.edu/lcollins-00/data/data/data/BCI", full.names=T, recursive=T, pattern="\\.dat")
f <- f[grep("\\.txt", f, invert=T)]
f <- f[grep("\\.xls", f, invert=T)]
length(f)

# Read files and parse parameter defs
pdef <- do.call(rbind,
                lapply(1:length(f),
               function(i) {

                 print(i)
                 err <- ""
 
                 # Read meta information (first line) from file
                 x <- tryCatch(scan(f[i], what="character", n=1, sep="\n", quiet=T),
                               warning=function(e) err <<- e$message,
                               error=function(e) err <<- e$message)
               
                 if(err=="") {
                 
                   # Parse meta data line (header length, source EEG signal count, and state vector length parameter values)
                   y <- gsub("=", "", x)
                   y <- strsplit(y, " ")[[1]]
                   y <- setNames(y[seq(2, length(y), 2)], y[seq(1, length(y), 2)])
                   hlen <- as.numeric(y["HeaderLen"])
 
                   # Read header section of file (based on length indicated by HeaderLen parameter)
                   # Note that the meta data line is included in the header length count
                   y <- readChar(f[i], nchars=hlen)
                   #print(y)
                   hdr <- strsplit(y, "\r\n")[[1]]

                   # Parse parameter definition lines
                   p0 <- which(hdr=="[ Parameter Definition ] ") + 1
                   p1 <- length(hdr)-1

                   # Isolate parameter defs
                   hdr <- hdr[p0:p1]

                   # Locate = signs
                   j <- regexpr("=", hdr)
                   # Convert endpoint when = absent
                   k <- which(j<1)
                   j[k] <- nchar(hdr[k])+1

                   # Parse parameter IDs (names) and values
                   parID <- substring(hdr, 1, j-1)
                   val <- ifelse(j>0, substring(hdr, j+2, nchar(hdr)), NA)

                   return(data.frame("f"=f[i], "err"=NA, "parID"=parID, "val"=val))
 
                 } else {
 
                   return(data.frame("f"=f[i], "err"=err[1], "parID"=NA, "val"=NA))
 
                 }
              }))
nrow(pdef)
head(pdef)

# Inspect file read errors
k <- which(!is.na(pdef[,"err"]))
pdef[k,c("f", "err")]

# Identify variations in parameter def names
ptxt <- c("choffset", "chgain", "gazeoffset", "gazescale", "standard", "usecheckerboard", "targetdef",
          "matrixrow", "matrixcol", "texttospell", "textres", "displayres")
#k <- which(is.na(pdef[,"err"]))
x <- do.call(rbind,
       lapply(ptxt,
         function(a) {
           print(a)
           # Construct data frame of all examples of current parameter ID text
           k <- grep(a, pdef[,"parID"], ignore.case=T)
           if(length(k)>0) {
             y <- pdef[k,c("parID", "f")]
             # Tabulate files by parameter ID text
             z <- table(y[,"parID"], useNA="ifany")
             # Retrieve example file name for each parameter ID text variety
             fz <- y[match(names(z), y[,"parID"]),"f"]
             # Construct data frame of parameter ID text varieties, frequencies, and example file names
             data.frame("partxt"=a, "parID"=names(z), "n"=as.vector(z), "exFile"=fz)
           } else {
             data.frame("partxt"=a, "parID"=NA, "n"=NA, "exFile"=NA)
           }
         }))

writeLines(apply(as.matrix(x), 1,
                 function(y) {
                   y <- gsub("%20", " ",
                             gsub("_", "\\_",
                                  gsub("/", "$\\\\backslash$",
                                       sub("\\\\lcollins-00.fs.egr.duke.edu/lcollins-00/data/data/data/", "", y, fixed=T)
                                  ),                                   
                                  fixed=T),
                             fixed=T)
                   paste("\\small ", y["partxt"], " & \\small ", y["parID"], " & \\multicolumn{1}{r}{\\small ",
                         format(as.numeric(y["n"]), big.mark=","),
                         "} \\\\ \\multicolumn{3}{l}{\\small ex: ", y["exFile"], "} \\\\[6pt] ", sep="")
                 }))

# Inspect speller target definitions
k <- grep("targetdef", pdef[,"parID"], ignore.case=T)
y <- pdef[k,"val"]
#unique(y)
unique(apply(as.matrix(y), 1, function(a) substring(a, nchar(a)-50, nchar(a))))

# Replace bracketed element of speller def with a 5 (all observed defs have length-5 elements)
i <- regexpr("\\{", y)
j <- regexpr("\\}", y)
k <- which(i>1)
y[k] <- apply(as.matrix(k), 1, function(m) paste(substring(y[m], 1, i[m]-1), substring(y[m], j[m]+1, nchar(y[m])), sep=""))
k <- regexpr("//", y)
y <- apply(as.matrix(1:length(y)), 1,
           function(i) {
             if(k[i]>1) {
               a <- substring(y[i], 1, k[i]-2)
             } else {
               a <- y[i]
             }
             b <- strsplit(a, " ")[[1]]
             paste(b[seq(3, length(b), 5)], collapse=" ", sep="")
           })
z <- as.data.frame(table(y, useNA="ifany"))
names(z) <- c("spdef", "n")
z[,"spdef"] <- levels(z[,"spdef"])

writeLines(apply(as.matrix(z), 1,
           function(y)
             paste("\\small ", gsub("_", "\\_", y[1], fixed=T),
                   " & \\multicolumn{1}{r}{", format(y[2], big.mark=","), "} \\\\[6pt]", sep="")))

# Identify eye logger parameter definitions
k <- grep("eye", pdef[,"parID"], ignore.case=T)
unique(pdef[k,"parID"])

k <- grep("LogEyetracker", pdef[,"parID"])
length(k)
pdef[k,"val"]

# Identify notch filter parameter definitions
k <- grep("notch", pdef[,"parID"], ignore.case=T)
unique(pdef[k,"parID"])

k <- grep("NotchEnabled", pdef[,"parID"])
length(k)
table(pdef[k,"val"])

k <- grep("NotchFilter", pdef[,"parID"])
length(k)
table(pdef[k,"val"])
