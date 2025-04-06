# Duke University Brain-Computer Interface (BCI) Dataset Development Project
# Functions

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

#################################################################################################
# Verify numeric validity of a value (first element of argument)
#################################################################################################

isnum <- function(x) {

  if(length(x)>0) {
    tryCatch(
      { y <- as.numeric(x)
        success <- T
      },
      error=function(e) success <<- F,
      warning=function(w) success <<- F
    )
    return(success)
  } else {
    return(F)
  }

}

#################################################################################################
# Read a BCI2000 data file
#
# A v1.0 BCI2000 file contains two major sections:  header and data.
#
#   The header section is ASCII ecoded, consisting of carriage return, line feed delimited lines.
#   It has three sections:
#
#     1. Meta information (line 1)
#
#        From bci2000.org:
#        --------------------------------------------------------------------------------------------
#        The fields in the first line specify meta information required to access the binary data
#        that follows the header.
#
#        Since version 1.1, the first line begins with a BCI2000V field containing a floating point
#        version number, and ends with a DataFormat field describing the format of the binary data as
#        int16, int32, or float32. A missing BCI2000V field indicates a file format version of 1.0,
#        and a DataFormat of int16.
#
#        The number of bytes in the state vector is determined by the sum of the lengths (given in bits)
#        for all states, rounded up to the next byte (which equals the value of StateVectorLength in both
#        the first line and, since StateVectorLength is also a system-wide parameter, in one of the lines
#        in the [ Parameter Definition ] section). Thus, the first line contains all information required
#        to read the binary information contained in the data file.
#        --------------------------------------------------------------------------------------------
#
#        The meta information line contains three data elements in name, value pairs:
#
#        a. HeaderLen indicates the total number of ASCII characters contained in the header section
#           (including the header line and terminating CRLFs)
#        b. SourceCh indicates the number of signals encoded in each element of the data section
#        c. StatevectorLen indicates the length (in bytes) of the state vectors that immediately
#           follow each data vector in the data section
#
#     2. State definition section (begins with line containing "[ State Vector Definition ]" and
#        ending at the line immediately prior to the parameter definition section)
#
#        From bci2000.org:
#        --------------------------------------------------------------------------------------------
#        State definitions are preceded with a line [ State Vector Definition ], and given in the general
#        State Definition format. In the context of the file header, the state definitions' value fields
#        are redundant since they match the values associated with the first sample in the data file.
#        --------------------------------------------------------------------------------------------
#
#        State definition lines have format:
#
#            Name Length Value ByteLocation BitLocation CRLF
#
#        where
#
#        Name         is a state parameter name (EyetrackerLeftEyeGazeX, EyetrackerLeftEyeGazeY, etc.)
#        Length       is the number of bits forming the parameter value
#        Value        is the (integer) parameter value of the first sample
#        ByteLocation is the 0-based starting byte containing the parameter value
#        BitLocation  is the 0-based bit position in ByteLocation of the parameter value
#
#     3. Parameter definition section (begins with line containing "[ Parameter Definition ]" and
#        ending at the character in position HeaderLen if the header section)
#
#        From bci2000.org:
#        --------------------------------------------------------------------------------------------
#        Parameter definitions are preceded with a line [ Parameter Definition ], and follow the general
#        Parameter Definition format. They represent the respective parameters' values at the beginning
#        of the recording.
#        --------------------------------------------------------------------------------------------
#
#        Parameter definition lines have format:
#
#            Section DataType Name= Value DefaultValue LowRange HighRange // Comment CRLF
#
#    The data section is binary encoded
#
################################################################################################################
#
#    Parameters:
#
#    f .......................... Full path and file name of BCI2000 file to read
#
#    Return object is a list with elements:
#
#    meta ........... A list with elements from the meta data line (first) of the specified file:
#                     HeaderLen ........ Header length
#                     SourceCh ......... Number of data signals
#                     StatevectorLen ... State vector length
#    stateVector .... A list of numeric vectors, one for each state variable in the specified file
#                     List names correspond to state variable names
#                     Each vector is of length 4, pos 1 = value, pos 2 = default value,
#                     pos 3 = lower bound, pos 4 = upper bound, % entries indicate empty
#    parameterDef ... A list of lists, one for each parameter contained in the specified file,
#                     each sublist with the following elements as parsed from space delimited
#                     elements fields of rows in the file:
#                     section .... A character vector of hierarchical identifiers parsed from
#                                  colon delimited values appearing prior to the first space on
#                                  an input line (ex: Storage:Session:DataIOFilter)
#                     dataType ... string, int, float, matrix (text parsed from between first
#                                  and second space of input line)
#                     name ....... Text parsed from between second and third space of input line
#                                  with equal sign omitted
#                     value ...... ASCII encoded Parameter value, parsed from text appearing between
#                                  the third space and double slash (//) on an input line
#                                  Note that int, float, and string types have four position
#                                  vectors as values, with pos 1 = value, pos 2 = default value,
#                                  pos 3 = lower bound, pos 4 = upper bound, % entries indicate empty
#                     Comment .... Free-form text appearing after the // on an input line
#    signalDat ...... A list containing:
#                     eegVal ..... An nSample X nSignals matrix of discretized (signed integer) values,
#                                  col j corresponding to EEG signal j
#                                  Integer values correspond to the discrete point on corresponding
#                                  [physical min, physical max] intervals
#                                  Physical value can be computed as
#                                  physicalMin+(X-digitalMin)/(digitalMax-digitalMin)*(physicalMax-physicalMin)
#                     eegProp .... A data frame with one row per eegVal column (row i of eegProp corresponds to
#                                  col i of eegVal) and column entries describing the corresponding eegVal
#                                  variable as follows:
#                                  pMin .... Physical min value
#                                  pMax .... Physical max value
#                                  dMin .... Digital min value
#                                  dMax .... Digital max value
#                                  tdc ..... transducer label
#                                  dim ..... Physical dimension
#                     etVal ...... An nSample X nSignals matrix of discretized (signed integer) values,
#                                  col j corresponding to the jth computed eye tracker variable
#                                  Integer values correspond to the discrete point on corresponding
#                                  [physical min, physical max] intervals
#                                  Physical value can be computed as
#                                  physicalMin+(X-digitalMin)/(digitalMax-digitalMin)*(physicalMax-physicalMin)
#                                  Note that etVal will consist of a single NA value when eye tracker data
#                                  are not officially included in the BCI.dat file (eye tracker signals may
#                                  appear in the BCI file, but unless the LogEyetracker parameter def equals 1,
#                                  the data are not official results of the study) 
#                     etProp ..... A data frame with one row per eegVal column (row i of eegProp corresponds to
#                                  col i of eegVal) and column entries describing the corresponding eegVal
#                                  variable as follows:
#                                  feasMin ...... Lower bound of the binary integer of corresponding bit length,
#                                                 given the variable's signed/unsigned specification
#                                  feasMax ...... Upper bound of bit length binary integer
#                                  pMin ......... Physical min value
#                                  pMax ......... Physical max value
#                                  dMin ......... Digital min value
#                                  dMax ......... Digital max value
#                                  dtruncLow .... Digital lower bound (values below are converted to this value)
#                                  dtruncHigh ... Digital upper bound (values above are converted to this value)
#                                  tdc .......... Transducer label
#                                  dim .......... Physical dimension
#                                  Note that etProp will consist of a single NA value when the LogEyetracker parameter
#                                  def is absent or unequal to 1
#                     p300Val .... A matrix of dimension nSamples X nCharInSpellerGrid, with one column per character,
#                                  each row of each column containing either 0 or 1, indicating whether or not the
#                                  corresponding character (col) was active for the corresponding sample (row)
#                     p300Prop ... A data frame with one column per p300Val column, each containing parameters for
#                                  converting integer signals to corresponding real number (physical) values
#
#################################################################################################

bciRetrieve <- function(f) {

  err <- vector("character")

  # Read meta information (first line)
  x <- tryCatch(
         scan(f, what="character", n=1, sep="\n"),
         warning=function(e) err <<- e[["message"]],
         error=function(e) err <<- e[["message"]]
       )

  if(length(err)==0) {

    # Parse meta data line (header length, source EEG signal count, and state vector length parameter values)
    y <- gsub("=", "", x)
    y <- strsplit(y, " ")[[1]]
    y <- setNames(y[seq(2, length(y), 2)], y[seq(1, length(y), 2)])
    # Elements referenced using name not in names(y) result in NA
    meta <- list("Version"=as.numeric(y["BCI2000V"]),
                 "HeaderLen"=as.numeric(y["HeaderLen"]),
                 "SourceCh"=as.numeric(y["SourceCh"]),
                 "StatevectorLen"=as.numeric(y["StatevectorLen"]),
                 "DataFormat"=y["DataFormat"])

    # Validate existence of meta variables

    # Set DataFormat when version absent
    # According to https://www.bci2000.org/mediawiki/index.php/Technical_Reference:BCI2000_File_Format,
    # Since version 1.1, the first line begins with a BCI2000V field containing a floating point version number, and ends
    # with a DataFormat field describing the format of the binary data as int16, int32, or float32. A missing BCI2000V
    # field indicates a file format version of 1.0, and a DataFormat of int16.
    if(is.na(meta[["Version"]])) {
      meta[["Version"]] <- 1.0
      meta[["DataFormat"]] <- "int16"
    }

    # Read header section of file (based on length indicated by HeaderLen parameter)
    # Note that the meta data line is included in the header length count
    y <- readChar(f, nchars=meta[["HeaderLen"]])
    #substr(y, 1, 50)
    #substr(y, nchar(y)-150, nchar(y))

    # Parse header lines (delimited by a carriage returne, line feed)
    # The resulting vector has one element per line
    hdr <- strsplit(y, "\r\n")[[1]]
    #hdr[1:10]
    #hdr[(length(hdr)-10):length(hdr)]

    # Validate:  length(hdr)>1 and all but final element of char length > 0
    # nchar(hdr)

    # Identify positions of state vector and parameter definition elements
    psec <- c("state"=which(hdr=="[ State Vector Definition ] "),
              "par"=which(hdr=="[ Parameter Definition ] "))

    # Extract state vector definition elements and parse into name, value pairs
    # The result is a list with one element per state vector definition, each element
    # containing a numeric vector with the corresponding Length, Value, ByteLocation,
    # and BitLocation values
    # List element names correspond to state vector definition names
    if(psec["state"]<psec["par"]) {
      # Index state definition elements
      k <- (psec["state"]+1):(psec["par"]-1)
      # Locate first space in each state vector element (delimits parameter name from value)
      i <- regexpr(" ", hdr[k])
      if(all(i>1)) {
        sv <- lapply(substr(hdr[k], i+1, nchar(hdr[k])),
                     function(a)
                       # Length, Value, ByteLocation, and BitLocation are space dilimited numeric values
                       # Invalid ASCII encoded numeric values are converted to NA
                       as.numeric(strsplit(a[1], " ")[[1]]))
        names(sv) <- substr(hdr[k], 1, i-1)
      } else {
        sv <- list()
      }
    } else {
      sv <- list()
    }

    # Validate:  length(sv)>0, each element is numeric of length 4, no elements contain NA

    # Extract parameter definition elements and parse into section, data type, name, value,
    # default value, low range, high range, comment values
    # Result is a list with one element per parameter definition, each element
    # containing a list of Section, DataType, Name, Value, DefaultValue, LowRange, HighRange,
    # Comment named elements
    if(psec["state"]<psec["par"]) {
      # Index parameter definition elements
      k <- (psec["par"]+1):(length(hdr)-1)
      # Index space delimiters within each parameter definition
      i <- gregexpr(" ", hdr[k])
      # Index comment delimiters
      j <- regexpr("//", hdr[k])
      # Parameter definitions have been observed with no comment delimiter
      # If comment delimiter absent, set comment location to one position beyond parameter def string length
      # Note that substr(a) returns "" for positions outside of 1:nchar(a)
      m <- which(j<0)
      if(length(m)>0)
        j[m] <- nchar(hdr[k[m]])+1
      # Require at least three space and one comment delimiters for each parameter definition
      if(length(i)==length(j) & all(unlist(lapply(i, length))>2) & all(j>0)) {
        # Iterate through all parameter definition elements and compose list of corresponding elements
        # Pass vector of positions of spaces for each definition (they delimit fields and/or values)
        pd <- lapply(1:length(k),
                     function(m) {
                       # Properly formatted parameter definition entries have the structure:
                       # Section DataType Name= Value DefaultValue LowRange HighRange // Comment
                       # Example: Visualize:Processing%20Stages int VisualizeTransmissionFilter= 0 0 0 1 // Visualize TransmissionFilter output (boolean)
                       # Notes:
                       # Section may contain colon delimited, hierarchical subsections
                       # Spaces in section names are encoded as %20
                       # Observed DataType entries:
                       #   int and float ... a four position value vector, one position for each of Value,
                       #                     DefaultValue, LowRange, and HighRange, with empty positions
                       #                     encoded as a single %
                       #   list ............ structure: n = number of entries in list, n values, default value, min, max
                       #   intList ......... structure: list containing int values
                       #   floatlist ....... structure: list containing float values
                       #   stringlist ...... structure: list containing string values
                       #   matrix .......... structure: n = number of values, m = number of elements (that follow) composing
                       #                     each value, n X m elements, default value, min value, max value
                       #   string .......... structure: four positions containing Value, DefaultValue, %, %
                       #
                       # Parse section, data type, name, and value using first three space delimiters
                       # Parse comment using the comment delimiter
                       # Values such as "1 0 % %" have been observed for type intlist, which appears as a standard
                       # value, default, min, max value, but is interpreted as a single 0
                       # Values such as "4 0 0 0 0 0 % %" have been observed for floatlist; the four 0s in positions
                       # 2-5 are retained
                       val <- ""
                       dt <- substr(hdr[k[m]], i[[m]][1]+1, i[[m]][2]-1)
                       if(length(grep("list", dt, fixed=T))==1) {
                         # Examine first value and return that number of trailing values
                         val0 <- strsplit(substr(hdr[k[m]], i[[m]][3]+1, j[m]-2), " ")[[1]]
                         n1 <- length(val0)
                         if(n1>1)
                           if(isnum(val0[1])) {
                             n2 <- as.numeric(val0[1])
                             if(n2>0) {
                               if(n1>n2) {
                                 val <- val0[2:(1+n2)]
                               } else {
                                 val <- c(val0[2:n1], rep("", n2-n1+1))
                               }
                             }
                           }
                       } else if(length(grep("matrix", dt, fixed=T))==1) {
                         # Examine first (number of elements) and second (subelements within each element) values
                         # Return the first value of each subelement
                         val0 <- strsplit(substr(hdr[k[m]], i[[m]][3]+1, j[m]-2), " ")[[1]]
                         n1 <- length(val0)
                         if(n1>2)
                           if(isnum(val0[1]) & isnum(val0[2])) {
                             n2 <- as.numeric(val0[1])
                             n3 <- as.numeric(val0[2])
                             if(n2>0 & n3>0 & n1>n2*n3)
                               val <- apply(as.matrix(seq(3, 2+n2*n3, n3)), 1, function(i2) val0[i2])
                           }
                       } else {
                         # Scalar
                         # Retain the first element of the value list
                         val <- strsplit(substr(hdr[k[m]], i[[m]][3]+1, j[m]-2), " ")[[1]][1]
                       }
                       # Parse parameter definition elements
                       # Note that substr(a) produces "" for positions outside of 1:nchar(a)
                       list("section"=strsplit(gsub("%20", "", substr(hdr[k[m]], 1, i[[m]][1]-1)), ":")[[1]],
                            "dataType"=dt,
                            "name"=sub("=", "", substr(hdr[k[m]], i[[m]][2]+1, i[[m]][3]-1)),
                            "value"=val,
                            "comment"=substr(hdr[k[m]], j[m]+3, nchar(hdr[k[m]])))
                     })
        names(pd) <- unlist(lapply(1:length(pd),
                                   function(i)
                                     paste(paste(pd[[i]][["section"]], collapse=":", sep=""), ":", pd[[i]][["name"]], sep="")))
      } else {
        pd <- list()
      }
    } else {
      pd <- list()
    }

    # Validate:

    # Inspect data types
    # unique(unlist(lapply(pd, function(a) a[["dataType"]])))

    # Retrieve EEG signal shift and scale values (one per signal)
    # Multiple parameter definition names have been observed for these, although a single definition exists in any one file
    # Search for known parameter defs
    # If none are found, zero-length vectors result
    eegOffset <- as.numeric(pd[["Source:SignalProperties:DataIOFilter:SourceChOffset"]][["value"]])
    if(length(eegOffset)==0)
      eegOffset <- as.numeric(pd[["Filtering:DataIOFilter:SourceChOffset"]][["value"]])
    eegGain <- as.numeric(pd[["Source:SignalProperties:DataIOFilter:SourceChGain"]][["value"]])
    if(length(eegGain)==0)
      eegGain <- as.numeric(pd[["Filtering:DataIOFilter:SourceChGain"]][["value"]])

    # Proceed if number of offsets and gains = number of signals
    if(length(eegOffset)==meta[["SourceCh"]] & length(eegGain)==meta[["SourceCh"]]) {

      # Extract binary EEG signal data from file (beginning at position HeaderLen+1)
      # Note that bit order of source bytes is maintained (ASCII "A" stored as raw 41 hex, "h" as 68 hex)
      x <- readr::read_file_raw(f)
      x <- x[(meta[["HeaderLen"]]+1):length(x)]

      # Number of bytes per signal per sample and total bytes per sample
      # Two-byte integers for int16 format, 4 byte integers for 1nt32, or 4 byte floating point for float32
      # All data types treated as little-endian
      # Include bytes for the state vector
      if(meta[["DataFormat"]]=="int16") {
        nbsig <- 2
        sigfmt <- "integer"
      } else if(meta[["DataFormat"]]=="int32") {
        nbsig <- 4
        sigfmt <- "integer"
      } else if(meta[["DataFormat"]]=="float32") {
        nbsig <- 4
        sigfmt <- "numeric"
      } else {
        nbsig <- -1
        sigfmt <- NA
      }
      nbsamp <- nbsig*meta[["SourceCh"]]+meta[["StatevectorLen"]]

      # Number of samples
      # Note that, for some BCI files, nsamp is non-integer
      nsamp <- length(x)/nbsamp

      # Verify: nsamp an integer
      #nsamp-as.integer(nsamp)

      ###########################################################################################################################
      # Compose a list consististing of four elements:
      # eegVal ......... A matrix of dimension nSamples X nEEGSignals, with one column per EEG signal, each containing an
      #                  integer in the range [-32768, 32767] corresponding to each signal value
      #                  Integer (int16 and int32 data formats) are assumed to be two's complment (signed) values
      # eegProp ........ A data frame with one column per EEG signal, each containing parameters for converting integer
      #                  signals to corresponding real number (physical) values
      # etVal .......... A matrix of dimension nSamples X nETVariables, with one column per computed eye tracker variable
      # etProp ......... A data frame with one column per eye tracker variable, each containing parameters for converting
      #                  integer signals to corresponding real number (physical) values
      # p300Val ........ A matrix of dimension nSamples X nCharInSpellerGrid, with one column per character, each row
      #                  of each column containing either 0 or 1, indicating whether or not the corresponding character (col)
      #                  was active for the corresponding sample (row)
      # p300Prop ....... A data frame with one column per p300Val column, each containing parameters for converting
      #                  integer signals to corresponding real number (physical) values
      ###########################################################################################################################

      #######################################################################################
      # Compose EEG signal matrix (eegVal)
      #######################################################################################

      print("EEG signal matrix composition")

      if(meta[["SourceCh"]]>0 & meta[["DataFormat"]] %in% c("int16", "int32", "float32")) {

        # Note that all channels are returned, regardless of the contents of the transmit channel list parameter
        # Configure pointers to first byte (0-based) of each signal within a sample
        psig <- seq(0, nbsig*(meta[["SourceCh"]]-1), nbsig)
        psamp <- seq(0, nbsamp*(nsamp-1), nbsamp)
        # Iterate through each signal
        # Configure pointer (p0) to first byte of signal within each sample
        eegVal <- matrix(apply(as.matrix(psig), 1,
                               function(p0)
                                 # Iterate through each sample
                                 # Configure pointer (p1) to current signal in each sample
                                 # Extract bytes and adjust for two's complement
                                 apply(as.matrix(p0+psamp), 1,
                                       function(p1)
                                         # Explicit interpretation
                                         #if(meta[["DataFormat"]]=="int16") {
                                         #  z <- as.integer(x[p1+1])+256*as.integer(x[p1+2])
                                         # Convert from two's complement
                                         #if(z>32767)
                                         #  z <- z-65536
                                         #} else if(meta[["DataFormat"]]=="int32") {
                                         #z <- as.integer(x[p1+1])+256*as.integer(x[p1+2])+
                                         #     65536*as.integer(x[p1+3])+16777216*as.integer(x[p1+4])
                                         # Four byte integers in R are read strictly as signed
                                         # Convert from two's complement
                                         #if(z>2147483648)
                                         #  z <- z-4294967296
                                         #} else if(meta[["DataFormat"]]=="float32") {
                                         #z <- readBin(x[p1+1:4], what="numeric", size=4, n=1, endian="little")
                                         #}
                                       # R interpretation
                                       readBin(x[p1+1:nbsig], what=sigfmt, size=nbsig, n=1, signed=T, endian="little"))),
                         ncol=meta[["SourceCh"]])

        # Specify EEG signal physical and digital min and max
        # Physical min/max represent the real-valued range that actual measurements are expected to reside within
        # Digital min/max represent the range of integers that discretized (integer) values corresponding to real
        # values reside within and are set to the range of values that can be represented with a two byte, signed integer
        if(meta[["DataFormat"]] %in% c("int16", "int32")) {
          pMin <- eegOffset-32768*eegGain
          pMax <- eegOffset+32767*eegGain
        } else {
          # DataFormat is expected to be float32, but use observed min/max for all remaining formats
          # Apply offsets and gains
          eegVal <- apply(as.matrix(1:ncol(eegVal)), 1, function(j) eegOffset[j]+eegGain[j]*eegVal[,j])
          pMin <- apply(eegVal, 2, min)
          pMax <- apply(eegVal, 2, max)
        }

        # Compose EEG signal property data frame
        eegProp <- data.frame(
          "pMin"=pMin,
          "pMax"=pMax,
          "dMin"=rep(-32768, ncol(eegVal)),
          "dMax"=rep(32767, ncol(eegVal)),
          # Compose EEG transducer type and physical dimension vectors
          # Transducer and dimension element positions correspond to signal matrix columns
          "tdc"=rep("AgAgCl electrode", ncol(eegVal)),
          "dim"=rep("uV", ncol(eegVal)))

        # Map int32 and float32 values to two byte, signed integers on [-32768, 32767]
        # Result is an nsamp X nsig matrix of integers, one col per signal
        # Coerce matrix result in case number of signal is 1 (in which case apply() returns a vector)
        if(meta[["DataFormat"]]=="int32") {
          eegVal <- matrix(apply(eegVal, 2,
                                 function(x)
                                   # Compute interval that each x belongs to
                                   # Since 2**32/2**16=2**16, there are 2**16=65536 intervals, each of length 2**16
                                   # Note that negative values are shifted negative one interval
                                   # This is due to, for instance, all x on [-65535, 65535] mapping to 0
                                   as.integer(ifelse(x>=0, x/65536, (x-65535)/65536))),
                           ncol=ncol(eegVal))
        } else if(meta[["DataFormat"]]=="float32") {
          eegVal <- matrix(apply(as.matrix(1:ncol(eegVal)), 1,
                                 function(j) {
                                   # Adjust any physical values outside of [pmin, pmax]
                                   # This prevents digital values outside of two-byte integer range
                                   x <- eegVal[,j]
                                   x[which(x<eegProp[j,"pMin"])] <- eegProp[j,"pMin"]
                                   x[which(x>eegProp[j,"pMax"])] <- eegProp[j,"pMax"]
                                   # Compute proportion of [pmin, pmax] occupied by x then map proportion to
                                   # integer on [dmin, dmax] corresponding to dmax-dmin uniform divisions
                                   # of [pmin, pmax]
                                   y <- eegProp[j,"dMin"]+as.integer((x-eegProp[j,"pMin"])/
                                                                     (eegProp[j,"pMax"]-eegProp[j,"pMin"])*
                                                                     (eegProp[j,"dMax"]-eegProp[j,"dMin"])+0.5)
                                   # Adjust any digital values equal to dMax+1
                                   # These correspond to physical values equal to pMax
                                   y[which(y>eegProp[j,"dMax"])] <- eegProp[j,"dMax"]
                                   return(y)
                                 }),
          ncol=ncol(eegVal))
        }

      } else {
        eegVal <- matrix()
        eegProp <- data.frame()
        warning("Error-10 in function bciRetrieve: No channels specified or unknown data format in BCI file")
      }

      #######################################################################################
      # Parse state vectors
      # Result is an nSample X stateVectorLength matrix of raw vectors, row i the state vector bytes of sample i
      #######################################################################################
      stv <- t(apply(as.matrix(1:nsamp), 1, function(i)
                                              # State vector (follows last EEG signal)
                                              x[(i*nbsamp-meta[["StatevectorLen"]]+1):(i*nbsamp)]))

      #######################################################################################
      # Compose eye tracker value matrix (etVal)
      # Note that 0-valued eye tracker columns are generated for any variables that are
      # absent in input BCI file
      #######################################################################################

      # Retrieve eye tracker enabled status
      k <- grep("LogEyetracker", names(pd))
      if(length(k)>0) {
        # Select first one <> BCI empty string (%)
        a <- unlist(lapply(pd[k], function(b) b[["value"]]))
        k <- which(a!="%")
        if(length(k)>0) {
          # Convert 1=enabled, 0=disabled
          if(a[k[1]]=="1") {
            etEnabled <- T
          } else {
            etEnabled <- F
          }
        } else {
          etEnabled <- F
        }
      } else {
        etEnabled <- F
      }

      if(etEnabled) {

        print("Eye tracker signal matrix composition")
       
        # Specify eye tracker variables to extract from the state vector
        etVar <- c("EyetrackerStatesOK",
                   "EyetrackerLeftEyeValidity", "EyetrackerRightEyeValidity",
                   "EyetrackerLeftEyeGazeX", "EyetrackerLeftEyeGazeY",
                   "EyetrackerRightEyeGazeX", "EyetrackerRightEyeGazeY",
                   "EyetrackerLeftPupilSize", "EyetrackerRightPupilSize",
                   "EyetrackerLeftEyePosX", "EyetrackerLeftEyePosY",
                   "EyetrackerRightEyePosX", "EyetrackerRightEyePosY",
                   "EyetrackerLeftEyeDist", "EyetrackerRightEyeDist")
  
        # Verify existence of eye tracker variables in state vector definition
        etvExist <- unlist(lapply(etVar, function(v) v %in% names(sv)))
        names(etvExist) <- etVar
        k <- which(!etvExist)
        if(length(k)>0)
          warning(paste("Error-10 in function bciRetrieve. Eye tracker variable(s) missing in BCI file: ",
        paste(etVar[k], collapse=", ", sep=""), sep=""))
  
        # Parse eye tracker values from state vectors
        # Result is an nSamp X nETVar matrix of integer values
        # Note the options for two's complement interpretation and diminishing (shifting down) values by 2**(bitLen-1)
        # A vector of nsamp NAs is returned for any eye tracker variable not defined in the input state vector definition
        etVal <- apply(as.matrix(etVar), 1,
                       function(v) {
                         # Retrieve state vector variable bit length (element 1), sample value (element 2),
                         # starting byte position (element 3), and starting bit position (element 4)
                         k <- which(names(sv)==v)
                         if(length(k)>0) {
                           if(!any(is.na(sv[[k]]))) {
                             bciStateVectorExtract(
                               svDat=stv,
                               bytePos=sv[[k]][3],
                               bitPos=sv[[k]][4],
                               bitLen=sv[[k]][1],
                               convertInt=T,
                               twosComp=F,
                               unsignedShift=F)
                           } else {
                             rep(NA, nsamp)
                             warning(paste("Error-10 in function bciRetrieve: NAs detected in Eye Tracker state vector variable definition for ", v, sep=""))
                           }
                         } else {
                           rep(NA, nsamp)
                         }
                       })
        colnames(etVal) <- etVar
  
        # Compose eye tracker signal matrix
        etVal <- cbind("ETLeftEyeValid"=ifelse(!is.na(etVal[,"EyetrackerStatesOK"]) & !is.na(etVal[,"EyetrackerLeftEyeValidity"]),
                                               ifelse(etVal[,"EyetrackerLeftEyeValidity"]<2, 1, 0), 0),
                       "ETRightEyeValid"=ifelse(!is.na(etVal[,"EyetrackerStatesOK"]) & !is.na(etVal[,"EyetrackerRightEyeValidity"]),
                                                ifelse(etVal[,"EyetrackerRightEyeValidity"]<2, 1, 0), 0),
                       "ETLeftEyeGazeX"=ifelse(!is.na(etVal[,"EyetrackerLeftEyeGazeX"]), etVal[,"EyetrackerLeftEyeGazeX"], 0),
                       "ETLeftEyeGazeY"=ifelse(!is.na(etVal[,"EyetrackerLeftEyeGazeY"]), etVal[,"EyetrackerLeftEyeGazeY"], 0),
                       "ETRightEyeGazeX"=ifelse(!is.na(etVal[,"EyetrackerRightEyeGazeX"]), etVal[,"EyetrackerRightEyeGazeX"], 0),
                       "ETRightEyeGazeY"=ifelse(!is.na(etVal[,"EyetrackerRightEyeGazeY"]), etVal[,"EyetrackerRightEyeGazeY"], 0),
                       "ETLeftPupilSize"=ifelse(!is.na(etVal[,"EyetrackerLeftPupilSize"]), etVal[,"EyetrackerLeftPupilSize"], 0),
                       "ETRightPupilSize"=ifelse(!is.na(etVal[,"EyetrackerRightPupilSize"]), etVal[,"EyetrackerRightPupilSize"], 0),
                       "ETLeftEyePosX"=ifelse(!is.na(etVal[,"EyetrackerLeftEyePosX"]), etVal[,"EyetrackerLeftEyePosX"], 0),
                       "ETLeftEyePosY"=ifelse(!is.na(etVal[,"EyetrackerLeftEyePosY"]), etVal[,"EyetrackerLeftEyePosY"], 0),
                       "ETRightEyePosX"=ifelse(!is.na(etVal[,"EyetrackerRightEyePosX"]), etVal[,"EyetrackerRightEyePosX"], 0),
                       "ETRightEyePosY"=ifelse(!is.na(etVal[,"EyetrackerRightEyePosY"]), etVal[,"EyetrackerRightEyePosY"], 0),
                       "ETLeftEyeDist"=ifelse(!is.na(etVal[,"EyetrackerLeftEyeDist"]), etVal[,"EyetrackerLeftEyeDist"], 0),
                       "ETRightEyeDist"=ifelse(!is.na(etVal[,"EyetrackerRightEyeDist"]), etVal[,"EyetrackerRightEyeDist"], 0))
  
        # Retrieve bit lengths for eye tracker variables
        # These are used to compute feasible ranges of observed values
        # The first element of a state vector definition contains bit length
        bitlen <- c(
          # Left/Right eye validity are computed as either 0 or 1 and expressed by a single bit
          1,
          1,
          # Retrieve bit lengths for remaining variables, in order of appearance in etVal
          ifelse(etvExist["EyetrackerLeftEyeGazeX"],   sv[["EyetrackerLeftEyeGazeX"]][1],   NA),
          ifelse(etvExist["EyetrackerLeftEyeGazeY"],   sv[["EyetrackerLeftEyeGazeY"]][1],   NA),
          ifelse(etvExist["EyetrackerRightEyeGazeX"],  sv[["EyetrackerRightEyeGazeX"]][1],  NA),
          ifelse(etvExist["EyetrackerRightEyeGazeY"],  sv[["EyetrackerRightEyeGazeY"]][1],  NA),
          ifelse(etvExist["EyetrackerLeftPupilSize"],  sv[["EyetrackerLeftPupilSize"]][1],  NA),
          ifelse(etvExist["EyetrackerRightPupilSize"], sv[["EyetrackerRightPupilSize"]][1], NA),
          ifelse(etvExist["EyetrackerLeftEyePosX"],    sv[["EyetrackerLeftEyePosX"]][1],    NA),
          ifelse(etvExist["EyetrackerLeftEyePosY"],    sv[["EyetrackerLeftEyePosY"]][1],    NA),
          ifelse(etvExist["EyetrackerRightEyePosX"],   sv[["EyetrackerRightEyePosX"]][1],   NA),
          ifelse(etvExist["EyetrackerRightEyePosY"],   sv[["EyetrackerRightEyePosY"]][1],   NA),
          ifelse(etvExist["EyetrackerLeftEyeDist"],    sv[["EyetrackerLeftEyeDist"]][1],    NA),
          ifelse(etvExist["EyetrackerRightEyeDist"],   sv[["EyetrackerRightEyeDist"]][1],   NA))
  
        # Configure eye tracker variable property data frame
        etgOffset <- as.numeric(pd[["Source:Eyetracker:EyetrackerLogger:GazeOffset"]][["value"]])
        if(length(etgOffset)==0) {
          etgOffset <- 0
          warning("Error-10 in function bciRetrieve: Missing Eye Tracker gaze offset in BCI file - eye gaze physical min/max not computed")
        }
        etgScale <- as.numeric(pd[["Source:Eyetracker:EyetrackerLogger:GazeScale"]][["value"]])
        if(length(etgScale)==0) {
          etgScale <- 0
          warning("Error-10 in function bciRetrieve: Missing Eye Tracker gaze scale in BCI file - eye gaze physical min/max not computed")
        }
  
        etProp <- data.frame(
          row.names=colnames(etVal),
          # Specify eye tracker variable feasible range (based on bit length), physical range, and digital range
          # Feasible range is used to compute proportion of the range occupied by observed digital values
          # This proportion is applied to the specified digital range
          # Digital range is the interval of integer values that feasible range proportions map to
          # Digital values are used by external software (EDF Browser, for instance) to map observed values
          # to physical values
          # Physical range is used by external software to map proportion of digital range occupied by
          # digital values to corresponding proprtion in the physical range
          # Physical min/max were provided by project researchers and provide consistent ranges between data
          # sets (individual data collection sessions)
          # Digital min/max is 0/1 for IsGazeValid (since it is binary)
          # For other variables, digital min is the max of -32768 and -2**(bitlen-1) and digital max is
          # the min of 32767 and 2**(bitlen-1)-1
          # Bounds of [-32768, 32767] are imposed for EDF compatibility and provide maximum resolution
          # using two-byte, signed integers, as in EDF
          "feasMin"=rep(0, length(bitlen)),
          "feasMax"=2**bitlen-1,
          "pMin"=c(0,
          0,
          ifelse(etgScale!=0, -etgOffset/etgScale, 0),
          ifelse(etgScale!=0, -etgOffset/etgScale, 0),
          ifelse(etgScale!=0, -etgOffset/etgScale, 0),
          ifelse(etgScale!=0, -etgOffset/etgScale, 0),
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0),
          "pMax"=c(1,
          1,
          ifelse(etgScale!=0, (1-etgOffset)/etgScale, 1),
          ifelse(etgScale!=0, (1-etgOffset)/etgScale, 1),
          ifelse(etgScale!=0, (1-etgOffset)/etgScale, 1),
          ifelse(etgScale!=0, (1-etgOffset)/etgScale, 1),
          2**bitlen["EyetrackerLeftPupilSize"]-1,
          2**bitlen["EyetrackerRightPupilSize"]-1,
          1,
          1,
          1,
          1,
          2**bitlen["EyetrackerLeftEyeDist"]-1,
          2**bitlen["EyetrackerRightEyeDist"]-1),
          "dMin"=-2**(bitlen-1),
          "dMax"=2**(bitlen-1)-1,
          # Specify truncation bounds
          # Values observed below dtruncLow will be converted to dtruncLow and those above dtruncHigh
          # will be converted to dtruncHigh
          "dtruncLow"=c(NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA),
          "dtruncHigh"=c(NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA,
          NA),
          # Compose transducer type and physical dimension vectors (one each for every computed eye tracker variable)
          "tdc"=c(rep("IR eyetracker", 14)),
          "dim"=c("bool", "bool", rep("mm", 12)))
  
        # Discretize computed eye tracker values
        # Rescale eye tracker signals to specified digital ranges
        v <- colnames(etVal)
        etVal <- apply(as.matrix(1:ncol(etVal)), 1,
                       function(j) {
                         x <- etVal[,j]
                         # Truncate observed values below or above truncation limits
                         if(!is.na(etProp[j,"dtruncLow"]))
                         x[which(x<etProp[j,"dtruncLow"])] <- etProp[j,"dtruncLow"]
                         if(!is.na(etProp[j,"dtruncHigh"]))
                         x[which(x>etProp[j,"dtruncHigh"])] <- etProp[j,"dtruncHigh"]
                         # Compute proportion of feasible range occupied by x then map proportion to integer on
                         # [dmin, dmax+1] corresponding to the feasible range proportion
                         y <- etProp[j,"dMin"]+
                              as.integer((x-etProp[j,"feasMin"])/(etProp[j,"feasMax"]-etProp[j,"feasMin"])*
                                         (etProp[j,"dMax"]+1-etProp[j,"dMin"]))
                         # Note that values observed at the feasible range upper bound are expected to
                         # have computed values of y=dMax+1
                         y[which(y<etProp[j,"dMin"])] <- etProp[j,"dMin"]
                         y[which(y>etProp[j,"dMax"])] <- etProp[j,"dMax"]
                         return(y)
                       })
        colnames(etVal) <- v

      } else {
        etVal <- NA
        etProp <- NA
      }

      #######################################################################################
      # Compose matrix of P300 Speller stimulus (target) indicators
      # Two paradigms are known to exist:  the row-column method and the checkerboard method
      # Row-column simultaneously "flashes" all symbols in a row or column of the speller grid
      # Checkerboard "flashes" a set of symbols from the total appearing on the grid, generally
      # from multiple rows and columns
      # In the row-column method, StimulusCode indicates which row or column is flashing
      # When StimulusCode <= nGridRows then a row is flashing and the row ID = StimulusCode
      # When StimulusCode > nGridRows then a column is flashing and the col ID = StimulusCode-nGridRows
      # In both cases, an nSample X nCharsInGrid matrix of 0/1 indicators is constructed, with row i,
      # column j indicating whether or not the jth symbol is flashing in the ith sample
      #######################################################################################

      print("P300 stimulus/target matrix composition")

      # Retrieve parameter definitions
      # Multiple values for some parameters have been observed in the collection of project BCI2000 files
      # In general, at most, a single parameter value has been observed in an individual file
      # Search for known parameter IDs
      # Missing parameters result in a NULL value
      # Speller paradigm parameter
      # Current supported paradigms are row-column, checkerboard, performance, and adaptive based
      # Note that checkerboard and performance based paradigms have identical target value structure
      # in source files observed
      # Search for explicit checkerboard "standard" configuration
      # Standard=1 implies row-column, 0 implies checkerboard
      k0 <- intersect(intersect(grep("Checkerboard", names(pd)), grep("P3Speller", names(pd))),
                                grep("Standard", names(pd)))
      # Search for checkerboard (including adaptive checkerboard) paradigm
      k1 <- intersect(grep("P3Speller|AdaptiveP300SpellerTask|AdaptiveP300SpellerTrainingTask", names(pd)),
                      grep("UseCheckerboard", names(pd)))
      # Search for performance based paradigm
      k2 <- intersect(grep("P3Speller|P300Speller", names(pd)),
                      grep("UseParadigmFile|UseAdaptiveParadigm", names(pd)))
      # Identify true/false values in paradigm parameters
      # Note that any() returns false when supplied an empty vector (parameter values not found)
      # The standard parameter may be missing for both row-col and checkerboard files
      # Checkerboard and/or paradigm file parameters may be missing for row-col files
      p00 <- any(unlist(lapply(k0, function(i) pd[[i]][["value"]][1]))=="0")
      p01 <- any(unlist(lapply(k0, function(i) pd[[i]][["value"]][1]))=="1")
      p10 <- any(unlist(lapply(k1, function(i) pd[[i]][["value"]][1]))=="0")
      p11 <- any(unlist(lapply(k1, function(i) pd[[i]][["value"]][1]))=="1")
      p20 <- any(unlist(lapply(k2, function(i) pd[[i]][["value"]][1]))=="0")
      p21 <- any(unlist(lapply(k2, function(i) pd[[i]][["value"]][1]))=="1")
      # Paradigm identification rule:
      # If Checkerboard:P3Speller:Standard=1 or
      #    Checkerboard:P3Speller:Standard=0 not found and
      #    P3Speller:UseCheckerboard=1 not found and 
      #    P3Speller:UseParadigmFile=1 not found then row-col=true
      # Else row-col=false
      # If (Checkerboard:P3Speller:Standard=0 or
      #     P3Speller:UseCheckerboard=1 or
      #     P3Speller:UseParadigmFile=1) and
      #    Checkerboard:P3Speller:Standard=1 not found then checkerboard=true
      # Else checkerboard=false
      p3SpellerRowCol <- p01 | !p00 & !p11 & !p21
      p3SpellerCheckerboard <- (p00 | p11 | p21) & !p01
      # Parameter Speller Definition
      k <- intersect(grep("Application:SpellerTargets|P3Speller", names(pd)), grep("TargetDefinitions|TargetDefinitionMatrix", names(pd)))
      if(length(k)>0) {
        spdef <- pd[[k[1]]][["value"]]
      } else {
        spdef <- NULL
      }
      # Parameter speller matrix number of rows
      k <- intersect(grep("Application:SpellerTargets|P3Speller", names(pd)), grep("NumMatrixRows", names(pd)))
      if(length(k)>0) {
        if(isnum(pd[[k[1]]][["value"]][1])) {
          nSpDefRow <- as.integer(pd[[k[1]]][["value"]][1])
        } else {
          nSpDefRow <- 0
        }
      } else {
        nSpDefRow <- 0
      }
      # Parameter speller matrix number of columns
      k <- intersect(grep("Application:SpellerTargets|P3Speller", names(pd)), grep("NumMatrixColumns", names(pd)))
      if(length(k)>0) {
        if(isnum(pd[[k[1]]][["value"]][1])) {
          nSpDefCol <- as.integer(pd[[k[1]]][["value"]][1])
        } else {
          nSpDefCol <- 0
        }
      } else {
        nSpDefCol <- 0
      }
      # Parameter DisplayResults
      # Assign 0 if non-existent
      k <- grep("DisplayResults", names(pd))
      if(length(k)>0) {
        if(isnum(pd[[k[1]]][["value"]][1])) {
          displayResults <- as.integer(pd[[k[1]]][["value"]][1])
        } else {
          displayResults <- 0
        }
      } else {
        displayResults <- 0
      }
      # The speller definition is a vector containing speller grid symbols
      # Elements (i-1)*nSpDefCol+1 through i*nSpDefCol correspond to row i of the actual speller matrix
      # presented to the subject during data collection
      # For instance, elements nSpDefCol+1 through 2*nSpDefCol correspond to row 2 of the presented matrix
      # Elements j, j+nSpDefRow, j+2*nSpDefRow, ... correspond to col j of the presented matrix
      # For instance, elements 2, 2+nSpDefRow, 2+2*nSpDefRow, ... correspond to col 2 of the presented matrix
      # It was asserted by one researcher that a colum-wise indexing paradigm was exhibited in certain of the
      # project files, but all files were inspected and no column-wise indexed files were found
      # It is assumed that all speller definitions are row-wise indexed, as described above
      # Given NumMatrixRows=9 and NumMatrixColumns=8, a typical grid is:
      # A B C D E F G H
      # I J K L M N O P
      # Q R S T U V W X
      # Y Z ...
      # Speller definition indices and associated symbols are:  1=A, 2=B, ..., 9=I, 10=J, ...
      # Impose assumption of row-wise indexing
      spellerGridRowWise <- T
      # Require consistency of speller definition vector length and stated number of rows and cols
      w <- ""
      if(!is.null(spdef) & nSpDefRow>0 & nSpDefCol>0) {
        if(length(spdef)==nSpDefRow*nSpDefCol) {
          # Extract stimulus code values for each sample
          # Result is an nsamp vector of stimulus codes
          k <- which(names(sv)=="StimulusCode")
          if(length(k)>0) {
            if(!any(is.na(sv[[k]]))) {
              stimCode <- as.vector(bciStateVectorExtract(
                                      svDat=stv,
                                      bytePos=sv[[k]][3],
                                      bitPos=sv[[k]][4],
                                      bitLen=sv[[k]][1],
                                      convertInt=T,
                                      twosComp=F,
                                      unsignedShift=F))
              # Construct stimulus indicator matrix
              if(p3SpellerCheckerboard & !p3SpellerRowCol) {
                # Checkerboard method
                # Use Targ00-Targnn to identify active (flashing) symbols
                nm <- names(sv)
                k <- grep("Targ[0-9][0-9]", nm)
                if(length(k)>1) {
                  k <- k[order(nm[k])]
                  if(nm[k[1]]=="Targ00") {
                    # Extract stimulus indices from Targ00-Targnn state vector variables
                    # Result is an nSamp X (nTarg+1) matrix of indices into the P300 speller definitions vector
                    # Targ00 contains the number of active stimulii
                    # Targ01-Targnn contain indices in the speller definition vector for active stimulus symbols
                    jtg <- apply(as.matrix(k), 1,
                                 function(i) {
                                   # Extract state vector data corresponding to current variable's bit length (element 1),
                                   # sample value (element 2), starting byte position (element 3), and starting bit
                                   # position (element 4)
                                   if(!any(is.na(sv[[i]]))) {
                                     bciStateVectorExtract(
                                       svDat=stv,
                                       bytePos=sv[[i]][3],
                                       bitPos=sv[[i]][4],
                                       bitLen=sv[[i]][1],
                                       convertInt=T,
                                       twosComp=F,
                                       unsignedShift=F)
                                   } else {
                                     rep(NA, nsamp)
                                     warning(paste("Error-20 in function bciRetrieve: NAs detected in P300 state vector definition for ", nm[k], sep=""))
                                   }
                                 })
                    # Col 1 of stimulus indicator matrix contains the count of non-zero indices in corresponding rows
                    # Require all col 1 values <= number of indicator cols
                    if(all(jtg[,1]<ncol(jtg))) {
                      # Require all stimulus indices to be <= length of speller grid (in vector format)
                      if(max(jtg[,2:ncol(jtg)])<=length(spdef)) {
                        # Create a vector of zeros and assign a 1 to each position indexed in each sample
                        x <- as.integer(rep(0, length(spdef)))
                        # Iterate through each row of indices
                        # Column 1 contains the count of non-zero indices in the corresponding row
                        # Use indices appearing in cols 2:end to set columns of p300Val to 1
                        # Result is an nSamp X nSpellerChar matrix of 0/1 indicators, col j corresponding to
                        # the jth speller symbol
                        # Note all positions of x contain a zero at the beginning of each iteration, since a local copy is made
                        p300Val <- t(apply(as.matrix(1:nrow(jtg)), 1,
                                           function(i) {
                                             # Col 1 contains the count of non-zero indices in the current row
                                             # If col 1 contains n then it is expected that the first n indicator columns are non-zero
                                             # Columns with index > n have been observed containing non-zero indicator values
                                             # Limit retrieval to n indicator cols
                                             # Note that valid (active) stimulus rows require corresponding stimulus code > 0
                                             if(stimCode[i]>0 & jtg[i,1]>0) {
                                               # Retrieve index speller indices (cols 2 through n) and set corresponding pos to 1
                                               k <- jtg[i,2:(jtg[i,1]+1)]
                                               x[k[which(k>0)]] <- 1
                                             }
                                             return(x)
                                           }))
                      } else {
                        w <- "Stimulus index observed that exceeds speller grid dimensions - P300 stimulii not included in output"
                      }
                    } else {
                      w <- "Targ00 value observed > number of indicator columns - P300 stimulii not included in output"
                    }
                  } else {
                    w <- "Missing Targ00 definition in state vector of BCI file - P300 stimulii not included in output"
                  }
                } else {
                  w <- "Missing Targ00-Targnn definition in state vector of BCI file - P300 stimulii not included in output"
                }
              } else if(p3SpellerRowCol & !p3SpellerCheckerboard) {
                # Row-column method
                # Stimulus code is a vector of integer row or column IDs, ith element corresponding to sample i
                if(max(stimCode)<=nSpDefRow+nSpDefCol) {
                  # Compose indicator vectors for each possible stimulus code (one for each row and column)
                  # These are indices into the speller definition vector for the ith row or jth column
                  # Row i includes speller def indices (i-1)*ncol+1 through i*ncol, while col j consists of indices j, j+ncol, j+2*ncol, ...
                  # For instance, row 2 consists of indices 9 through 16, while col 5 consists of 5, 13, 21, ...
                  # By-row = T produces a row-wise matrix of indices, as above, while F would produce a column-wise matrix
                  spellerGridIndex <- matrix(1:(nSpDefRow*nSpDefCol), nrow=nSpDefRow, byrow=spellerGridRowWise)
                  # Construct indicator variables for each sample and element in the speller grid
                  # Note that x, being local to the function below, begins as a vector of zeros in each iteration
                  x <- as.integer(rep(0, length(spdef)))
                  p300Val <- t(apply(as.matrix(stimCode), 1,
                                     function(k) {
                                       if(k>0)
                                         if(k<=nSpDefRow) {
                                           x[spellerGridIndex[k,]] <- 1
                                         } else {
                                           x[spellerGridIndex[,k-nSpDefRow]] <- 1
                                         }
                                       return(x)
                                     }))
                } else {
                  w <- "Stimulus code observed > number of speller grid rows + columns - P300 stimulii not included in output"
                }
              } else {
                w <- "Conflicting or missing P3 speller paradigm parameters - P300 stimulii not included in output"
              }
            } else {
              w <- "NAs detected in BCI state vector definition for stimulus code - P300 stimulii not included in output"
            }
          } else {
            w <- "Missing state vector definition for stimulus code - P300 stimulii not included in output"
          }
        } else {
          w <- "Speller definition vector length <> specified rows X columns - P300 stimulii not included in output"
        }
      } else {
        w <- "Missing speller definition or invalid number of rows or columns- P300 stimulii not included in output"
      }
      if(w=="") {
        # Compose column names from speller grid definition
        # Name format is [symbols from speller def]_row_col
        # Note that, if no warnings (w=="") then length(spdef)==nSpDefRow*nSpDefCol
        # Compose speller grid element names as symbol_row_col
        # Speller definition positions correspond to stimulus indicator matrix columns
        if(spellerGridRowWise) {
          # Speller definition is in row order, the first ncol symbols of spdef appear in row 1 of the grid,
          # symbols ncol+1 through 2*ncol appear in row 2, etc.
          nm <- apply(as.matrix(1:length(spdef)), 1,
                      function(i)
                        paste(spdef[i], "_", as.integer((i-1)/nSpDefCol)+1, "_", (i-1)%%nSpDefCol+1, sep=""))
        } else {
          # Speller definition is in column order, the first nrow symbols appear in col 1 of the grid,
          # symbols nrow+1 through 2*nrow appear in col 2, etc.
          nm <- apply(as.matrix(1:length(spdef)), 1,
                      function(i)
                        paste(spdef[i], "_", (i-1)%%nSpDefRow+1, "_", as.integer((i-1)/nSpDefRow)+1, sep=""))
        }
        if(length(nm)==ncol(p300Val)) {
          colnames(p300Val) <- nm
        } else {
          warning("Error-10 in function bciRetrieve: Dimension of speller definition different from number of stimulus columns - stimulus columns will not be named")
        }
      } else {
        # Return a single vector of zeros
        p300Val <- matrix(rep(0, nsamp), ncol=1)
        colnames(p300Val) <- "x"
        warning(paste("Error-10 in function bciRetrieve: ", w, sep=""))
      }

      # Compose P300 speller matrix properties
      p300Prop <- data.frame(
        "pMin"=rep(0, ncol(p300Val)),
        "pMax"=rep(1, ncol(p300Val)),
        "dMin"=rep(0, ncol(p300Val)),
        "dMax"=rep(1, ncol(p300Val)),
        "tdc"=rep("P300 speller", ncol(p300Val)),
        "dim"=rep("bool", ncol(p300Val)))

      # Retrieve P300 target variable data
      targVar <- c("StimulusType", "SelectedTarget", "SelectedRow", "SelectedColumn", "PhaseInSequence", "StimulusBegin", "StimulusCode")

      # Verify existence of target variables
      targvExist <- unlist(lapply(targVar, function(v) v %in% names(sv)))
      names(targvExist) <- targVar
      k <- which(!targvExist)
      if(length(k)>0)
        warning(paste("Error-10 in function bciRetrieve. Target variable(s) missing in P300 state vector: ", paste(targVar[k], collapse=", ", sep=""), sep=""))

      # Parse target values from state vectors
      # Result is an nSamp X nTargVar matrix of integer values
      # A vector of nsamp NAs is returned for any target variable not defined in the input state vector definition
      targVal <- apply(as.matrix(targVar), 1,
                       function(v) {
                         # Retrieve state vector variable bit length (element 1), sample value (element 2),
                         # starting byte position (element 3), and starting bit position (element 4)
                         k <- which(names(sv)==v)
                         if(length(k)>0) {
                           if(!any(is.na(sv[[k]]))) {
                             bciStateVectorExtract(
                               svDat=stv,
                               bytePos=sv[[k]][3],
                               bitPos=sv[[k]][4],
                               bitLen=sv[[k]][1],
                               convertInt=T,
                               twosComp=F,
                               unsignedShift=F)
                           } else {
                             rep(NA, nsamp)
                             warning(paste("Error-20 in function bciRetrieve: NAs detected in P300 state vector variable definition for ", v, sep=""))
                           }
                         } else {
                           rep(NA, nsamp)
                         }
                       })
      colnames(targVal) <- c("StimulusType", "SelectedTarget", "SelectedRow", "SelectedColumn", "PhaseInSequence", "StimulusBegin", "StimulusCode") # StimulusBegin -crf37

      # Convert NAs to 0
      targVal <- apply(targVal, 2, function(x) {
                                     x[which(is.na(x))] <- 0
                                     return(x)
                                   })

      #######################################################################################
      # Construct current target vector
      #######################################################################################

      # Search for interpret mode parameter(s)
      k <- grep("InterpretMode", names(pd))
      if(length(k)>0) {
        # Select first one <> BCI empty string (%)
        a <- unlist(lapply(pd[k], function(b) b[["value"]]))
        k <- which(a!="%")
        if(length(k)>0) {
          # Convert 1 = online free mode, 2 = copy mode, 0 = none
          if(a[k[1]]=="1") {
            # Subject spells from imagination, as opposed to copying supplied text
            interpretMode <- "free"
          } else if(a[k[1]]=="2") {
            # Subject expected to spell what is being prompted (primarily for testing)
            interpretMode <- "copy"
          } else {
            interpretMode <- "none"
          }
        } else {
          interpretMode <- "none"
        }
      } else {
        interpretMode <- "none"
      }
      # Compose current target vector only in copy mode, since it is the only mode with pre-specified text to spell
      if(interpretMode=="copy") {
        # Construct vector consisting of one current target value per sample
        # Elements contain speller definition index values of active target symbols within each phase seq 2 interval
        # Search for text to spell parameter(s)
        k <- grep("TextToSpell", names(pd))
        if(length(k)>0) {
          # Select first one <> BCI empty string (%)
          a <- unlist(lapply(pd[k], function(b) b[["value"]]))
          k <- which(a!="%")
          if(length(k)>0) {
            textToSpell <- a[k[1]]
          } else {
            textToSpell <- "%"
          }
        } else {
          # Text to spell not specified, assign BCI empty string value
          textToSpell <- "%"
        }
        if(textToSpell!="%") {
          # Text to spell explicitly defined
          # Symbols are "selected" while PhaseInSequence=2 within the 1, 2, 3, 1 cycle
          # There should exist one cycle of PhaseInSequence for each symbol in the text to spell
          # Identify indices of samples where PhaseInSequence transitions to 2
          k <- 2:nsamp
          phseq0 <- k[which(targVal[,"PhaseInSequence"][k]==2 & targVal[,"PhaseInSequence"][k-1]!=2)]
          # Identify indices of samples where PhaseInSequence transitions from 2
          # Begin search at beginning initial phseq0 interval, since extraneous from-2 transitions have
          # been observed (reported to be files begun after terminating previous collection without
          # resetting the speller equipment)
          if(length(phseq0)>0) {
            k <- phseq0[1]:(nsamp-1)
            phseq1 <- k[which(targVal[,"PhaseInSequence"][k]==2 & targVal[,"PhaseInSequence"][k+1]!=2)]
          } else {
            phseq1 <- vector("numeric")
          }
          # Verify one transition interval for each symbol sought
          if(nchar(textToSpell)==length(phseq0) & length(phseq0)==length(phseq1)) {
            # Assign grid index of textToSpell symbols to corresponding intervals between transition
            # beginning and ending indices
            # All remaining elements will contain 0
            # Configure vector with no current targets
            x <- rep(0, nsamp)
            kmissing <- vector()
            # Compose vector of indices into speller symbols for each symbol of text to spell
            # Symbols that do not appear in speller grid and missing grid conditions cause NA values
            # Parse individual symbols
            a <- strsplit(textToSpell, "")[[1]]
            # Convert spaces to "Sp"
            a[which(a==" ")] <- "Sp"
            k <- match(a, spdef)
            # Iterate through symbols to spell
            # Assign speller index to corresponding target interval
            for(j in 1:length(k))
            if(!is.na(k[j])) {
              # Symbol to spell exists in speller definition
              # Assign index to all samples in current interval
              x[phseq0[j]:phseq1[j]] <- k[j]
            } else {
              # Symbol to spell not in speller def
              # Flag as missing
              kmissing <<- c(kmissing, j)
            }
            targVal <- cbind(targVal, x)
            colnames(targVal)[ncol(targVal)] <- "CurrentTarget"
            if(length(kmissing)>0)
              warning(paste("Error-10 in function bciRetrieve: Symbols ",
            paste(kmissing, collapse=", ", sep=""),
            " of text to spell missing in P300 speller definition - current targets not generated for these chars", sep=""))
          } else {
            targVal <- cbind(targVal, rep(0, nsamp))
            colnames(targVal)[ncol(targVal)] <- "CurrentTarget"
            warning("Error-10 in function bciRetrieve: Incompatible lengths of P300 text to spell and number of phase seq 2 transitions - current targets not generated")
          }
        } else {
          # Text to spell not provided
          # Infer current target text using phase sequence, flashing state, and stimulus indices previously derived (p300Val)
          # Flashing state, also referred to as on-target, is indicated by StimulusType=1
          # Note the assumption that p300Val, at this stage, contains no columns other than stimulus index columns
          # corresponding to speller grid symbols
          # Identify indices of samples where PhaseInSequence transitions to 2 or from 2
          k <- 2:nsamp
          phseq0 <- k[which(targVal[,"PhaseInSequence"][k]==2 & targVal[,"PhaseInSequence"][k-1]!=2)]
          k <- 1:(nsamp-1)
          phseq1 <- which(targVal[,"PhaseInSequence"][k]==2 & targVal[,"PhaseInSequence"][k+1]!=2)
          # Verify number of endpoints for each interval
          if(length(phseq0)>0 & length(phseq0)==length(phseq1)) {
            # For each sample in each phase sequence 2 interval, identify "flashing" target symbols
            # Cases for each interval:
            #   A single symbol is flashing throughout
            #     Assign CurrentTarget = that symbol
            #   Multiple symbols are flashing throughout
            #     Evaluate the flashing state of all indicated symbols outside of the interval
            #       If a single symbol does not have flashing status outside of the interval
            #         Assign CurrentTarget = that symbol
            #       Else
            #         Assign CurrentTarget = 0
            #         Generate warning
            # Configure vector with no current targets
            x <- rep(0, nsamp)
            for(j in 1:length(phseq0)) {
              # Index samples in phase sequence 2 interval with active flashing (on target) status
              i <- which(targVal[phseq0[j]:phseq1[j],"StimulusType"]==1)
              # Note that, if no samples in interval are flashing, current target remains 0 for all samples
              if(length(i)>0) {
                # Accumulate, for each speller symbol, the number of active (flashing) samples in interval
                # Note the use of drop=F to prevent [] from converting a 1-column matrix into a vector
                y <- apply(p300Val[(phseq0[j]:phseq1[j])[i],,drop=F], 2, sum)
                k <- which(y==length(i))
                # If only one symbol is flashing for all samples then use it
                if(length(k)==1) {
                  x[phseq0[j]:phseq1[j]] <- k
                } else if(length(k)==0) {
                  warning(paste("Error-10 in function bciRetrieve: Missing P300 target symbol - current target for PhaseSeq(2) interval ", j, " not returned", sep=""))
                } else {
                  # Multiple targets indicate flashing within current interval
                  # According to researchers, at most one target should actually flash at a given time
                  # Identify targets with flashing status within the interval, but not-flashing status outside
                  i2 <- setdiff(1:nsamp, phseq0[j]:phseq1[j])
                  y <- apply(p300Val[i2,k], 2, sum)
                  k2 <- which(y==0)
                  if(length(k2)==1) {
                    x[phseq0[j]:phseq1[j]] <- k[k2]
                  } else {
                    warning(paste("Error-10 in function bciRetrieve: Ambiguous P300 target symbol - current target for PhaseSeq(2) interval ", j, " not returned", sep=""))
                  }
                }
              }
            }
            targVal <- cbind(targVal, x)
            colnames(targVal)[ncol(targVal)] <- "CurrentTarget"
          } else {
            targVal <- cbind(targVal, rep(0, nsamp))
            colnames(targVal)[ncol(targVal)] <- "CurrentTarget"
            warning("Error-10 in function bciRetrieve: Incompatible lengths of P300 phase seq 2 transitions - current targets not generated")
          }
        }
      } else {
        # Construct null vector
        targVal <- cbind(targVal, rep(0, nsamp))
        colnames(targVal)[ncol(targVal)] <- "CurrentTarget"
      }

      #######################################################################################
      # Construct FakeFeedback vector
      # One position for each sample
      # Contains speller definition index of tested target within phase seq 3 intervals
      # Search for fake feedback parameter(s)
      # Modified from CurrentTarget definition -- crf37 07/01/22
      #######################################################################################
      k <- grep("FakeFeedback", names(pd))
      if(length(k)>0) {
        # Generate fake feedback vector when displayResults=1 or file is from ERN study with
        # feedback text length of 20
        # Rule provided by Duke BCI team, Oct 2020
        if(displayResults==1 | length(grep("data_2011fall_ern", f, ignore.case=T)>0) & nchar(pd[[k[1]]][["value"]][1])==20) {
          # Select first parameter def <> BCI empty string (%)
          a <- unlist(lapply(pd[k], function(b) b[["value"]]))
          k <- which(a!="%")
          if(length(k)>0) {
            ffeedback <- a[k[1]]
          } else {
            ffeedback <- "%"
          }
        } else {
          ffeedback <- "%"
        }
      } else {
        # Fake feedback not specified, assign BCI empty string value
        ffeedback <- "%"
      }
      # Configure default fake feedback vector
      x <- rep(0, nsamp)
      if(ffeedback!="%") {
        # Generate vector of fake feedback symbols
        # Symbols are "reported" while PhaseInSequence=3 within the 1, 2, 3, 1 cycle
        # There should exist one cycle of PhaseInSequence for each symbol in the fake feedback string
        # Identify indices of samples where PhaseInSequence transitions to 3 or from 3
        k <- 2:nsamp
        phseq0 <- k[which(targVal[,"PhaseInSequence"][k]==3 & targVal[,"PhaseInSequence"][k-1]!=3)]
        k <- 1:(nsamp-1)
        phseq1 <- which(targVal[,"PhaseInSequence"][k]==3 & targVal[,"PhaseInSequence"][k+1]!=3)
        # Verify one transition interval for each symbol sought
        if(nchar(ffeedback)==length(phseq0) & length(phseq0)==length(phseq1)) {
          # Assign grid index of feedback symbols to corresponding intervals between transition
          # beginning and ending indices
          # All remaining elements will contain 0
          kmissing <- vector()
          # Compose vector of indices into speller symbols for each symbol of fake feedback
          # Symbols that do not appear in speller grid and missing grid conditions cause NA values
          # Parse individual symbols
          a <- strsplit(ffeedback, "")[[1]]
          # Convert spaces to "Sp"
          a[which(a==" ")] <- "Sp"
          k <- match(a, spdef)
          # Iterate through symbol definitions
          # Assign speller index to corresponding target interval
          for(j in 1:length(k))
            if(!is.na(k[j])) {
              # Symbol to spell exists in speller definition
              # Assign index to all samples in current interval
              x[phseq0[j]:phseq1[j]] <- k[j]
            } else {
              # Symbol to spell not in speller def
              # Flag as missing
              kmissing <<- c(kmissing, j)
            }
          if(length(kmissing)>0)
              warning(paste("Error-10 in function bciRetrieve: P300 Symbols ",
                            paste(kmissing, collapse=", ", sep=""),
                            " of Fake Feedback missing in speller definition - Fake Feedback not generated for these chars", sep=""))
        } else {
          warning("Error-10 in function bciRetrieve: Incompatible lengths of P300 Fake Feedback and number of phase seq 3 transitions - Fake Feedback not generated")
        }
      }
      targVal <- cbind(targVal, x)
      colnames(targVal)[ncol(targVal)] <- "FakeFeedback"

      #######################################################################################
      # Construct Display Results
      # TRUE (1) for (DisplayResults=1 or FakeFeedback not empty) and PhaseInSequence=3
      # Otherwise FALSE (0)
      #######################################################################################
      if(displayResults==1 | ffeedback!="%") {
        x <- targVal[,"PhaseInSequence"]==3
      } else {
        x <- rep(0, nsamp)
      }
      targVal <- cbind(targVal, x)
      colnames(targVal)[ncol(targVal)] <- "DisplayResults"

      # REMOVED 07/01/2022 - CRF37
      # # Construct BCI selection vector
      # # These are the values (symbols, one per sample) inferred by the BCI algorithm as those selected by subject
      # Search for TextResult parameter(s)
      # k <- grep("TextResult", names(pd))
      # if(length(k)>0) {
      #   # Select first one <> BCI empty string (%)
      #   a <- unlist(lapply(pd[k], function(b) b[["value"]]))
      #   k <- which(a!="%")
      #   if(length(k)>0) {
      #     bciText <- a[k[1]]
      #   } else {
      #     bciText <- "%"
      #   }
      # } else {
      #   # Text to spell not specified, assign BCI empty string value
      #   bciText <- "%"
      # }
      # # If TextResult specified, parse symbols
      # if(bciText!="%") {
      #   # Compose vector of indices into speller symbols for each symbol of text to spell
      #   # symbols that do not appear in speller grid and missing grid conditions cause NA values
      #   kbciText <- match(strsplit(bciText, "")[[1]], spdef)
      #   exbciText <- T
      # } else {
      #   exbciText <- F
      # }
      # # Identify phase sequence 3 intervals (should be one for each TextResult char) and return corresponding char
      # k <- 2:nsamp
      # phseq0 <- k[which(targVal[,"PhaseInSequence"][k]==3 & targVal[,"PhaseInSequence"][k-1]!=3)]
      # k <- 1:(nsamp-1)
      # phseq1 <- which(targVal[,"PhaseInSequence"][k]==3 & targVal[,"PhaseInSequence"][k+1]!=3)
      # # Verify one transition interval per TextResult symbol or no TextResult supplied
      # if((!exbciText | nchar(bciText)==length(phseq0)) & length(phseq0)==length(phseq1)) {
      #   # For each sample in each phase sequence 3 interval, assign corresponding TextResult symbol,
      #   # if present, otherwise, assign selected target
      #   x <- rep(0, nsamp)
      #   kmissing <- vector()
      #   for(j in 1:length(phseq0)) {
      #     if(exbciText) {
      #       if(!is.na(kbciText[j])) {
      #         x[phseq0[j]:phseq1[j]] <- kbciText[j]
      #       } else {
      #         kmissing <<- c(kmissing, j)
      #       }
      #     } else {
      #       # Assign corresponding selected target values
      #       x[phseq0[j]:phseq1[j]] <- targVal[phseq0[j]:phseq1[j],"SelectedTarget"]
      #     }
      #   }
      #   targVal <- cbind(targVal, x)
      #   colnames(targVal)[ncol(targVal)] <- "BCISelection"
      #   if(length(kmissing)>0)
      #     warning(paste("Symbols ",
      #                   paste(kmissing, collapse=", ", sep=""),
      #                   " of TextResult missing in speller definition - BCISelection not generated for these chars", sep=""))
      # } else {
      #   targVal <- cbind(targVal, rep(0, nsamp))
      #   colnames(targVal)[ncol(targVal)] <- "BCISelection"
      #   warning("Incompatible lengths of TextResult and number of phase seq 3 transitions - BCISelection not generated")
      # }

      # REMOVED 07/01/2022 - CRF37
      # # Construct error vector
      # # Values indicate a discrepancy between selected and current targets
      # # Three values are defined:
      # # -1 ... phase seq not in state 3
      # #  0 ... phase seq in state 3, selected target <> current target
      # # 1 ... phase seq in state 3, selected target = current target
      # x <- rep(0, nsamp)
      # x[which(targVal[,"PhaseInSequence"]!=3)] <- -1
      # x[which(targVal[,"PhaseInSequence"]==3 & targVal[,"SelectedTarget"]!=0 &
      #         targVal[,"SelectedTarget"]==targVal[,"CurrentTarget"])] <- 1
      # targVal <- cbind(targVal, x)
      # colnames(targVal)[ncol(targVal)] <- "Error"

      #####################################################################################################################
      # Define properties for additional target value variables
      # targVal columns are:  StimulusType, SelectedTarget, SelectedRow, SelectedColumn, PhaseInSequence, StimulusBegin,
      #                       StimulusCode, CurrentTarget, FakeFeedback, DisplayResults
      #####################################################################################################################
      targProp <- data.frame(
        "pMin"=rep(0, 10),
        "pMax"=c(1, 32767, 255, 255, 3, 1, 32767, 32767, 32767, 1),
        "dMin"=rep(0, 10),
        "dMax"=c(1, 32767, 255, 255, 3, 1, 32767, 32767, 32767, 1),
        "tdc"=rep("P300 speller", 10),
        "dim"=c("bool", rep("int", 4), "bool", rep("int", 3), "bool"))

      #####################################################################################################################
      # Restrict additional target values to corresponding digital limits
      # Selected target should be bounded at speller grid nrow X ncol and should be well below 32767
      #####################################################################################################################
      nmt <- colnames(targVal)
      targVal <- apply(as.matrix(1:ncol(targVal)), 1,
                       function(j) {
                         x <- targVal[,j]
                         k <- which(x<targProp[j,"dMin"])
                         if(length(k)>0) {
                           x[k] <- targProp[j,"dMin"]
                           warning(paste("Error-20 in function bciRetrieve: ", length(k), " elements of P300 ", targVar[j], " below digital min - adjusted to digital min", sep=""))
                         }
                         k <- which(x>targProp[j,"dMax"])
                         if(length(k)>0) {
                           x[k] <- targProp[j,"dMax"]
                           warning(paste("Error-20 in function bciRetrieve: ", length(k), " elements of P300 ", targVar[j], " above digital max - adjusted to digital max", sep=""))
                         }
                         return(x)
                       })

      # Append additional target variables to P300 value matrix
      nmp <- colnames(p300Val)

      p300Val <- cbind(p300Val, targVal)
      colnames(p300Val) <- c(nmp, nmt)

      # Append additional target variable properties to P300 property data frame
      p300Prop <- rbind(p300Prop, targProp)

      ##############################################################################
      # Compose result list
      ##############################################################################
      return(list("meta"=meta,
                  "stateVector"=sv,
                  "parameterDef"=pd,
                  "signalDat"=list("eegVal"=eegVal, "eegProp"=eegProp,
                                   "etVal"=etVal, "etProp"=etProp,
                                   "p300Val"=p300Val, "p300Prop"=p300Prop)))

    } else {
      stop("Error-00 in function bciRetrieve: Missing EEG signal offsets or gains in parameter definition of BCI file - no data output")
    }

  } else {
    stop(paste("Error-00 in function bciRetrieve: ", err, sep=""))
  }

}

#################################################################################################
# Extend or truncate a character string to a fixed length
# Non-character parameter values are converted to character by nchar() and paste()
# Limit parameter values to single element vectors
#################################################################################################

fixLength <- function(a, len) {

  if(length(a)!=1) {
    a <- ""
  } else if(is.null(a)) {
    a <- ""
  } else if(is.na(a)) {
    a <- ""
  }

  if(nchar(a)<len) {
    paste(a, paste(rep(" ", len-nchar(a)), collapse="", sep=""), sep="")
  } else {
    substring(a, 1, len)
  }

}

#################################################################################################
# Extract state vector values from specified BCI2000 signal data, beginning at a given byte and
# bit position, for a given number of bits
#
# Parameters:
# svDat ........... A raw matrix with row i containing the state vector bytes of sample i of BCI2000 data
# bytePos ......... 0-based byte position from which to begin extraction
# bitPos .......... 0-based bit position from which to begin extraction within bytePos
# bitLen .......... Number of bits to extract
# convertInt ...... (T/F) Convert extracted bits to integer
# twosComp ........ (T/F) Interpret bits as two's complement number when converting results to int
# unsignedShift ... (T/F) Treat extracted bits as unsigned integers (when converting to int) then
#                   subtract 2**(bitLen-1), so that values are in the range [-2**(bitLen-1), 2**(bitLen-1)-1]
#                   This places values in the signed 2**bitlen range
#
# Result is:
# convertInt==T ... A vector of integer values corresponding to the specified bit range, two's complement adjusted
# convertInt==F ... bitLen>1 .... A matrix of raw values, row i corresponding to sample i, column j to bit position
#                                 j-1 (low order bits in first cols)
#                   bitLen==1 ... A vector of ra values, position i corresponding to sample i
#################################################################################################

bciStateVectorExtract <- function(svDat, bytePos, bitPos, bitLen, convertInt=F, twosComp=F, unsignedShift=F) {

  # Compute the number of bytes to interrogate
  # Add 1 if number of bits not a multiple of 8, to retain byte containing final bits requested
  nbytes <- as.integer((bitPos+bitLen)/8)+ifelse((bitPos+bitLen)%%8>0, 1, 0)

  # Construct bits necessary to fill to 32 positions, as required by packBits()
  # Compute two's complement threshold and offset
  if(convertInt) {
    padbits <- raw(length=32-bitLen)
    exp2bitLen <- 2**bitLen
    exp2bitLenDiv2 <- 2**(bitLen-1)
  }

  # Iterate through all samples (one per row), extract specified bits, and convert to int, if requested
  # Note that, from https://www.bci2000.org/mediawiki/index.php/Technical_Reference:State_Definition
  # A state vector is a narrowly packed bit field in little endian ordering. This implies that, for a
  # state containing more than a single bit, more significant bits are placed at higher bit and byte
  # locations. As an example, consider a state vector consisting of a 1-bit state "Running", and a
  # 16-bit state "SourceTime". This will result in a three-byte state vector layout like this:
  # ---------------------------------------------------------------------
  # | State Vector Byte 0  | State Vector Byte 1  | State Vector Byte 2 |
  # ---------------------------------------------------------------------
  # |   0    0 1 2 3 4 5 6 | 7 8 9 10 11 12 13 14 | 15 . . . . . . .    |
  # ---------------------------------------------------------------------
  # |Running|  SourceTime                              |                |
  # ---------------------------------------------------------------------
  # The above explanation omits the important fact that bits are contiguous conceptually, not physically
  # For example, the 16 bits of SourceTime are composed, in sequence, from bits 1-7 of byte 0,
  # bits 0-7 of byte 1, and bit 0 of byte 2, where bit 0 indicates the low order bit and bit 7 the high order bit
  # Bits composing SourceTime are, then:
  # ---------------------------------------------------------------------
  # | State Vector Byte 0  | State Vector Byte 1  | State Vector Byte 2 |
  # ---------------------------------------------------------------------
  # |     1 2 3 4 5 6      |    0 1 2 3 4 5 6 7   |          0          |
  # ---------------------------------------------------------------------
  # Important features of functions employed are:
  # rawToBits() returns a vector of 0/1 values in least to most significant order (least bit in initial vector pos)
  # packBits("integer") requires 32 bits, in order of significance (little-bitian)
  w <- apply(svDat, 1,
  function(sv) {
    # Convert bytes to bits
    # Note that bytePos is 0-based (as specified in BCI2000 lit) while R vectors are 1-based
    # Note that rawToBits() returns bits in least to most significant order
    # Extract bits beginning at specified pos (0-based) for specified length
    x <- rawToBits(sv[(bytePos+1):(bytePos+nbytes)])[(bitPos+1):(bitPos+bitLen)]
    if(convertInt) {
      # Append pad bits to high order positions
      y <- c(x, padbits)
      # Integer conversion assumes little-endian byte order (they are) and
      # least to most significant bit order (they are)
      # Examples:
      # packBits(rawToBits(as.raw(c(1, 0, 0, 0))), "integer") = 1
      # packBits(rawToBits(as.raw(c(1, 1, 0, 0))), "integer") = 257
      # packBits(rawToBits(as.raw(c(1, 2, 0, 0))), "integer") = 513
      # packBits(rawToBits(as.raw(c(1, 4, 0, 0))), "integer") = 1025
      # packBits(rawToBits(as.raw(c(1, 8, 0, 0))), "integer") = 2049
      z <- packBits(y, "integer")
      if(twosComp & z>=exp2bitLenDiv2) {
        return(z-exp2bitLen)
      } else if(unsignedShift) {
        return(z-exp2bitLenDiv2)
      } else {
        return(z)
      }
    } else {
      # Note that bits are in least to most significant order, vector position 1 corresponding to 2**0 bit,
      # vector position 2 to 2**1 bit, etc.
      return(x)
    }
  })
  # Transpose result matrix when not converting to integer and bitLen>1
  if(!convertInt & bitLen>1) {
    return(t(w))
  } else {
    return(w)
  }

}

#################################################################################################
# Compose European Data Format (EDF) version of BCI2000 file data
#
# Parameters:
# bcidat ........ list of BCI2000 data elements as generated by the bciRetrieve() function
# patientID ..... patient ID
# recordingID ... recording ID
#
# Value: A raw vector of bytes containing a properly formatted EDF+ file representaion of the
#        input BCI2000 data
#
# From edfplus.info:
# ---------------------------------------------------------------------------------------------
# Below is the detailed digital format of the header record (upper block, ascii's only) and of
# each subsequent data record (lower block, integers only). Note that each one of the ns signals
# is characterized separately in the header.
#
# HEADER RECORD (we suggest to also adopt the 12 simple additional EDF+ specs)
# 8 ascii : version of this data format (0)
# 80 ascii : local patient identification (mind item 3 of the additional EDF+ specs)
# 80 ascii : local recording identification (mind item 4 of the additional EDF+ specs)
# 8 ascii : startdate of recording (dd.mm.yy) (mind item 2 of the additional EDF+ specs)
# 8 ascii : starttime of recording (hh.mm.ss)
# 8 ascii : number of bytes in header record
# 44 ascii : reserved
# 8 ascii : number of data records (-1 if unknown, obey item 10 of the additional EDF+ specs)
# 8 ascii : duration of a data record, in seconds
# 4 ascii : number of signals (ns) in data record
# ns * 16 ascii : ns * label (e.g. EEG Fpz-Cz or Body temp) (mind item 9 of the additional EDF+ specs)
# ns * 80 ascii : ns * transducer type (e.g. AgAgCl electrode)
# ns * 8 ascii : ns * physical dimension (e.g. uV or degreeC)
# ns * 8 ascii : ns * physical minimum (e.g. -500 or 34)
# ns * 8 ascii : ns * physical maximum (e.g. 500 or 40)
# ns * 8 ascii : ns * digital minimum (e.g. -2048)
# ns * 8 ascii : ns * digital maximum (e.g. 2047)
# ns * 80 ascii : ns * prefiltering (e.g. HP:0.1Hz LP:75Hz)
# ns * 8 ascii : ns * nr of samples in each data record
# ns * 32 ascii : ns * reserved
#
# DATA RECORD
# nr of samples[1] * integer : first signal in the data record
# nr of samples[2] * integer : second signal
# ..
# ..
# nr of samples[ns] * integer : last signal
# ---------------------------------------------------------------------------------------------
#################################################################################################

bciToEDF <- function(bcidat, patientID, recordingID) {

  # Compose vector of signal labels
  eegLab <- c("F3", "Fz", "F4", "T7", "C3", "Cz", "C4", "T8", "CP3", "CP4", "P3", "Pz",
              "P4", "PO7", "PO8", "Oz", "FP1", "FP2", "F7", "F8", "FC5", "FC1", "FC2",
              "FC6", "CPz", "P7", "P5", "PO3", "POz", "PO4", "O1", "O2")

  err <- vector("character")

  # Identify session date parameter
  # Currently set to a constant (below)
  #k <- grep("StorageTime", names(bcidat[["parameterDef"]]))
  #if(length(k)>0) {
  #  datePar <- names(bcidat[["parameterDef"]])[k[1]]
  #  dateFmt <- c("%Y-%m-%dT%H:%M:%S", "%a %b %d %H:%M:%S %Y")[2]
  #} else {
  #  err <- c(err, "Missing session date parameter in input BCI data")
  #}

  # Identify sampling rate parameter
  k <- grep("SamplingRate", names(bcidat[["parameterDef"]]))
  if(length(k)>0) {
    sampRatePar <- names(bcidat[["parameterDef"]])[k[1]]
  } else {
    err <- c(err, "Missing sampling rate parameter in input BCI data")
  }

  # Verify existence of signal data
  if(length(err)==0)
    if("signalDat" %in% names(bcidat)) {
      # Verify existence of EEG matrix and property data frame
      if(all(c("eegVal", "eegProp") %in% names(bcidat[["signalDat"]]))) {
        ncoleeg <- ncol(bcidat[["signalDat"]][["eegVal"]])
        # Verify existence of eye tracker matrix and property data frame
        if(all(c("etVal", "etProp") %in% names(bcidat[["signalDat"]]))) {
          # Identify empty eye tracker signal matrix, which is a valid condition,
          # in which case the EDF file will not contain eye tracker signal data
          if(class(bcidat[["signalDat"]][["etVal"]])[1]=="matrix") {
            etNA <- F
            ncolet <- ncol(bcidat[["signalDat"]][["etVal"]])
          } else {
            etNA <- T
            ncolet <- 0
          }
          # Verify existence of speller target matrix and property data frame
          if(all(c("p300Val", "p300Prop") %in% names(bcidat[["signalDat"]]))) {
            ncolp300 <- ncol(bcidat[["signalDat"]][["p300Val"]])
          } else {
            err <- c(err, "Missing p300Val or p300Prop element in signalDat of input BCI data")
          }
        } else {
          err <- c(err, "Missing etVal or etProp element in signalDat of input BCI data")
        }
      } else {
        err <- c(err, "Missing eegVal or eegProp element in signalDat of input BCI data")
      }
    } else {
      err <- c(err, "Missing signalDat element in input BCI data")
    }

  ###########################################################
  # Compose a matrix of signal values
  ###########################################################

  if(length(err)==0)
    # Require number of signals < EDF limit of 9999
    if(ncoleeg>0 & (ncolet>0 | etNA) & ncolp300>0 & ncoleeg+ncolet+ncolp300<9999) {
      # Require identical sample sizes in EEG, eye tracker, and p300 matrices
      if(ifelse(!etNA, nrow(bcidat[["signalDat"]][["eegVal"]])==nrow(bcidat[["signalDat"]][["etVal"]]), T) &
         nrow(bcidat[["signalDat"]][["eegVal"]])==nrow(bcidat[["signalDat"]][["p300Val"]])) {
        # Compose matrix of EEG, eye tracker, and P300 values
        if(!etNA) {
          sigVal <- cbind(bcidat[["signalDat"]][["eegVal"]],
                          bcidat[["signalDat"]][["etVal"]],
                          bcidat[["signalDat"]][["p300Val"]])
        } else {
          sigVal <- cbind(bcidat[["signalDat"]][["eegVal"]],
                          bcidat[["signalDat"]][["p300Val"]])
        }
        # Convert NA signal values to 0, since EDF does not have a representaion for NA
        # Issue a warning if any NAs detected
        # Note the explicit conversion to a matrix in case sigVal contains a single column
        # Result is a numeric matrix of dimension nSamples X nSignals
        nNA <- 0
        sigVal <- matrix(apply(sigVal, 2,
                               function(x) {
                                 k <- which(is.na(x))
                                 x[k] <- 0
                                 nNA <<- nNA+length(k)
                                 x
                               }), ncol=ncol(sigVal))
        # Assign signal labels
        # EEG signal labels are composed from rules involving number of signals and transmit channel cfg
        # Eye tracker labels are taken from the etVal matrix
        # P300 labels are taken from the p300Val matrix
        if(ncoleeg==1) {
          eLab <- "EEG"
        } else if(ncoleeg==2) {
          eLab <- c("EEG_L", "EEG_R")
        } else {
          # Note that, if transmit channel list is missing, tcl is NULL, with length 0, and isnum() is F
          tcl <- bcidat[["parameterDef"]][["Source:OnlineProcessing:TransmissionFilter:TransmitChList"]][["value"]]
          if(all(isnum(tcl))) {
            if(ncoleeg %in% c(8, 16, 32)) {
              eLab <- paste("EEG_", eegLab[1:ncoleeg], sep="")
            } else {
              eLab <- paste("EEG_", 1:ncoleeg, sep="")
            }
          } else if(ncoleeg==length(tcl)) {
            eLab <- tcl
          } else {
            eLab <- paste("EEG_", 1:ncoleeg, sep="")
          }
        }
        if(!etNA) {
          colnames(sigVal) <- c(eLab,
                                colnames(bcidat[["signalDat"]][["etVal"]]),
                                colnames(bcidat[["signalDat"]][["p300Val"]]))
        } else {
          colnames(sigVal) <- c(eLab, colnames(bcidat[["signalDat"]][["p300Val"]]))
        }
        if(nNA>0)
          warning(paste("Error-20 in function bciToEDF: ", nNA, " NAs detected in eegVal, etVal, or P300Val matrix", sep=""))
        # Require integer signal values bounded by two-byte, signed integer range
        if(all(apply(sigVal, 2, function(x) all(as.integer(x)==x & x>=-32768 & x<=32767)))) {
          # Verify one set of properties for each signal
          # Columns of property data frames contain properties
          # Rows of property data frame correspond to columns, or variables, of component signal matrix
          # Note that missing named list elements have length of 0
          if(nrow(bcidat[["signalDat"]][["eegProp"]])==ncol(bcidat[["signalDat"]][["eegVal"]]) &
             ifelse(!etNA, nrow(bcidat[["signalDat"]][["etProp"]])==ncol(bcidat[["signalDat"]][["etVal"]]), T) &
             nrow(bcidat[["signalDat"]][["p300Prop"]])==ncol(bcidat[["signalDat"]][["p300Val"]])) {
            # Require and retrieve all signal properties
            propID <- c("pMin", "pMax", "dMin", "dMax", "tdc", "dim")
            if(all(propID %in% colnames(bcidat[["signalDat"]][["eegProp"]])) &
               (all(propID %in% colnames(bcidat[["signalDat"]][["etProp"]])) | etNA) &
               all(propID %in% colnames(bcidat[["signalDat"]][["p300Prop"]]))) {
              if(!etNA) {
                datProp <- rbind(bcidat[["signalDat"]][["eegProp"]][,propID],
                                 bcidat[["signalDat"]][["etProp"]][,propID],
                                 bcidat[["signalDat"]][["p300Prop"]][,propID])
              } else {
                datProp <- rbind(bcidat[["signalDat"]][["eegProp"]][,propID],
                                 bcidat[["signalDat"]][["p300Prop"]][,propID])
              }
              # Convert numeric properties with NA values to 0
              nNA <- 0
              datProp <- data.frame(lapply(datProp[,propID], function(x) {
                k <- which(is.na(x))
                if(class(x)=="numeric") {
                  x[k] <- 0
                } else {
                  x[k] <- ""
                }
                nNA <<- nNA+length(k)
                x
              }))
              colnames(datProp) <- propID
              if(nNA>0)
                warning(paste("Error-20 in function bciToEDF: ", nNA, " NAs detected in eegProp, etProp, or p300Prop matrix", sep=""))
              # Require digital min/max to be integer and in two-byte, signed range
              if(!all(apply(datProp[,c("dMin", "dMax")], 2, function(x) all(as.integer(x)==x & x>=-32768 & x<=32767)))) {
                err <- c(err, "Non-integer values or values outside of two-byte, signed integer range detected in eegProp/etProp/p300Prop dMin/dMax")
              }
            } else {
              err <- c(err, paste("Missing one or more of ", paste(propID, collapse=", ", sep=""), " col(s) in eegProp/etProp/p300Prop matrix", sep=""))
            }
          } else {
            err <- c(err, "Number of eegProp/etProp/p300Prop signals (rows) different from eegVal/etVal/p300Val signals (columns)")
          }
        } else {
          err <- c(err, "Non-integer values or values outside of two-byte, signed integer range detected in sigVal/etVal/p300Val matrix")
        }
      } else {
        err <- c(err, "Number of signal, state vector, p300 (eegVal/etVal/p300Val) samples (rows) differ")
      }
    } else {
      err <- c(err, "Signal, state vector, p300 value matrix (eegVal/etVal/p300Val) empty or number of combined columns exceeds EDF limit of 9,999")
    }

  if(length(err)==0) {

    # Number of signals and samples
    nsig <- ncol(sigVal)
    nsamp <- nrow(sigVal)

    # Adjust physical/digital max when <= physical/digital min
    # EDF requires pMax>pMin and dMax>dMin
    k <- which(datProp[,"pMin"]>=datProp[,"pMax"])
    datProp[k,"pMax"] <- datProp[k,"pMax"]+1e-5
    k <- which(datProp[,"dMin"]>=datProp[,"dMax"])
    datProp[k,"dMin"] <- -32768
    datProp[k,"dMax"] <- 32767

    # Convert signals to raw bytes in little-endian format (least significant byte first)
    # It is assumed that all signal values are integers on the interval [-32768, 32767]
    # The 16 low order bits are retained
    # Result is a vector of raw bytes
    # Positions 1..2*nSamples of result correspond to signal 1, positions 2*nSamples+1..4*nSamples to signal 2, etc.
    sigEDF <- unlist(lapply(1:nsig, function(j) apply(as.matrix(sigVal[,j]), 1, function(x) packBits(intToBits(x)[0:16]))))

    # Construct EDF header fields
    # Note the adjustment to lengths as required by EDF spec
    ver <- fixLength(0, 8)
    # Patient ID and recording ID
    patientID <- fixLength(patientID, 80)
    recordingID <- fixLength(recordingID, 80)
    # Date
    # BCI contents currently ignored and replaced with a static value
    # Convert %20 to single space
    #x <- strptime(gsub("%20", " ", bcidat[["parameterDef"]][[datePar]][["value"]][1], fixed=T), format=dateFmt)
    #if(!is.na(x)) {
    #  # Note that POSIXlt years are offset from 1900, months are 0-based
    #  # Prefix single digit values with 0
    #  recordingDate <- paste(ifelse(x$mday<10, "0", ""), x$mday, ".",
    #                         ifelse(x$mo<9, "0", ""), x$mo+1, ".",
    #                         ifelse(x$year%%100<10, "0", ""), x$year%%100, sep="")
    #  recordingTime <- paste(ifelse(x$hour<10, "0", ""), x$hour, ".",
    #                         ifelse(x$min<10, "0", ""), x$min, ".",
    #                         ifelse(x$sec<10, "0", ""), x$sec, sep="")
    #} else {
    #  recordingDate <- fixLength("", 8)
    #  recordingTime <- fixLength("", 8)
    #}
    recordingDate <- "01.01.20"
    recordingTime <- "00.00.00"
    # Number of bytes in header will be updated after header construction is complete
    nbHeader0 <- fixLength("", 8)
    rsv1 <- fixLength("EDF+C", 44)
    #rsv1 <- fixLength("", 44)
    # EDF file will consist of one data record
    nDataRec <- fixLength(ifelse(nsamp>0, 1, -1), 8)
    # Sampling rate is in hertz, duration required in seconds
    # Divide number of data recs by sampling rate for duration
    x <- suppressWarnings(as.numeric(bcidat[["parameterDef"]][[sampRatePar]][["value"]][1]))
    durDataRec <- fixLength(ifelse(length(x)>0 & !is.na(x), ifelse(x>0, nsamp/x, "0"), "0"), 8)
    # Signal labels
    sigLab <- colnames(sigVal)
    # Prefilter string
    # Format is "HP:xHz LP:yHz" where x = filter high pass, y = filter low pass
    prefiltF <- vector("character", 0)
    k <- grep("FilterEnabled", names(bcidat[["parameterDef"]]), fixed=T)
    if(length(k)>0)
      if(bcidat[["parameterDef"]][[k[1]]][["value"]][1]=="1") {
        # Include filter high and low pass values, if present
        k <- grep("FilterHighPass", names(bcidat[["parameterDef"]]), fixed=T)
        if(length(k)>0)
          prefiltF <- c(prefiltF, paste("HP:", bcidat[["parameterDef"]][[k[1]]][["value"]][1], "Hz", sep=""))
        k <- grep("FilterLowPass", names(bcidat[["parameterDef"]]), fixed=T)
        if(length(k)>0)
          prefiltF <- c(prefiltF, paste("LP:", bcidat[["parameterDef"]][[k[1]]][["value"]][1], "Hz", sep=""))
      }
    # Notch string (x = notch high pass, y = notch low pass
    # Format is:
    #   "60Hz" when notch enabled, notch high pass = 58, and notch low pass = 62
    #   "50Hz" when notch enabled, notch high pass = 48, and notch low pass = 52
    #   Otherwise, "NHP:xHz NLP:yHz" where x = notch high pass, y = notch low pass
    prefiltN <- vector("character", 0)
    k1 <- grep("NotchEnabled", names(bcidat[["parameterDef"]]), fixed=T)
    k2 <- grep("NotchHighPass", names(bcidat[["parameterDef"]]), fixed=T)
    k3 <- grep("NotchLowPass", names(bcidat[["parameterDef"]]), fixed=T)
    # Attempt 50 or 60 Hz evaluation
    if(length(k1)>0)
      if(bcidat[["parameterDef"]][[k1[1]]][["value"]][1]=="1" & length(k2)>0 & length(k3)>0)
        if(bcidat[["parameterDef"]][[k2[1]]][["value"]][1]=="58" & bcidat[["parameterDef"]][[k3[1]]][["value"]][1]=="62") {
          prefiltN <- "N:60Hz"
        } else if(bcidat[["parameterDef"]][[k2[1]]][["value"]][1]=="48" & bcidat[["parameterDef"]][[k3[1]]][["value"]][1]=="52") {
          prefiltN <- "N:50Hz"
        }
    # Compose NHP:vHz NLP:wHz string when 50 or 60 Hz not generated
    if(length(prefiltN)==0) {
      if(length(k2)>0)
        prefiltN <- paste("NHP:", bcidat[["parameterDef"]][[k2[1]]][["value"]][1], "Hz", sep="")
      if(length(k3)>0)
        prefiltN <- c(prefiltN, paste("NLP:", bcidat[["parameterDef"]][[k3[1]]][["value"]][1], "Hz", sep=""))
    }
    # Composite filter and notch string
    prefilt <- paste(c(prefiltF, prefiltN), collapse=" ", sep="")
    if(nsig>1)
      prefilt <- c(rep(prefilt, ncoleeg), rep("", ncolet+ncolp300))
    fill1 <- rep("", nsig)

    # Include annotations signal (required for EDF+ file)
    # See https://www.edfplus.info/specs/edfplus.html for specification
    sigAnnot <- c(charToRaw("+0"), as.raw(20), as.raw(20), charToRaw("Begin recording"), as.raw(20), as.raw(0))
    # Force even number of bytes (each sample is composed from two bytes)
    if(length(sigAnnot)%%2==1)
      sigAnnot <- c(sigAnnot, as.raw(0))
    pMin2 <- c(datProp[,"pMin"], 0)
    pMax2 <- c(datProp[,"pMax"], 1)
    dMin2 <- c(datProp[,"dMin"], -32768)
    dMax2 <- c(datProp[,"dMax"], 32767)
    sigEDF2 <- c(sigEDF, sigAnnot)
    nsig2 <- nsig+1
    # Number of samples for annotation record is computed as number of two-byte integers (as with signals)
    nsamp2 <- c(rep(nsamp, nsig), length(sigAnnot)/2)
    sigLab2 <- c(sigLab, "EDF Annotations")
    tdType2 <- c(datProp[,"tdc"], "")
    pDim2 <- c(datProp[,"dim"], "")
    prefilt2 <- c(prefilt, "")
    fill2 <- c(fill1, "")

    # Concatenate vector elements into contiguous strings
    nsig3 <- fixLength(nsig2, 4)
    sigLab3 <- paste(unlist(lapply(sigLab2, function(a) fixLength(a, 16))), collapse="", sep="")
    tdType3 <- paste(unlist(lapply(tdType2, function(a) fixLength(a, 80))), collapse="", sep="")
    pDim3 <- paste(unlist(lapply(pDim2, function(a) fixLength(a, 8))), collapse="", sep="")
    pMin3 <- paste(unlist(lapply(pMin2, function(x) fixLength(x, 8))), collapse="", sep="")
    pMax3 <- paste(unlist(lapply(pMax2, function(x) fixLength(x, 8))), collapse="", sep="")
    dMin3 <- paste(unlist(lapply(dMin2, function(x) fixLength(x, 8))), collapse="", sep="")
    dMax3 <- paste(unlist(lapply(dMax2, function(x) fixLength(x, 8))), collapse="", sep="")
    prefilt3 <- paste(unlist(lapply(prefilt2, function(x) fixLength(x, 80))), collapse="", sep="")
    nsamp3 <- paste(unlist(lapply(nsamp2, function(x) fixLength(x, 8))), collapse="", sep="")
    fill3 <- paste(unlist(lapply(fill2, function(x) fixLength(x, 32))), collapse="", sep="")

    # Accumulate number of bytes in header
    nbHeader <- fixLength(
                  nchar(ver)+nchar(patientID)+nchar(recordingID)+nchar(recordingDate)+nchar(recordingTime) +
                  nchar(nbHeader0)+nchar(rsv1)+nchar(nDataRec)+nchar(durDataRec)+nchar(nsig3) +
                  nchar(sigLab3)+nchar(tdType3)+nchar(pDim3)+nchar(pMin3)+nchar(pMax3)+nchar(dMin3)+nchar(dMax3) +
                  nchar(prefilt3)+nchar(nsamp3)+nchar(fill3), 8)

    # Compose header
    # Verify computed length of header
    # According to www.edfplus.info/specs/edf.html,
    # The first 256 bytes of the header record specify the version number of this format, local patient
    # and recording identification, time information about the recording, the number of data records and
    # finally the number of signals (ns) in each data record. Then for each signal another 256 bytes follow
    # in the header record, each specifying the type of signal (e.g. EEG, body temperature, etc.),
    # amplitude calibration and the number of samples in each data record (from which the sampling frequency
    # can be derived since the duration of a data record is also known). In this way, the format allows for
    # different gains and sampling frequencies for each signal. The header record contains 256 + (ns * 256) bytes.
    hdr <- paste(ver, patientID, recordingID, recordingDate, recordingTime, nbHeader, rsv1,
                 nDataRec, durDataRec, nsig3, sigLab3, tdType3, pDim3, pMin3, pMax3, dMin3, dMax3,
                 prefilt3, nsamp3, fill3, collapse="", sep="")

    # Report required and assembled byte lengths
    print(paste("EDF signal data bytes (reported, actual):  ", 2*nsig*nsamp+length(sigAnnot), ", ", length(sigEDF2), sep=""))
    print(paste("EDF header bytes (required, reported, actual):  ", nsig2*256+256, ", ", as.numeric(nbHeader), ", ", nchar(hdr), sep=""))

    # Compose string of bytes in EDF file format

    # According to www.edfplus.info/specs/edf.html,
    # Following the header record, each of the subsequent data records contains 'duration' seconds of 'ns' signals,
    # with each signal being represented by the specified (in the header) number of samples. In order to reduce
    # data size and adapt to commonly used software for acquisition, processing and graphical display of polygraphic
    # signals, each sample value is represented as a 2-byte integer in 2's complement format.

    # From https://www.edfplus.info/specs/edffloat.html
    # In many cases, the floating point data can be scaled to fit into the available 2 byte integer range of -32768 to 32767.
    # The scaling factor can be adapted to the dynamic range of the analysis result even after the analysis was done. Put these
    # scaling factors in the header (digital and physical minimum and maximum) of the analysis file.

    # Test resulting file contents with the EDF browser, available at www.edfplus.info/downloads/index.html

    return(c(charToRaw(hdr), sigEDF2))

  } else {

    stop(paste("Error-00 in function bciToEDF: ", err, "\n", sep=""))

  }

}
