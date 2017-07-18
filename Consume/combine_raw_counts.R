#
# This simple script reads the fixed width counts data files from CalTrans and
# write a combined csv file containing all years.
#

# Initialization: Set the workspace and load needed libraries
.libPaths(Sys.getenv("R_LIB"))
library(dplyr)
library(readr)

#### Input and output locations
F_INPUT_DIR  <- "M:/Data/Traffic/Caltrans/Raw Data"
F_OUTPUT_DIR <- "M:/Data/Traffic/Caltrans/Processed Data"
START_YEAR   <- 1998
END_YEAR     <- as.integer(format(Sys.Date(), "%Y"))
last_year    <- START_YEAR # last year written

F_INPUT_DIR   <- gsub("\\\\","/",F_INPUT_DIR ) # switch slashes around
F_OUTPUT_DIR  <- gsub("\\\\","/",F_OUTPUT_DIR) # switch slashes around

# pre-1998 data is a different format
cols_post97 <- rbind(
  c("ROUTE",      3, "c"),
  c("ROUTESEQ",   8, "c"),
  c("DISTRICTid", 2, "i"),
  c("COUNTY",     3, "c"),
  c("POSTMILP",   1, "c"),
  c("PM",         6, "c"),
  c("POSTMILS",   1, "c"),
  c("LEG",        1, "c"),
  c("DIR",        1, "c"),
  c("ROADTYPE",   1, "c"),
  c("LOCATION",   1, "c"),
  c("STATION",    7, "c"),
  c("DESCRIP",   30, "c"),
  c("YEAR",       2, "i"),
  c("MONTH",      2, "i"),
  c("DATE1",      2, "i"),
  c("WEEKDAY",    3, "c"),
  c("END1AM",     6, "i"),  c("END1AMC",    1, "c"),
  c("END2AM",     6, "i"),  c("END2AMC",    1, "c"),
  c("END3AM",     6, "i"),  c("END3AMC",    1, "c"),
  c("END4AM",     6, "i"),  c("END4AMC",    1, "c"),
  c("END5AM",     6, "i"),  c("END5AMC",    1, "c"),
  c("END6AM",     6, "i"),  c("END6AMC",    1, "c"),
  c("END7AM",     6, "i"),  c("END7AMC",    1, "c"),
  c("END8AM",     6, "i"),  c("END8AMC",    1, "c"),
  c("END9AM",     6, "i"),  c("END9AMC",    1, "c"),
  c("END10AM",    6, "i"),  c("END10AMC",   1, "c"),
  c("END11AM",    6, "i"),  c("END11AMC",   1, "c"),
  c("END12NOO",   6, "i"),  c("END12NOC",   1, "c"),
  c("END1PM",     6, "i"),  c("END1PMC",    1, "c"),
  c("END2PM",     6, "i"),  c("END2PMC",    1, "c"),
  c("END3PM",     6, "i"),  c("END3PMC",    1, "c"),
  c("END4PM",     6, "i"),  c("END4PMC",    1, "c"),
  c("END5PM",     6, "i"),  c("END5PMC",    1, "c"),
  c("END6PM",     6, "i"),  c("END6PMC",    1, "c"),
  c("END7PM",     6, "i"),  c("END7PMC",    1, "c"),
  c("END8PM",     6, "i"),  c("END8PMC",    1, "c"),
  c("END9PM",     6, "i"),  c("END9PMC",    1, "c"),
  c("END10PM",    6, "i"),  c("END10PMC",   1, "c"),
  c("END11PM",    6, "i"),  c("END11PMC",   1, "c"),
  c("END12NIG",   6, "i"),  c("END12NIC",   1, "c"),
  c("TOTAL24",    7, "i"),  c("TOTAL24C",   1, "c"),
  c("DAYTOT",     7, "i"),  c("DAYTOTC",    1, "c")
)

allyears_df <- data.frame()

# output file will be counts_YYYY_YYYY.csv - where the first YYYY is the start year and the second is the final year
for (year in START_YEAR:END_YEAR) {
  fwf_filename <- file.path(F_INPUT_DIR, paste0("count",year,".dat"))
  
  if (file.exists(fwf_filename)) {
    print(paste("Reading",fwf_filename))
    tmpfile <- fwf_filename
    cols    <- cols_post97
    
    # leaving this in but decided to scrap processing 1996 and 1997 since they are a different format
    # but the nul-removal is for reference
    if (year <= 1997) {
      # read as raw, replace NULLs with spaces and write to temp file
      fwf_bin <- readBin(fwf_filename, raw(), file.info(fwf_filename)$size)
      fwf_bin[fwf_bin==as.raw(0)] = as.raw(0x20) ## replace null with space
      fwf_data <- rawToChar(fwf_bin)
      tmpfile  <- tempfile(pattern = "combine_raw_counts", tmpdir = tempdir(), fileext = ".txt")
      print(paste("Writing",tmpfile))
      write(fwf_data, tmpfile)
    }

    # read the temp file
    fwf_df     <- read_fwf(tmpfile,
                           col_positions=fwf_widths(as.integer(cols[,2]), col_names=cols[,1]),
                           col_types    =paste(cols[,3], collapse=''))
    print(head(fwf_df))
    
    allyears_df <- rbind(allyears_df, fwf_df)
    last_year   <- year
    print(paste("allyears_df has length", nrow(allyears_df)))
  }
  
  # if (year==2001) { break }
}

# create date, ident
allyears_df <- mutate(allyears_df,
                      YEAR=ifelse(YEAR>90, YEAR+1900, YEAR+2000),
                      date=sprintf("%4d-%02d-%02d", YEAR,MONTH,DATE1),
                      ident=paste0(ROUTE,ROUTESEQ,DISTRICTid,COUNTY,POSTMILP,PM,POSTMILS,LEG,DIR))

# convert some character fields to numbers and trim whitespace
allyears_df <- mutate(allyears_df,
                      POSTMILP =trimws(POSTMILP),
                      PM       =trimws(PM),
                      POSTMILS =trimws(POSTMILS),
                      COUNTY   =trimws(COUNTY),
                      ROADTYPE =trimws(ROADTYPE))

print(head(allyears_df))

# write combined file
OUTFILE <- paste0("counts_",START_YEAR,"-",last_year,".csv")
write.table(allyears_df, file.path(F_OUTPUT_DIR,OUTFILE), sep=",", row.names=FALSE, quote=TRUE)
