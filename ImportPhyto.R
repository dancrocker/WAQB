##############################################################################################################################
#     Title: ImportPhyto.R
#     Description: This script will Format/Process MWRA data to DCR
#     Written by: Dan Crocker, Max Nyquist
#     Last Update: April 2018
#    This script will process and import Wachusett Phytoplankton data into the AB Database
##############################################################################################################################

# COMMENT OUT BELOW WHEN RUNNING FUNCTION IN SHINY

# # Load libraries needed
  library(tidyverse)
  library(stringr)
  library(odbc)
  library(RODBC)
  library(DBI)
  library(lubridate)
  library(readxl)
  library(magrittr)
  library(DescTools)

# COMMENT OUT ABOVE CODE WHEN RUNNING IN SHINY!
scriptname <- "ImportPhyto.R"
config <- read.csv("//env.govt.state.ma.us/enterprise/DCR-WestBoylston-WKGRP/WatershedJAH/EQStaff/WQDatabase/R-Shared/WAVE-WIT/Configs/WAVE_WIT_Config.csv", header = TRUE)
config <- as.character(config$CONFIG_VALUE)

dataset <-  read_excel(config[9], sheet = 1, col_names = T, trim_ws = T) %>%
  filter(ImportMethod == "Importer-R" & ScriptProcessImport == scriptname)
# Choose the dataset from options (Trib Selected):
dataset <- slice(dataset,1)
rawdatafolder <- paste0(dataset[10])
processedfolder <- paste0(dataset[11])
filename.db <- paste0(dataset[6])
ImportTable <- paste0(dataset[7])
ImportFlagTable <- NULL # This data has no related flag table

# ### Find the file to Import -  if this will always be a csv then your regex need not include "xlsx" both here and in your datasets excel file
files <- grep(
  x = list.files(rawdatafolder, ignore.case = T, include.dirs = F),
  pattern = "^(?=.*\\b(xlsx|csv)\\b)(?!.*\\$\\b)", # regex to show xlsx files, but filter out lockfiles string = "$"
  value = T,
  perl =T
)

#  List the files:
files
# # Select the file to import manually
file <- files[1]

############################################################################################
#DC Testing Code (these lines not needed in final script - delete down to line 58

filename.db <-  "//env.govt.state.ma.us/enterprise/DCR-WestBoylston-WKGRP/WatershedJAH/EQStaff/WQDatabase/DB_Testing/TESTING_WQDB_be.mdb"
rawdatafolder <- "P:/DOCUMENTS/R/QUABBIN"
ImportTable <- "tbl_PhytoQ"
file <- "2018-04-09_Quabbin_206.xlsx"

######################################################################################################################
######################################################################################################################
######################################################################################################################

PROCESS_DATA <- function(file, rawdatafolder, filename.db, probe = NULL, ImportTable, ImportFlagTable = NULL){ # Start the function - takes 1 input (File)

# Eliminate Scientific notation in numerical fields
options(scipen = 999)
# Get the full path to the file
path <- paste0(rawdatafolder,"/", file)
# Assign the sheet number
sheetNum <- as.numeric(length(excel_sheets(path)))
# Read in the raw data - defaults to the last sheet added
df.wq <- read_excel(path, sheet = sheetNum, range = cell_cols("K:W"),  col_names = F, trim_ws = T, na = "nil") %>%
                    as.data.frame()   # This is the raw data - data comes in as xlsx file, so read.csv will not work

dataDate <- as.Date(df.wq[3,2])
dataLoc <- as.character(df.wq[3,7])
analyst <- as.character(df.wq[3,9])
# Remove unwanted columns and rows
df.wq <- df.wq  %>% 
  select(c(1,5:8,10)) %>%
  slice(6:8)

# Rename Columns using first row values and then remove the first row
names(df.wq) <- unlist(df.wq[1,])
df.wq <- df.wq[-1,]

# Gather data records into tidy format, modify column names and add missing columns
df.wq <- gather(df.wq,"Taxa","Density", 2:ncol(df.wq), na.rm =T) %>% 
  dplyr::rename("Depth_m" = "(meters)") %>% 
  mutate(SampleDate = dataDate, 
         Station = dataLoc, 
         Analyst = analyst, 
         ImportDate = today(), 
         DataSource = file) 

# Fix data types and digits
df.wq$Density <- round(as.numeric(df.wq$Density))
df.wq$Depth_m <- as.numeric(df.wq$Depth_m)

# Connect to db 
con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                                            paste0("DBQ=", filename.db), "Uid=Admin;Pwd=;", sep = ";"),
                 timezone = "America/New_York")
# Get Taxa Table and check to make sure taxa in df.wq are in the Taxa Table - if not warn and exit
df_taxa_wach <- dbReadTable(con,"tbl_Taxa")
unmatchedTaxa <- which(is.na(df_taxa_wach$ID[match(df.wq$Taxa, df_taxa_wach$Name)]))
if (length(unmatchedTaxa) > 0){
  # Exit function and send a warning to user
  stop(paste("This data file contains", length(unmatchedTaxa),
             "records with taxa that are not present in the Taxa Table -
             Please add new taxa to the Taxa Table or fix names prior to importing new records.",
             "The taxa not in the Taxa Table are: ", paste(unique(df.wq[unmatchedTaxa, "Taxa"]), collapse = ", ")), call. = FALSE)
}
# Unique ID number
df.wq$UniqueID <- paste(df.wq$Station, df.wq$SampleDate, df.wq$Depth_m, df_taxa_wach$ID[match(df.wq$Taxa, df_taxa_wach$Name)], sep = "_")

## Make sure it is unique within the data file - if not then exit function and send warning
dupecheck <- which(duplicated(df.wq$UniqueID))
dupes <- df.wq$UniqueID[dupecheck] # These are the dupes

if (length(dupes) > 0){
  # Exit function and send a warning to userlength(dupes) # number of dupes
  stop(paste("This data file contains", length(dupes),
             "records that appear to be duplicates. Eliminate all duplicates before proceeding.",
             "The duplicate records include:", paste(head(dupes, 15), collapse = ", ")), call. = FALSE)
}

### Make sure records are not already in DB
Uniq <- dbGetQuery(con,paste0("SELECT UniqueID, ID FROM ", ImportTable))
dupes2 <- Uniq[Uniq$UniqueID %in% df.wq$UniqueID,]

if (nrow(dupes2) > 0){
  # Exit function and send a warning to user
  stop(paste("This data file contains", nrow(dupes2),
             "records that appear to already exist in the database!
             Eliminate all duplicates before proceeding.",
             "The duplicate records include:", paste(head(dupes2$UniqueID, 15), collapse = ", ")), call. = FALSE)
}
rm(Uniq)

# Do some sorting:
df.wq <- df.wq[with(df.wq, order(SampleDate, Taxa, Depth_m)),]

# Assign the DataSourceID
df.wq$DataSourceID <- seq(1, nrow(df.wq), 1)

### IDs

# Read Tables
# WQ
setIDs <- function(){
  query.wq <- dbGetQuery(con, paste0("SELECT max(ID) FROM ", ImportTable))
  # Get current max ID
  if(is.na(query.wq)) {
    query.wq <- 0
  } else {
    query.wq <- query.wq
  }
  ID.max.wq <- as.numeric(unlist(query.wq))
  rm(query.wq)

  ### ID wq
  df.wq$ID <- seq.int(nrow(df.wq)) + ID.max.wq
}
df.wq$ID <- setIDs()

# Reorder columns to match the database table exactly
cnames <- dbListFields(con, ImportTable)
df.wq <- df.wq[,cnames]
# Create a list of the processed datasets
dfs <- list()
dfs[[1]] <- df.wq
dfs[[2]] <- path
dfs[[3]] <- NULL # Removed condition to test for flags and put it in the setFlagIDS() function

# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)
return(dfs)
} # END FUNCTION

#### COMMENT OUT SECTION BELOW WHEN RUNNING SHINY
########################################################################################################
# #RUN THE FUNCTION TO PROCESS THE DATA AND RETURN 2 DATAFRAMES and path AS LIST:
# dfs <- PROCESS_DATA(file, rawdatafolder, filename.db, ImportTable = ImportTable, ImportFlagTable = NULL )
# #
# # # Extract each element needed
# df.wq     <- dfs[[1]]
# path      <- dfs[[2]]
# df.flags  <- dfs[[3]]

########################################################################################################

##########################
# Write data to Database #
##########################

IMPORT_DATA <- function(df.wq, df.flags = NULL, path, file, filename.db, processedfolder, ImportTable, ImportFlagTable = NULL){
  # df.flags is an optional argument  - not used for this dataset

# Establish db connection
con <-  odbcConnectAccess(filename.db)
# Get Import Table Columns
ColumnsOfTable <- sqlColumns(con, ImportTable)

# Set variable types
varTypes  <- as.character(ColumnsOfTable$TYPE_NAME)
sqlSave(con, df.wq, tablename = ImportTable, append = T,
          rownames = F, colnames = F, addPK = F , fast = F, varTypes = varTypes)

# Disconnect from db and remove connection obj
odbcCloseAll()
rm(con)

  return("Import Successful")
}
### END
# 
# IMPORT_DATA(df.wq, df.flags = NULL, path, file, filename.db, processedfolder = NULL,
#             ImportTable = ImportTable, ImportFlagTable = NULL)




