# This script imports the read number data from the tsv created on amphiprion.
# It assigns Ligation ID to barcode
# It updates the laboratory database

library(stringr)
source("scripts/lab_helpers.R")

# look up the data files - fetched from amphiprion
pools <- list.files(path = "data/", pattern = "process")
names <- list.files(path = "data/", pattern = "names")


# create an empty dataframe
reads <- data.frame()
for (i in 1:length(pools)){
  # determine which pool is active
  pool <- substr(pools[i], 1,2)
  # assign the nth file to filename
  # using grep makes sure the same pool is used regardless of position in the list
  filename1 <- paste("data/", pools[grep(pool, pools)], sep = "") 
  # read in the data file
  data <- read.delim(filename1, header = F) 
  # rename the columns
  colnames(data) <- c("barcode", "total_reads", "no_rad_tag", "low_quality", "retained") 
  # assign the name file to a filename
  filename2 <- paste("data/", names[grep(pool, names)], sep = "")
  name <- read.delim(filename2, header = F) 
  # rename columns in name file
  colnames(name) <- c("ligation_id", "barcode") 
  # add the ligation names to the read data
  read <- dplyr::left_join(data, name, by = "barcode") 
  # remove the barcode column
  read$barcode <- NULL
  # add the finished product to the running tally
  reads <- rbind(reads, read) 
}

# get rid of extra characters in ligation_ids from read data
# create a search string to find ligation ids
ligid <- "(.+)(L\\d\\d\\d\\d)(.+)"
# ligid <- "(L\\d\\d\\d\\d)(.+)" 
# ligid <- "(.+)(L\\d\\d\\d\\d)(.+)" 
# test # 
str_detect(reads$ligation_id, ligid)
# change all of the names to ligation id only
reads$ligation_id <- reads$ligation_id %>% str_replace(ligid,"\\2")
# rename this column so the purpose is clear
reads <- rename(reads, lack_rad_tag = no_rad_tag)

# read in the table from the database
lab <- write_db("Laboratory")
lig <- dbReadTable(lab, "ligation")

# pull out the rows to be changed & remove the columns to be changed
change <- lig %>% 
  filter(ligation_id %in% reads$ligation_id) %>%
  select(-total_reads, -lack_rad_tag, -low_quality, -retained)
# add in new data
change <- left_join(change, reads, by = "ligation_id")

lig <- change_rows(lig, change, "ligation_id")

###########  BE CAREFUL ###################
# dbWriteTable(lab, "ligation", lig, row.names = F, overwrite = T)
# dbDisconnect(lab)
