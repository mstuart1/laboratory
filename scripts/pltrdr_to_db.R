# Read in quantification results ------------------------------------------
library(dplyr)
library(RMySQL)
source("scripts/lab_helpers.R")

# what types of samples are these
table = "extraction"
# table = "digest"
# table = "ligation"

# which plate is it?
range = "E4195-E4288"

# read in plate reader data for the first plate
platefile1 = "/Users/macair/Google Drive/Pinsky Lab/Plate Reader/2017_10/2017-10-09-plate12.txt"
# colsinplate1 = 2:12 # is this a full plate?

# skip the lines before the data
strs <- readLines(platefile1, skipNul = T)
linestoskip = (which(strs == "Group: Unknowns")) # the number of lines to skip

# create a table that contains well locations and quantities
dat1 <- read.table(text = strs,  skip = linestoskip, sep = "\t", fill = T, header = T, stringsAsFactors = F)

# remove footer rows
dat1 <- dat1[1:(which(dat1$Sample == "Group Summaries")-1), ]

# read in names for the samples
lab <- write_db("Laboratory")

# all of the data in the db for this table
dat <- dbReadTable(lab, table)

# select your desired plate
plate <- dat %>% 
  select(contains("id"), well, plate) %>% 
  filter(plate == range) %>% 
  collect()


quant1 <- dplyr::left_join(dat1, plate, by = c("Wells" = "well"))
quant1 <- quant1 %>% 
  ### THIS DEPENDS ON THE TYPE OF SAMPLE YOU ARE LOOKING AT extraction_id, digest_id, etc) ###
  select(contains("id"), AdjConc)

# # remove any empty wells and rename the column
quant1 <- quant1 %>%
#   filter(!is.na(1)) %>% 
  rename(quant = AdjConc)
# 
############### add this information to the database ##################
# BE CAREFUL #

# the entire table was pulled in as dat above

# select the samples that will be changed
change <- dat %>%
  filter(plate == range)

# remove the samples that will be changed from the main group
whole <- anti_join(dat, change, by = "extraction_id")

# incorporate the quantification
change <- change %>% 
  select(-quant)

change <- left_join(change, quant1, by = c("extraction_id", "sample_id"))

# add the changes back to the group
whole <- rbind(whole, change)

# write the group back to the database
# dbWriteTable(lab, table, whole, row.names = F, overwrite = T)
# dbDisconnect(lab)


# import the firsts #### 
# this is for the first column of each plate that was put onto a separate plate to make room for the standards

platefile2 = "/Users/macair/Google Drive/Pinsky Lab/Plate Reader/2017_10/2017-10-09-plate14.txt"

strs <- readLines(platefile2, skipNul = T)
linestoskip = (which(strs == "Group: Unknowns")) # the number of lines to skip

dat2 <- read.table(text = strs,  skip = linestoskip, sep = "\t", fill = T, header = T, stringsAsFactors = F)

# remove footer rows
dat2 <- dat2[1:(which(dat2$Sample == "Group Summaries")-1), ]

# open the list of first column samples
firsts <- read.csv("output/2017-10-09_firsts_list.csv")
firsts <- firsts %>% 
  mutate(Wells = paste(row, col, sep = ""))

# read in names for the samples
quant2 <- dplyr::left_join(dat2, firsts, by = "Wells")
quant2 <- quant2 %>%
  ### THIS DEPENDS ON THE TYPE OF SAMPLE YOU ARE LOOKING AT extraction_id, digest_id, etc) ###
  select(contains("id"), AdjConc) %>% 
  rename(quant = AdjConc)

# import into database
lab <- write_db("Laboratory")
exist <- dbReadTable(lab, table)

# select the rows for extraction ids that are in this quantification and remove the quant column
change <- exist %>% 
  filter(extraction_id %in% quant2$extraction_id) %>% 
  select(-quant)

# remove changed rows from exist
exist <- anti_join(exist, change, by = "extraction_id")
  
# add the quantifications to 
change <- left_join(change, quant2, by = "extraction_id")

# replace the rows in exist
exist <- rbind(exist, change)

# add back to database
# dbWriteTable(lab, table, exist, row.names = F, overwrite = T)
# dbDisconnect(lab)
