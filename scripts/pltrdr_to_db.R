# Read in quantification results ------------------------------------------
library(dplyr)
library(RMySQL)
library(stringr)
source("scripts/lab_helpers.R")

# what types of samples are these
table = "extraction"
# table = "digest"
# table = "ligation"

# which plate is it?
range = "E4195-E4288"

# read in plate reader data for the first plate
platefile1 = "data/2017-10-09-plate12.txt"
platefile2 = "data/2017-10-09-plate14.txt"
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
  select(contains("id"), AdjConc)

# remove any empty wells and rename the column
quant1 <- quant1 %>%
#   filter(!is.na(1)) %>% 
  rename(quant = AdjConc)
# 
############### add this information to the database ##################
# BE CAREFUL #

# the entire table was pulled in as dat above

# select the samples that will be changed
change <- dat %>%
  filter(plate == range) %>% 
  select(-quant)

# add in the new quants
change <- left_join(change, quant1, by = c("digest_id", "extraction_id"))

dat <- change_rows(dat, change, "digest_id")

# write the group back to the database
# dbWriteTable(lab, table, dat, row.names = F, overwrite = T)
# dbDisconnect(lab)


# import the firsts #### 
# this is for the first column of each plate that was put onto a separate plate to make room for the standards

strs <- readLines(platefile2, skipNul = T)
linestoskip = (which(strs == "Group: Unknowns")) # the number of lines to skip

dat2 <- read.table(text = strs,  skip = linestoskip, sep = "\t", fill = T, header = T, stringsAsFactors = F)

# remove footer rows
dat2 <- dat2[1:(which(dat2$Sample == "Group Summaries")-1), ]

# pull first column samples from database
firsts <- dat %>% 
  filter(plate == range, is.na(quant)) %>% 
  mutate(well = str_replace(well, "1", "7")) %>% # replace the 1 with whatever column of the firsts plate
  select(extraction_id, well)

# read in names for the samples
quant2 <- left_join(dat2, firsts, by = c("Wells" = "well"))
quant2 <- quant2 %>%
  ### THIS DEPENDS ON THE TYPE OF SAMPLE YOU ARE LOOKING AT extraction_id, digest_id, etc) ###
  select(contains("id"), AdjConc) %>% 
  rename(quant = AdjConc) %>% 
  filter(!is.na(extraction_id))

# import into database
lab <- write_db("Laboratory")
dat <- dbReadTable(lab, table)

# select the samples that will be changed
change <- dat %>%
  filter(digest_id %in% quant2$digest_id) %>% 
  select(-quant)

# add in the new quants
change <- left_join(change, quant2, by = "digest_id")

dat <- change_rows(dat, change, "digest_id")

change <- dat %>%
  filter(plate == range) %>% 
  mutate(final_vol = 40)

dat <- change_rows(dat, change, "digest_id")

# write the group back to the database
# dbWriteTable(lab, table, dat, row.names = F, overwrite = T)
# dbDisconnect(lab)

# which samples are too low to digest with the regular concentration of enzyme?
test <- dat %>% 
  filter(quant < 40 & sample_id != "XXXX")
