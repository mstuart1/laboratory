# Read in quantification results ------------------------------------------
library(dplyr)
library(RMySQL)
library(stringr)
source("scripts/lab_helpers.R")

# what types of samples are these
table = "extraction"
# table = "digest"
# table = "ligation"

# which plates?
range <- c("E4405-E4477", "E4488-E4550")
# range <- c("D3916-D4011", "D4012-D4107", "D4108-D4203", "D4204-D4299", "D4300-D4395", "D4396-D4491", "D4492-D4587")


# # special ####
# # read in the plate reader locations
# reader <- read.csv("data/blank_plate.csv") %>% 
#   rename(digest_id = X) %>% 
#   filter(grepl("D", digest_id)) %>% 
#   mutate(well = paste(Row, Col, sep = ""))

# for (i in 3:length(range)){
# read in plate reader data for the first plate
platefile1 = "data/2018-01-30-plate1_2a.txt"
# platefile1 = paste("data/2017-11-27-plate", i, ".txt", sep = "")
platefile2 = "data/2018-01-30-plate2b.txt"
platefile3 = "data/2018-01-30-plate3.txt"
# colsinplate1 = 2:12 # is this a full plate?

files <- c(platefile1, platefile2, platefile3)
for (i in 1:nrow){


# skip the lines before the data
strs <- readLines(i, skipNul = T)
linestoskip = (which(strs == "Group: Unknowns")) # the number of lines to skip

# create a table that contains well locations and quantities
dat1 <- read.table(text = strs,  skip = linestoskip, sep = "\t", fill = T, header = T, stringsAsFactors = F)

# remove footer rows
dat1 <- dat1[1:(which(dat1$Sample == "Group Summaries")-1), ]

# special #### on 2018-01-30 I used to standards for plate 1 for half of plate 2 ####
# dat2 <- dat1 %>% 
#   filter(Sample == 89 | Sample == " ")
# 
# dat1 <- anti_join(dat1, dat2)

# read in names for the samples
lab <- write_db("Laboratory")

# all of the data in the db for this table (for example, "digest")
dat <- dbReadTable(lab, table)

# select your desired plate
plate <- dat %>%
  select(contains("id"), well, plate) %>%
  filter(plate == range[i]) %>%
  collect()

# special ####
# quant1 <- left_join(dat1, reader, by = c("Wells" = "well"))
quant1 <- dplyr::left_join(dat1, plate, by = c("Wells" = "well"))
quant1 <- quant1 %>%
  select(contains("id"), AdjConc)

# remove any empty wells and rename the column
quant1 <- quant1 %>%
#   filter(!is.na(1)) %>% 
  rename(quant = AdjConc)
 
############### add this information to the database ##################
# BE CAREFUL #

# the entire table was pulled in as dat above

# select the samples that will be changed
# special
# quant1 <- quant1 %>% 
#   select(digest_id, AdjConc) %>% 
#   rename(quant = AdjConc)
# change <- dat %>%
#   filter(digest_id %in% reader$digest_id)
# # do any have notes?
# test <- change %>% 
#   filter(!is.na(notes))
# change <- change %>% 
#   rename(old_quant = quant)
# change <- left_join(change, quant1, by = "digest_id")
# change <- change %>% 
#   mutate(notes = ifelse(is.na(old_quant), notes, paste("Requantified on 12-04-2017, original quant was ", old_quant, sep = "")))
# change <- change %>% select(-old_quant)


change <- dat %>%
  filter(plate == range[i]) %>% 
  select(-quant) # don't bring in the quant column, will add that here

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

x <- as.character(i+1)

# pull first column samples from database
firsts <- dat %>% 
  filter(plate == range[i], is.na(quant)) %>% 
  mutate(well = str_replace(well, "1", x)) %>% # replace the 1 with whatever column of the firsts plate
  # select(extraction_id, well)
  select(digest_id, well)

# read in names for the samples
quant2 <- left_join(dat2, firsts, by = c("Wells" = "well"))
quant2 <- quant2 %>%
  ### THIS DEPENDS ON THE TYPE OF SAMPLE YOU ARE LOOKING AT extraction_id, digest_id, etc) ###
  select(contains("id"), AdjConc) %>% 
  rename(quant = AdjConc) %>% 
  # filter(!is.na(extraction_id))
  filter(!is.na(digest_id))

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

# update final volume of samples
change <- dat %>%
  filter(plate == range[i]) %>% 
  mutate(final_vol = 40)

dat <- change_rows(dat, change, "digest_id")

# write the group back to the database
# dbWriteTable(lab, table, dat, row.names = F, overwrite = T)
# dbDisconnect(lab)
}

# # which samples are too low to digest with the regular concentration of enzyme?
# test <- dat %>% 
#   filter(quant < 40 & sample_id != "XXXX")
