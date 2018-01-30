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

# which files
platefile1 = "data/2018-01-30-plate1_2a.txt"

files <- c(platefile1)

# for (i in seq(files)){
#   for (j in 1:)

# skip the lines before the data
strs <- readLines(i, skipNul = T)
linestoskip = (which(strs == "Group: Unknowns")) # the number of lines to skip

# create a table that contains well locations and quantities
dat1 <- read.table(text = strs,  skip = linestoskip, sep = "\t", fill = T, header = T, stringsAsFactors = F)

# remove footer rows
dat1 <- dat1[1:(which(dat1$Sample == "Group Summaries")-1), ]

# special #### on 2018-01-30 I used to standards for plate 1 for half of plate 2 ####
dat2 <- dat1 %>%
  filter(Sample == 89 | Sample == " ")

dat1 <- anti_join(dat1, dat2)

# read in names for the samples
lab <- write_db("Laboratory")

# all of the data in the db for this table (for example, "digest")
dat <- dbReadTable(lab, table)

# select your desired plate
plate <- dat %>%
  select(contains("id"), well, plate) %>%
  filter(plate == range[1]) %>%
  collect()

# special ####
# quant1 <- left_join(dat1, reader, by = c("Wells" = "well"))
quant1 <- left_join(dat1, plate, by = c("Wells" = "well"))
quant1 <- quant1 %>%
  select(contains("id"), AdjConc) %>% 
  # remove any empty wells
  filter(!is.na(extraction_id)) %>% 
  # rename the quant column
  rename(quant = AdjConc)


############### add this information to the database ##################
# BE CAREFUL #

# the entire table was pulled in as dat above

change <- dat %>%
  filter(plate == range[1]) %>% 
  select(-quant) # don't bring in the quant column, will add that here

# add in the new quants
change <- left_join(change, quant1, by = c("sample_id", "extraction_id"))

dat <- change_rows(dat, change, "extraction_id")

# write the group back to the database
# dbWriteTable(lab, table, dat, row.names = F, overwrite = T)
# dbDisconnect(lab)


# import the firsts #### 
# this is for the first column of each plate that was put onto a separate plate to make room for the standards

strs <- readLines(platefile3, skipNul = T)
linestoskip = (which(strs == "Group: Unknowns")) # the number of lines to skip

dat2 <- read.table(text = strs,  skip = linestoskip, sep = "\t", fill = T, header = T, stringsAsFactors = F)

# remove footer rows
dat2 <- dat2[1:(which(dat2$Sample == "Group Summaries")-1), ]

x <- as.character(i+1)

# pull first column samples from database
firsts <- dat %>% 
  filter(plate == range[1], is.na(quant)) %>% 
  # replace the 1 with whatever column of the firsts plate
  mutate(well = str_replace(well, "1", x)) %>% 
  select(extraction_id, well)

# read in names for the samples
quant2 <- left_join(dat2, firsts, by = c("Wells" = "well"))
quant2 <- quant2 %>%
  ### THIS DEPENDS ON THE TYPE OF SAMPLE YOU ARE LOOKING AT extraction_id, digest_id, etc) ###
  select(contains("id"), AdjConc) %>% 
  rename(quant = AdjConc) %>% 
  filter(!is.na(extraction_id))
  # filter(!is.na(digest_id))

# import into database
lab <- write_db("Laboratory")
dat <- dbReadTable(lab, table)

# select the samples that will be changed
change <- dat %>%
  filter(extraction_id %in% quant2$extraction_id) %>% 
  # filter(digest_id %in% quant2$digest_id) %>% 
  select(-quant)

# add in the new quants
# change <- left_join(change, quant2, by = "digest_id")
change <- left_join(change, quant2, by = "extraction_id")

dat <- change_rows(dat, change, "extraction_id")
# dat <- change_rows(dat, change, "digest_id")

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
