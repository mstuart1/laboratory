# Read in quantification results ------------------------------------------
library(dplyr)
library(RMySQL)

# what types of samples are these
table = "digest"

# which plate is it?
range = "D3247-D3339"


# read in plate reader data for the first plate
platefile1 = "/Users/macair/Google Drive/Pinsky Lab/Plate Reader/2016_09/20160919_plate1.txt"
# colsinplate1 = 2:12 # is this a full plate?

strs <- readLines(platefile1, skipNul = T)
linestoskip = (which(strs == "Group: Unk_Dilution")) # the number of lines to skip

# create a table that contains well locations and quantities
dat1 <- read.table(text = strs,  skip = linestoskip, sep = "\t", fill = T, header = T, stringsAsFactors = F)

# remove footer rows
dat1 <- dat1[1:(which(dat1$Sample == "Group Summaries")-1), ]

# read in names for the samples
lab <- dbConnect(MySQL(), "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

dat <- dbReadTable(lab, table)

# choose a plate 
plate <- dat %>% 
  select(contains("id"), well, plate) %>% 
  filter(plate == range) %>% 
  collect()


quant1 <- dplyr::left_join(dat1, plate, by = c("Wells" = "well"))
quant1 <- quant1 %>% 
  ### THIS DEPENDS ON THE TYPE OF SAMPLE YOU ARE LOOKING AT extraction_id, digest_id, etc) ###
  select(contains("id"), AdjResult)

# repeat for plate containing first column samples
platefile2 = "/Users/macair/Google Drive/Pinsky Lab/Plate Reader/2016_09/20160919_plate2.txt"

strs <- readLines(platefile2, skipNul = T)
linestoskip = (which(strs == "Group: Unk_Dilution")) # the number of lines to skip

dat2 <- read.table(text = strs,  skip = linestoskip, sep = "\t", fill = T, header = T, stringsAsFactors = F)

# remove footer rows
dat2 <- dat2[1:(which(dat2$Sample == "Group Summaries")-1), ]

# read in names for the samples
quant2 <- dplyr::left_join(dat2, plate, by = c("Wells" = "well"))
quant2 <- quant2 %>% 
  ### THIS DEPENDS ON THE TYPE OF SAMPLE YOU ARE LOOKING AT extraction_id, digest_id, etc) ###
  select(contains("id"), AdjResult)

# join the 2 results lists
quant <- rbind(quant1, quant2)

# remove any empty wells
quant <- quant %>% 
  filter(!is.na(1)) %>% 
  rename(quant = AdjResult)

############### add this information to the database ##################
# BE CAREFUL #

# the entire table was pulled in as dat above

# select the samples that will be changed
change <- dat %>%
  filter(plate == range)

# remove the samples that will be changed from the main group
whole <- anti_join(dat, change)

# incorporate the quantification
change <- change %>% 
  select(-quant)

change <- left_join(change, quant)

# add the changes back to the group
whole <- rbind(whole, change)

# write the group back to the database
# dbWriteTable(lab, table, whole, row.names = F, overwrite = T)
