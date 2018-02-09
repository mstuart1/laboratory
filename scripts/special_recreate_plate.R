# special - recreate a list of samples and plate locations from a platemap csv
source("scripts/lab_helpers.R")

map <- "~/Downloads/Untitled spreadsheet - Sheet1.csv" 

sheet <- read.csv(map)

# rename the columns
colnames(sheet) <- c("Row", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
# turn the info into table format
redigs <- reshape2::melt(sheet, id.vars = "Row", variable.name = "Col", value.name = "Sample")

# remove empty wells
redigs <- redigs %>% 
  # remove empty wells
  filter(!is.na(Sample), Sample != "") %>% 
  rename(extraction_id = Sample) 

redigs <- redigs %>% 
  # name the plate
  mutate(plate = "redigest3", 
    # create a unique digest_id
    digest_id = 1:nrow(redigs), 
    # create a well column
    dig_well = paste(Row, Col, sep = ""))


# send the digests to the database ####
lab <- write_db("Laboratory")

digs <- dbReadTable(lab, "digest")
dig_max <- digs %>% 
  summarise(x = max(digest_id)) %>% 
  mutate(x = substr(x, 2, 5))

redigs <- redigs %>% 
  mutate(digest_id = digest_id + as.numeric(dig_max$x)) %>% 
  mutate(digest_id = paste("D", digest_id, sep = "")) %>% 
  select(digest_id, extraction_id, dig_well)
min_dig <- redigs %>% 
  summarize(x = min(digest_id))
max_dig <- redigs %>% 
  summarize(y = max(digest_id))

redigs <- redigs %>% 
  mutate(
    date = NA, 
    vol_in = 30, 
    ng_in = NA, 
    enzymes = "PstI-MluCI",
    final_vol = NA, 
    quant = NA, 
    notes = NA,
    correction = NA, 
    corr_message = NA, 
    corr_editor = NA, 
    corr_date = NA, 
    plate = paste(min_dig, "-", max_dig, sep = "")
  ) %>% 
  rename(well = dig_well)

digs <- rbind(digs, redigs)

# dbWriteTable(lab, "digest", digs, row.names = F, overwrite = T)

####################################################################
# for samples that don't have a csv map ####

long <- read.csv("data/redigest_these_samples.csv", stringsAsFactors = F)
# narrow down to the samples you need
long <- long %>% 
  filter(extraction_id >= "E2738" & extraction_id <= "E2833")

# pull well info from extracts
redigs <- dbReadTable(lab, "extraction") %>% 
  filter(extraction_id %in% long$extraction_id) %>% 
  select(extraction_id, well, plate) %>% 
  rename(dig_well = well) %>% 
  mutate(Row = substr(dig_well, 1, 1), 
    Col = substr(dig_well, 2, 3), 
    digest_id = 1:nrow(long))

# go to line 28 ####
