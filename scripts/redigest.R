source("scripts/lab_helpers.R")
library(gridExtra) # for printing platemaps

digest <- read.csv("data/redigest_these_samples.csv")

# for all of the extracts on the list, get well and plate locations
lab <- read_db("Laboratory")

extr <- lab %>% 
  tbl("extraction") %>% 
  filter(extraction_id %in% digest$extraction_id) %>% 
  select(extraction_id, well, plate, notes) %>% 
  arrange(plate, extraction_id) %>% 
  collect()

# get list of plates to pull extracts from
platelist1 <- distinct(extr, plate) %>% 
  filter(!is.na(plate)) %>% 
  arrange(plate)

for (i in 1:nrow(platelist1)){
  temp <- lab %>% 
    tbl("extraction") %>% 
    filter(plate == platelist1$plate[i]) %>% 
    select(extraction_id, well, plate) %>% 
    collect()
  temp2 <- plate_from_db(temp, "extraction_id")
  platemap <- as.matrix(reshape2::acast(temp2, temp2[,1] ~ temp2[,2]), value.var = temp2[,3])
  pdf(paste("data/",platelist1$plate[i], ".pdf", sep = ""), height=8.5, width=11)
  grid.table(platemap)
  dev.off()
}

####################################################################

# create digest numbers for these samples ####
sub <- extr %>% 
  filter(plate == "E2901-E2967" & extraction_id != "E2949") %>% 
  mutate(dig_well = well)
redigs <- sub
sub <- extr %>% 
  filter(plate == "E2834-E2900") %>% 
  mutate(dig_well = c("D7", "E7", "F7", "G1", "H1", "C4", "G2", "H2", "F12","D5", "F3", "A6", "G3", "F4", "G4", "H4", "E6", "H3", "G5", "H5", "G6", "H6", "B7", "G7", "H7", "D8", "F8", "G8", "H8", "A9", "B9", "F9", "G9", "H9", "F10", "G10", "H10", "F11", "G11", "H11"))
redigs <- rbind(redigs, sub)


# double check that there are no duplicate wells
test <- redigs %>% 
  group_by(dig_well) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

# formatting
redigs <- redigs %>% 
  mutate(row = substr(dig_well, 1, 1), 
         col = substr(dig_well, 2, 3)) %>% 
  arrange(as.numeric(col), row)
redigs <- mutate(redigs, digest_id = 1:nrow(redigs))

#################################################################

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
    date = "2017-11-09", 
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

