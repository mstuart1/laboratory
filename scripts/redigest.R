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
  filter(plate == "E2258-E2353") %>% 
  mutate(dig_well = well)
redigs <- sub
sub <- extr %>% 
  filter(plate == "E2642-E2737" & 
           !extraction_id %in% c("E2643", "E2644", "E2645", "E2650", "E2652", "E2653", "E2654", "E2656", "E2657", "E2658", "E2662", "E2678", "E2696", "E2697", "E2732")) %>% 
  mutate(dig_well = c("B6", "C6", "G11", "E12", "G12"))

redigs <- rbind(redigs, sub)
sub <- extr %>% 
  filter(plate == "E1207-E1302" & !extraction_id %in% c("E1208", "E1243", "E1250", "E1253", "E1254")) %>% 
  mutate(dig_well = c("F1", "A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2", "C5"))
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

