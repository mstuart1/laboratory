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

##############################################

# create digest numbers for these samples
sub <- extr %>% 
  filter(extraction_id == "E0537" | extraction_id == "E0538" | extraction_id == "E0761") %>% 
  mutate(dig_plate = "redigest1", 
         dig_well = c("A1", "B1", "C1"))
redigs <- sub
sub <- extr %>% 
  filter(plate == "E1687-E1782") %>% 
  mutate(dig_plate = "redigest1", 
         dig_well = well)
redigs <- rbind(redigs, sub)
sub <- extr %>% 
  filter(plate == "E0823-E0918") %>% 
  mutate(dig_plate = "redigest1", 
         dig_well = c("E1", "G1", "H1", "B2", "D2", "A3", "B3"))
redigs <- rbind(redigs, sub)
sub <- extr %>% 
  filter(plate == "E0919-E1014") %>% 
  mutate(dig_plate = "redigest1", 
         dig_well = c("E2", "H2"))
redigs <- rbind(redigs, sub)
sub <- extr %>% 
  filter(plate == "E1015-E1110") %>% 
  mutate(dig_plate = "redigest1", 
         dig_well = c("D4","E4", "B6"))
redigs <- rbind(redigs, sub)
sub <- extr %>% 
  filter(plate == "E1303-E1398" & extraction_id != "E1341") %>% 
  mutate(dig_plate = "redigest1", 
         dig_well = c("G5", "D5", "D7", "D11", "E11", "F11", "G11", "G7", "B10"))
redigs <- rbind(redigs, sub)
sub <- extr %>% 
  filter(plate == "E2642-E2737" & !extraction_id %in% c("E2705", "E2713", "E2724", "E2726", "E2728") %>% 
  mutate(dig_plate = "redigest1", 
         dig_well = c("G5", "D5", "D7", "D11", "E11", "F11", "G11", "G7", "B10"))
redigs <- rbind(redigs, sub)
