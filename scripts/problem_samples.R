# samples that did not sequence well
source("scripts/lab_helpers.R")
# library(ggplot2)

# get the list of missing samples from amphiprion that have been edited to just Ligation number in text editor using the find: (APCL_\d+)(L\d+) and the replace: \2 and the find: (APCL_)(L\d+) and the replace: \2
fails <- read.csv("data/lowDP.indv.csv", header = F)
names(fails) <- "ligation_id"

# find sample_ids and extract ids
lab <- read_db("Laboratory")
ligs <- lab %>% 
  tbl("ligation") %>% 
  collect() %>% 
  filter(ligation_id %in% fails$ligation_id) %>% 
  select(ligation_id, digest_id)

digs <- lab %>% 
  tbl("digest") %>% 
  collect() %>% 
  filter(digest_id %in% ligs$digest_id) %>% 
  select(digest_id, extraction_id, quant) %>% 
  rename(dig_quant = quant)

digs <- left_join(ligs, digs, by = "digest_id")
rm(ligs)

sample <- lab %>% 
  tbl("extraction") %>% 
  collect() %>% 
  filter(extraction_id %in% digs$extraction_id) %>% 
  select(extraction_id, sample_id, quant) %>% 
  rename(extr_quant = quant)

sample <- left_join(digs, sample, by = "extraction_id")
rm(digs)  

# make a list of samples that need to be re-processed
sample <- sample %>% 
  select(sample_id, extraction_id, extr_quant, digest_id, dig_quant, ligation_id) %>% 
  arrange(sample_id)

# inform the leyte database that these samples failed sequencing
nam <- sample %>% 
  distinct(sample_id)

leyte <- write_db("Leyte")
clownfish <- dbReadTable(leyte, "clownfish")
change <- clownfish %>% 
  filter(sample_id %in% nam$sample_id)

test <- anti_join(nam, change, by = "sample_id")

# find out if these extractions were ligated successfully
digs <- lab %>%
  tbl("digest") %>% 
  filter(extraction_id %in% sample$extraction_id) %>% 
  select(digest_id, extraction_id) %>% 
  collect()

ligs <- lab %>%
  tbl("ligation") %>% 
  filter(digest_id %in% digs$digest_id, 
    !ligation_id %in% sample$ligation_id, 
    notes != "PCR failed", 
    ligation_id != "L0548"
    ) %>% 
  select(ligation_id, digest_id, notes) %>% 
  collect()

ligs <- left_join(ligs, digs, by = "digest_id")
rm(digs)

# remove successful ligations from the samples
sample <- anti_join(sample, ligs, by = "extraction_id") %>% 
  arrange(extr_quant, sample_id)

# write.csv(sample, paste("data/samples_to_retry", Sys.Date(), ".csv", sep = ""), row.names = F)

# plot a histogram of extr_quants
ggplot(data = sample) +
  geom_histogram(mapping = aes(x=extr_quant), binwidth = 10)

# based on this histogram, maybe do a low concentration enzyme digest with any samples with a quant lower than 10?

low_conc <- sample %>% 
  filter(extr_quant < 10)
  

ggplot(data = low_conc) +
  geom_histogram(mapping = aes(x = extr_quant), binwidth = 0.5)

# in order to make a plate of low concentration samples to digest, need to import locations from db
low_conc <- low_conc %>% 
  distinct(extraction_id)

low_conc_plate <- lab %>% 
  tbl("extraction") %>% 
  filter(extraction_id %in% low_conc$extraction_id) %>% 
  select(extraction_id, sample_id, quant, well, plate, notes) %>% 
  collect() %>% 
  filter(!grepl("empty", notes)) # remove empty wells
  

# get a list of plate names and the count of samples from each plate
counts <- low_conc_plate %>% 
  group_by(plate) %>% 
  summarise(samples = n()) %>% # count the number of samples to be digested on each plate
  arrange(plate)

# add a digest plate name
low_conc_plate <- low_conc_plate %>% 
  mutate(dig_plate = "low_conc_plate") %>% 
  arrange(extraction_id) # sort by extraction_id
  
plate_master <- data.frame( row = rep(LETTERS[1:8], 12), col = unlist(lapply(1:12, rep, 8)))
plate_master <- plate_master %>% 
      mutate(dig_well = paste(row, col, sep = ""))
plate_master <- plate_master[1:nrow(low_conc_plate), ] 
low_conc_plate <- cbind(low_conc_plate, plate_master) 
low_conc_plate <- low_conc_plate %>% 
  mutate(col = formatC(as.numeric(col), width = 2, format = "d", flag = "0"))%>% 
  arrange(col, row)
low_conc_plate <- low_conc_plate %>% 
  mutate(digest_id = 1:nrow(low_conc_plate))


  
  
