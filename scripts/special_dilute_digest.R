# prep samples for digest #### 
# this script will examine the extraction table, find candidates for future digests, and place them in a digest plate plan

# because it is easiest to use a multichannel pipet to pipet from plate to plate, changing this code to arrange plate then fill holes. 10/17/2017

# connect to the database
source("scripts/lab_helpers.R")

lab <- write_db("Laboratory")

# get a list of extractions from csv
extr <- read.csv("data/2018_rescue_samples - attempt_to_rescue.csv", stringsAsFactors = F)

# for the dilute rounds of digest, filter down to samples < 10uL - should be 289
extr <- extr %>% 
  filter(extr_quant < 10) %>% 
  select(extraction_id, well, plate, extr_quant) %>% 
  distinct() %>% 
  arrange(extraction_id)

# get a list of plate names and the count of samples from each plate
counts <- extr %>% 
  group_by(plate) %>% 
  summarise(samples = n()) %>% # count the number of samples to be digested on each plate
  # filter(samples > 80) %>%  # remove plates that are not full or not almost full
  arrange(plate)

group1 <- extr %>% 
  filter(plate == "E0535-E0630"| 
    plate == "E0631-E0726"| 
    plate == "E0727-E0822"| 
    plate == "E0823-E0918"| 
    plate == "E1207-E1302"| 
      plate == "E2738-E2833" |
      plate == "E0247-E0342" |
      plate == "E1015-E1110"|
    plate == "E1303-E1398")

# remove group1 from extr
extr <- anti_join(extr, group1, by = "extraction_id")

group2 <- extr %>% 
  filter(plate == "E0919-E1014" |
      plate == "E2258-E2353"|
      plate == "E2450-E2545"|
      plate == "E2546-E2641"|
      plate == "E2642-E2737")

extr <- anti_join(extr, group2, by = "extraction_id")

digest <- data.frame()
plate <- data.frame( Row = rep(LETTERS[1:8], 12), Col = unlist(lapply(1:12, rep, 8)))
plate <- mutate(plate, well = paste(Row, Col, sep = "")) %>% 
  filter(well != "D2", 
    well != "E8")

# set up the plates
  group1 <- group1 %>%
    mutate(dig_plate = "group1") %>% # add a digest plate name
    mutate(dig_well = plate$well[1:nrow(group1)]) # add a destination well
  # add blanks
  group1[95, ] <- c("XXXX", "D2", "blank", 0, "group1", "D2") # add blanks
  group1[96, ] <- c("XXXX", "E8", "blank", 0, "group1", "E8") # add blank
  digest <- rbind(digest, group1) 
  
  group2 <- group2 %>%
    mutate(dig_plate = "group2") %>% # add a digest plate name
    mutate(dig_well = plate$well[1:nrow(group2)]) # add a destination well
  # add blanks
  group2[95, ] <- c("XXXX", "D2", "blank", 0, "group2", "D2") # add blanks
  group2[96, ] <- c("XXXX", "E8", "blank", 0, "group2", "E8") # add blanks
  digest <- rbind(digest, group2) 
  
  extr <- extr %>%
    mutate(dig_plate = "extr") %>% # add a digest plate name
    mutate(dig_well = plate$well[1:nrow(extr)]) # add a destination well
  # add blanks
  extr[95, ] <- c("XXXX", "D2", "blank", 0, "extr", "D2") # add blanks
  extr[96, ] <- c("XXXX", "E8", "blank", 0, "extr", "E8") # add blanks
  digest <- rbind(digest, extr) 
  digest <- filter(digest, !is.na(extraction_id))

digest <- digest %>% 
  mutate(row = substr(dig_well, 1, 1), 
    col = as.numeric(substr(dig_well, 2, 3))) %>% 
  arrange(dig_plate, col) %>% 
  mutate(digest_id = 1:nrow(digest))

### ONLY DO THIS ONCE ###
# generate digest numbers for database ####
# get the last number used for digest and add digest_id
digested <- dbReadTable(lab, "digest") %>% 
  summarize(
    x = max(digest_id)
  )
digested[1,1] <- substr(digested[1,1], 2, 5)

# dbDisconnect(lab)
# rm(lab)

digest <- digest %>%
  mutate(digest_id = as.numeric(digested[1,1]) + as.numeric(digest_id), # add the row number to the last digest id in the database
    digest_id = paste("D", digest_id, sep = "")) # add a d


# make a note that these are planned extracts that haven't happened yet
digest$notes <- "digests planned for summer 2018 by MRS"

# select columns for db
digest <- digest %>% 
  mutate(vol_in = "30", 
    ng_in = 30 * as.numeric(extr_quant), 
    enzymes = "PstI_MluCI", 
    notes = "digest planned for summer 2018 by MRS") %>% 
  select(extraction_id, dig_plate, dig_well, digest_id, notes, vol_in, ng_in, enzymes) %>% 
  rename(plate = dig_plate, 
    well = dig_well)
  

plates <- digest %>% 
  select(plate) %>% 
  distinct()

# change plate name to match range
digest <- digest %>% 
  mutate(plate = ifelse(plate == "group1", "D5309-D5404", plate)) %>% 
  mutate(plate = ifelse(plate == "group2", "D5405-D5500", plate)) %>% 
  mutate(plate = ifelse(plate == "extr", "D5220-D5308", plate))


### import the digest list into the database ####
############# BE CAREFUL #################################
# lab <- write_db("Laboratory")
# 
# dbWriteTable(lab, "digest", digest, row.names = F, overwrite = F, append = T)

# dbDisconnect(lab)
# rm(lab)

#######################################################
# make plate maps to copy elsewhere or print ####

library(ggplot2)
library(gridExtra)

# connect to the database
lab <- read_db("Laboratory")

# pull in digest plate from the database (or list of plates) - based on planning notes
digs <- lab %>%
  tbl("digest") %>%
  collect() %>%
  filter(grepl("2018", notes), 
    !grepl("redigest", notes))

digests <- digs %>%
  distinct(plate)

for (i in 1:nrow(digests)){ # break it down into one plate at a time
  # make a platemap for where extracts are supposed to end up
  extr_dest <- digs %>% 
    filter(plate == digests$plate[i])
  map <- make_platemap(extr_dest)
  # define what will go in the grid
  t <- map
  # define the saved file location and name 
  out <- paste("plots/extr_dest_", digests$plate[i], Sys.Date(), ".pdf", sep = "")
    pdf(out, height=11, width = 8.5)
    grid.table(t)
    dev.off()
    
    # make a platemap for the digests
    dig_dest <- digs %>% 
      filter(plate == digests$plate[i]) %>% 
      select(digest_id, well)
    map <- make_platemap(dig_dest)
    # define what will go in the grid
    t <- map
    # define the saved file location and name 
    out <- paste("plots/dig_dest_", digests$plate[i], Sys.Date(), ".pdf", sep = "")
    pdf(out, height=11, width = 8.5)
    grid.table(t)
    dev.off()

  # get plate maps for sources
  extr <- lab %>%
    tbl("extraction") %>%
    filter(extraction_id %in% extr_dest$extraction_id) %>%
    collect()

  extr_plates <- extr %>%
    distinct(plate)

  # need whole platemaps from extractions, so create a new table of all of the extracts in source plates
  source <- lab %>%
    tbl("extraction") %>%
    collect() %>%
    filter(plate %in% extr_plates$plate)


  for(j in 1:nrow(extr_plates)){
    extr_source <- source %>%
      filter(plate == extr_plates$plate[j])
    plate <- plate_from_db(extr_source, "extraction_id") # this will give an error if you try to do more than one plate at once
    write.csv(platemap, file = paste("output/", Sys.Date(), "_extract_source_plate", i,"_", j, ".csv", sep = ""))

  }

}
rm(temp, temp2, i, j, plate, platemap, digests, extr_plates)


#
# # double check that 15 will reduce it enough - the following should result in zero
# fifteen <- extr %>%
#   mutate(DNA = quant * 15) %>%
#   filter(DNA > 5000) %>%
#   arrange(extraction_id)

# find these samples and make sure to highlight on plate maps, only add 15uL sample to digest

# # to print plates
# library(gridExtra)
#
# file_list <- sort(list.files(path = "output", pattern = "2017-10-18*")) # produces list of output files
#
# for (i in 1:length(file_list)){
#   infile <- paste("output/", file_list[i], sep = "")
#   t <- read.csv(infile)
#   out <- paste("output/", substr(file_list[i], 1, nchar(file_list[i])-4), ".pdf",  sep = "")
#   pdf(out, height=11, width = 8.5)
#   grid.table(t)
#   dev.off()
# }


  



