# prep samples for digest #### 
# this script will examine the extraction table, find candidates for future digests, and place them in a digest plate plan

# because it is easiest to use a multichannel pipet to pipet from plate to plate, changing this code to arrange plate then fill holes. 10/17/2017

# connect to the database
library(dplyr)
source("scripts/lab_helpers.R")

lab <- write_db("Laboratory")

# get a list of digested samples
dig <- dbReadTable(lab, "digest")

# # get list of extracted samples that are digestable
# extr <- dbReadTable(lab, "extraction") %>% 
#   filter(extraction_id > "E0036") %>%  # must be part of the current method
#   filter(quant > 5) %>%  # must have at least 5ng/ul concentration to digest
#   filter(grepl("APCL", sample_id)) %>%  # must be part of the current project
#   filter(!extraction_id %in% dig$extraction_id) %>%  # not digested yet
#   select(extraction_id, well, plate, quant) %>% 
#   arrange(extraction_id)

############### special case ##############################
# get a list of extractions from csv
extr <- read.csv("data/2018_rescue_samples - attempt_to_rescue.csv")

# for the dilute rounds of digest, filter down to samples < 10uL - should be 289
extr <- extr %>% 
  filter(extr_quant < 10) %>% 
  select(extraction_id, well, plate, extr_quant) %>% 
  arrange(extraction_id)

#############################################

# get a list of plate names and the count of samples from each plate
counts <- extr %>% 
  group_by(plate) %>% 
  summarise(samples = n()) %>% # count the number of samples to be digested on each plate
  # filter(samples > 80) %>%  # remove plates that are not full or not almost full
  arrange(plate)

# # get a list of the extraction samples that are in these plates
in_plates <- extr %>%
  filter(plate %in% counts$plate)
# 
# # get a list of samples that are available to fill in the gaps
# fills <- anti_join(extr, in_plates, by = "extraction_id")
# 
# rm(dig)

digest <- data.frame()
# set up the plates
for (i in 1:nrow(counts)){ # for all of the plates listed in the counts table
  temp <- in_plates %>%
    filter(plate == counts$plate[i]) %>% # find the extracts in that plate
    mutate(dig_plate = paste("plate", i, sep = "")) %>% # add a digest plate name
    mutate(dig_well = well) # add a destination well
  x <- nrow(temp)
  temp[x+1, ] <- c("XXXX", "D2", "blank", 0, paste("plate", i, sep = ""), "D2") # add blanks
  temp[x+2, ] <- c("XXXX", "E8", "blank", 0, paste("plate", i, sep = ""), "E8")
  temp <- temp %>% 
    mutate(row = substr(dig_well, 1, 1),
      col = substr(dig_well, 2, 3)) 
  if(nrow(temp) < 96){ # if the plate is not full
    # find the empty wells
    plate_master <- data.frame( row = rep(LETTERS[1:8], 12), col = unlist(lapply(1:12, rep, 8)))
    plate_master <- plate_master %>% 
      mutate(well = paste(row, col, sep = ""))
    holes <- anti_join(plate_master, temp, by = "well") %>% 
      rename(dig_well = well)
    # fill in the holes
    for (j in 1:nrow(holes)){
      temp2 <- fills[j, ] # take the first row of fills
      temp2 <- temp2 %>% 
        mutate(
          dig_plate = temp$dig_plate[1], # add the needed columns for the digest plate
          dig_well = holes$dig_well[j], 
          row = holes$row[j], 
          col = holes$col[j])
      fills <- anti_join(fills, temp2) # remove the fill from the fill table
      temp <- rbind(temp, temp2) # add it to the digest plate
    }
  }
  digest <- rbind(digest, temp) 
  digest <- digest %>%   # attach to master list of digests
    mutate(col = formatC(as.numeric(col), width = 2, format = "d", flag = "0")) %>% 
    arrange(dig_plate, col, row) 
  rm(temp, i, x)
}

digest <- digest %>% 
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
digest$notes <- "digests planned for October 2017 by MRS"

# select columns for db
digest <- digest %>% 
  mutate(date = NA) %>% 
  mutate(vol_in = "30") %>% # the volume used in this project
  mutate(ng_in = 30 * as.numeric(quant)) %>% 
  mutate(enzymes = "PstI_MluCI") %>% # the enzymes used in this project 
  mutate(final_vol = NA) %>% 
  mutate(quant = NA) %>% 
  mutate(correction = NA) %>%
  mutate(corr_message = NA) %>% 
  mutate(corr_editor = NA) %>% 
  mutate(corr_date = NA) %>% 
  select(digest_id, extraction_id, date, vol_in, ng_in, enzymes, final_vol, quant, dig_well, dig_plate, notes, correction, corr_message, corr_editor, corr_date)

nplates <- digest %>% 
  select(dig_plate) %>% 
  distinct() 

# change plate name to match range
for (i in 1:nrow(nplates)){
  x <- paste("plate", i, sep = "")
  name <- digest %>% 
    filter(dig_plate == x)
  if (nrow(name) > 0){
    digest <- anti_join(digest, name, by = "dig_plate") # remove these rows from digest
    a <- name %>% filter(dig_well == "A1") %>% select(digest_id) # get the first digest
    b <- name %>% filter(dig_well == "H12") %>% select(digest_id) # get the last digest
    name$dig_plate <- paste(a, "-", b, sep = "")
    digest <- rbind(digest, name) # add rows back in to extr
  }
}

digest <- digest %>% 
  rename(well = dig_well, 
    plate = dig_plate)

### import the digest list into the database ####
############# BE CAREFUL #################################
# lab <- write_db("Laboratory")
# 
# dbWriteTable(lab, "digest", digest, row.names = F, overwrite = F, append = T)
# 
# dbDisconnect(lab)
# rm(lab)

# # make plate maps to copy elsewhere or print ####
# 
# # connect to the database
# lab <- read_db("Laboratory")
# 
# # pull in digest plate from the database (or list of plates) - based on planning notes
# digs <- lab %>% 
#   tbl("digest") %>%
#   collect() %>% 
#   filter(grepl("October 2017", notes))
# 
# digests <- digs %>% 
#   distinct(plate)
# 
# for (i in 1:nrow(digests)){ # break it down into one plate at a time
#   # make a platemap for where extracts are supposed to end up
#   temp <- digs %>% 
#     filter(plate == digests$plate[i])
#   plate <- plate_from_db(temp, "extraction_id") # this will give an error if you try to do more than one plate at once
#   write.csv(platemap, file = paste("output/", Sys.Date(), "_extract_dest_plate", i, ".csv", sep = ""))
#   
#   # repeat for what the digest names will be 
#   plate <- plate_from_db(temp, "digest_id")
#   write.csv(platemap, file = paste("output/", Sys.Date(), "_digest_dest_plate", i, ".csv", sep = ""))
#   
#   # get plate maps for sources
#   extr <- lab %>%
#     tbl("extraction") %>% 
#     filter(extraction_id %in% temp$extraction_id) %>% 
#     collect()
#   
#   extr_plates <- extr %>% 
#     distinct(plate)
#   
#   # need whole plates from extractions, so create a new table of all of the extracts in source plates
#   source <- lab %>% 
#     tbl("extraction") %>% 
#     collect() %>% 
#     filter(plate %in% extr_plates$plate) 
#     
#   
#   for(j in 1:nrow(extr_plates)){
#     temp2 <- source %>% 
#       filter(plate == extr_plates$plate[j])
#     plate <- plate_from_db(temp2, "extraction_id") # this will give an error if you try to do more than one plate at once
#     write.csv(platemap, file = paste("output/", Sys.Date(), "_extract_source_plate", i,"_", j, ".csv", sep = ""))
#     
#   }
#   
# }
# rm(temp, temp2, i, j, plate, platemap, digests, extr_plates)
# 
# # find any extracts in the plan that need to be diluted (add 15uL sample plus
# 15uL water instead of 30uL sample)
digs <- lab %>%
  tbl("digest") %>% 
  collect() %>% 
  filter(grepl("October 2017", notes))

extr <- lab %>%
  tbl("extraction") %>%
  filter(extraction_id %in% digs$extraction_id) %>%
  mutate(DNA = quant * 30) %>% # multiply the quant by 30 to calculate how much DNA will be digested
  filter(DNA > 5000) %>% # find any samples that will contain more than 5ug in the digest
  arrange(extraction_id) %>%
  collect()
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


  


