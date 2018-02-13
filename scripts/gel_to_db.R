# edit notes in db for gel results
source("scripts/lab_helpers.R")
library(dplyr)

lab <- write_db("Laboratory")

# THIS DEPENDS ON WHICH TABLE YOU WANT TO WORK WITH 

#for the most recent use of this script, I am going to change the notes on a group of extractions
# pull in all extractions
extr <- lab %>% dbReadTable("extraction") %>% collect()

# which(duplicated(extr$extraction_id) == T) # a test to see if any extraction ids are duplicated

# change the values for a group of samples - this pulls out only the few that fit the filter
# fails <- c("E3081", "E3113", "E3075", "E3124")
wells <- c("D2", "E8", "A6", "A7", "A8", "B11")

change <- extr %>% 
  filter(plate == "E3913-E4006")  %>%
  mutate(gel = "2018-02-12", 
    # double check that these notes are already empty ####
    notes = ifelse(well %in% wells, "no band", notes)) 
  # filter(extraction_id %in% fails) %>%
  # filter(extraction_id == "E4523") %>% 
  # mutate(notes = "no band on gel") # made sure to check that no pre-existing notes present

# remove those few from the whole group (the extraction ids are for prechange rows)
extr <- anti_join(extr, change, by = "extraction_id")

# add in the changed rows
extr <- rbind(extr, change)

# write the changes to the db
################### BE CAREFUL ########################################
# dbWriteTable(lab, "extraction", extr, row.names = F, overwrite = T)
# 
# dbDisconnect(lab)
# rm(lab)

