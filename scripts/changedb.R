# edit notes in db
source("scripts/lab_helpers.R")
library(dplyr)
lab <- write_db("Laboratory")

# THIS DEPENDS ON WHICH TABLE YOU WANT TO WORK WITH 

#for the most recent use of this script, I am going to change the notes on a group of extractions

# pull in all extractions
extr <- lab %>% dbReadTable("extraction") %>% collect()

# which(duplicated(extr$extraction_id) == T) # a test to see if any extraction ids are duplicated

change <- extr %>% 
  filter(plate == "E4007-E4100" | plate == "E3819-E3912" | plate == "E3913-E4006") %>% 
  mutate(notes = "fins have been loaded into plates and lysed")
  # mutate(date = "2017-08-29", notes = "")
  

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

