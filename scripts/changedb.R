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
  filter(plate == "E4007-E4100" | plate == "E3913-E4006") %>%
  # filter(extraction_id >= "E4289") %>% 
  # mutate(notes = "remaining fin clips don't fit into a set of 2 96 well plates, plan alternative") %>% 
  # mutate(plate = NA)
  mutate(date = "2017-10-03", notes = NA)
  # mutate(notes = NA)
  

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

