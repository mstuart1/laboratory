# edit notes in db
library(RMySQL)
library(dplyr)
lab <- dbConnect(MySQL(), "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# THIS DEPENDS ON WHICH TABLE YOU WANT TO WORK WITH 

#for the most recent use of this script, I am going to change the notes on a group of extractions

# pull in all extractions
extr <- lab %>% dbReadTable("extraction") %>% collect()

which(duplicated(extr$extraction_id) == T)
# change the values for a group of samples - this pulls out only the few that fit the filter
change <- extr %>% 
  filter(plate == "E3631-E3724") %>% 
  mutate(notes = "fins have been loaded into plates and lysed")
  # mutate(date = "2017-08-29", notes = "")

# remove those few from the whole group (the extraction ids are for prechange rows)
extr <- anti_join(extr, change, by = "extraction_id")

# add in the changed rows
extr <- rbind(extr, change)

# write the changes to the db
################### BE CAREFUL ########################################
dbWriteTable(lab, "extraction", extr, row.names = F, overwrite = T)

dbDisconnect(lab)
rm(lab)

