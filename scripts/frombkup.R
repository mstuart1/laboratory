# access backups of the db while amphiprion is down 
library(dplyr)
library(readr)
library(stringr)

bkup <- "~/Documents/db_backups/extraction_2017-09-12_10-37 AM.sql"
# BEFORE READING IN THIS FILE, GO INTO SUBLIME AND REPLACE ALL OF THE ' WITH NOTHING AND ALL OF THE () WITH NOTHING ########################
# read the sql dump

#lines to skip in extraction backup is 47

thing <- read_csv(file = bkup, skip = 49, col_names = c("extraction_id", "sample_id", "date", "method", "final_vol", "quant", "gel", "well", "plate", "notes", "correction", "corr_message", "corr_editor", "corr_date"));

# clean up the table - remove the non-data lines
nothing <- thing %>% 
  filter(grepl("INSERT", extraction_id) |
      grepl("40101", extraction_id) |
      grepl("UNLOCK", extraction_id) |
      grepl("40000", extraction_id) |
      grepl("VALUES", extraction_id) |
      grepl("SET", extraction_id)
    )
thing <- anti_join(thing, nothing)

# THING SHOULD NOW BE THE TABLE AS IT WAS IN THE DATABASE ### THERE COULD BE ERRORS, BE CAREFUL!!! #####

