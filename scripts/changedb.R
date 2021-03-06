# edit notes in db
source("scripts/lab_helpers.R")
library(dplyr)
lab <- write_db("Laboratory")

# THIS DEPENDS ON WHICH TABLE YOU WANT TO WORK WITH ####

# pull in all table
# extr <- lab %>% dbReadTable("extraction") %>% collect()
digs <- lab %>% dbReadTable("digest") %>% collect()
# ligs <- lab %>% dbReadTable("ligation") %>% collect()

# change <- extr %>%
#   filter(extraction_id < "E0247", 
#     grepl("APCL", sample_id), 
#     !grepl("empty", notes)) %>% 
#   mutate(notes = ifelse(is.na(notes), "empty", paste("empty, ", notes)))


# # get quants to calculate dig ng_in
# extr <- lab %>%
#   dbReadTable("extraction") %>%
#   filter(extraction_id %in% digs$extraction_id) %>%
#   select(extraction_id, quant) %>%
#   collect() %>%
#   rename(extr_quant = quant)
# digs <- left_join(digs, extr, by = "extraction_id")

change <- digs %>%
  filter(digest_id >= "D5029", digest_id <="D5219") %>%
  mutate(final_vol = 40) 

# change <- ligs %>%
#   filter(plate == "L3171-L3266" | plate == "L3651-L3746") %>%
#   arrange(ligation_id) %>% 
#   mutate(notes = NA, 
#     date = "2018-05-16", 
#     barcode_num = rep(1:48, 4))
  # mutate(notes = ifelse(ligation_id >= "L3171", "ligation planned for January 2018", notes))

# extr <- change_rows(extr, change, "extraction_id")
digs <- change_rows(digs, change, "digest_id")

# %>% 
#   select(-extr_quant)
# ligs <- change_rows(ligs, change, "ligation_id")
# write the changes to the db
################### BE CAREFUL ########################################
# dbWriteTable(lab, "extraction", extr, row.names = F, overwrite = T)
# dbWriteTable(lab, "digest", digs, row.names = F, overwrite = T)
# dbWriteTable(lab, "ligation", ligs, row.names = F, overwrite = T)
# 
# dbDisconnect(lab)
# rm(lab)


# # #for this iteration this script, I am going to change the notes on a group of extractions
# # 
# # # pull in all extractions
# extr <- lab %>% dbReadTable("extraction") %>% collect()
# # 
# # # which(duplicated(extr$extraction_id) == T) # a test to see if any extraction ids are duplicated
# # 
# change <- extr %>%
#   filter(plate == "E0151-E0246") %>%
#   # filter(extraction_id >= "E4289") %>%
#   mutate(notes = paste("empty - ", notes, sep = ""))
#   # mutate(plate = NA)
#   # mutate(date = "2017-10-03", notes = NA)
# # mutate(notes = NA)
# # 
# # 
# # # remove those few from the whole group (the extraction ids are for prechange rows)
# extr <- anti_join(extr, change, by = "extraction_id")
# # 
# # # add in the changed rows
# extr <- rbind(extr, change)
# # 
# # # write the changes to the db
# # ################### BE CAREFUL ########################################
# # # dbWriteTable(lab, "extraction", extr, row.names = F, overwrite = T)
# # # 
# # # dbDisconnect(lab)
# # # rm(lab)

# 
# # add one column of quants into database
# platefile2 = "data/2017-11-07-plate07.txt"
# 
# strs <- readLines(platefile2, skipNul = T)
# linestoskip = (which(strs == "Group: Unknowns")) # the number of lines to skip
# 
# dat2 <- read.table(text = strs,  skip = linestoskip, sep = "\t", fill = T, header = T, stringsAsFactors = F)
# 
# # remove footer rows
# dat2 <- dat2[1:(which(dat2$Sample == "Group Summaries")-1), ]
# dat2 <- select(dat2, Wells, AdjConc) %>% 
#   rename(quant = AdjConc) %>% 
# #   rename(well = Wells)
# 
# # pull first column samples from database
# change <- dbReadTable(lab, "extraction") %>%
#   filter(grepl("E3819", plate), grepl("6", well)) %>% 
#   mutate(well = str_replace(well, "6", "8")) %>% 
#   select(-quant)
# 
# # read in names for the samples
# quant2 <- left_join(dat2, change, by = "well")
# quant2 <- quant2 %>%
#   ### THIS DEPENDS ON THE TYPE OF SAMPLE YOU ARE LOOKING AT extraction_id, digest_id, etc) ###
#   filter(!is.na(extraction_id)) %>% 
#   select(extraction_id, quant)

# table = "extraction"
# # import into database
# lab <- write_db("Laboratory")
# dat <- dbReadTable(lab, table)
# 
# # select the samples that will be changed
# change <- dat %>% 
#   filter(extraction_id %in% quant2$extraction_id) %>% 
#   select(-quant)
# 
# change <- left_join(change, quant2, by = "extraction_id")
# 
# dat <- change_rows(dat, change, "extraction_id")

# write the group back to the database
# dbWriteTable(lab, table, dat, row.names = F, overwrite = T)
