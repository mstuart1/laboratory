# add plate name and well locations to extractions in the database that are currently NULL for those columns

#connect to laboratory db
lab <- dbConnect(MySQL(), "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# import extraction table and reduce to only the sample_id column
extr <- dbReadTable(lab, "extraction") 
by_samp <- extr %>% 
  filter(sample_id > "APCL16_094" & sample_id < "APCL16_189")
by_ids <- extr %>%
  filter(extraction_id >= min(by_samp$extraction_id) & extraction_id <= max(by_samp$extraction_id))

todo <- arrange(by_ids, extraction_id)
temp <- data.frame(todo)
temp$row <- rep(LETTERS[1:8], 12)
temp$col <- unlist(lapply(1:12, rep, 8))
todo <- left_join(todo, temp)

todo$well <- paste(todo$row, todo$col, sep = "")
todo$plate <- paste(min(todo$extraction_id), "-", max(todo$extraction_id), sep = "")

todo <- select(todo, -row, -col)

# pull in all extractions
extr <- lab %>% dbReadTable("extraction") %>% collect()

extr <- anti_join(extr, todo, by = "extraction_id")

extr <- rbind(extr, todo)

# write the changes to the db
################### BE CAREFUL ########################################
# dbWriteTable(lab, "extraction", extr, row.names = F, overwrite = T)
# 
# dbDisconnect(lab)
# rm(lab)
