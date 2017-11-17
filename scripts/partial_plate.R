# make a partial plate from the database - pull plate locations from the database with no expectation of filling an entire plate.
source("scripts/lab_helpers.R")

# get samples and plate locations from db
lab <- read_db("Laboratory")

digs <- lab %>% 
  tbl("digest") %>% 
  # filter(date == "2017-11-09") %>% 
  arrange(digest_id) %>% 
  select(digest_id, well, plate, extraction_id) %>% 
  collect()

# make a list of plate names
plates <- digs %>% 
  distinct(plate)

# make a blank full plate
blank <- data_frame(row = rep(LETTERS[1:8], 12), col = unlist(lapply(1:12, rep, 8)))
blank$well <- paste(blank$row, blank$col, sep = "")

# separate out 1 plate from digs
current <- digs %>% 
  filter(plate == "D4204-D4299")

full <- left_join(blank, current, by = "well") %>% 
  select(row, col, digest_id)

# make a plate map
platemap <<- as.matrix(reshape2::acast(full, full$row ~ full$col))

# make a source map
source <- current %>% 
  select(well, extraction_id)

fulls <- left_join(blank, source, by = "well") %>% 
  select(row, col, extraction_id)

platemap <<- as.matrix(reshape2::acast(fulls, fulls$row ~ fulls$col))
  
