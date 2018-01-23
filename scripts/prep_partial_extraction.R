# This is a script for adding samples that are not in the laboratory database and have not been extracted yet
# library(RMySQL)
# library(dplyr)
library(gtools)
source("scripts/lab_helpers.R")

# load from an existing list
load("data/need_extract.Rdata")
work <- not_extr
# remove extra columns
work <- select(work, sample_id) 


# separate out the work into 2 plates 
y <- nrow(work)
x <- floor(y/2)
work1 <- work %>% 
  slice(1:x)
if(odd(y)){
  work2 <- work %>% 
    slice((x+1):(y-1))
}else{
  work2 <- work %>% 
    slice(x+1:y)
}


# make a blank full plate
blank <- data_frame(row = rep(LETTERS[1:8], 12), col = unlist(lapply(1:12, rep, 8)))
# cut blank down to the number of rows for each work table
blank <- blank %>% 
  arrange(row) %>%  
  slice(1:x)

# attach wells to samples
work1 <- cbind(work1, blank)
work1 <- select(work1, row, col, sample_id)
work2 <- cbind(work2, blank)
work2 <- select(work2, row, col, sample_id)

# make a source map
platemap1 <<- as.matrix(reshape2::acast(work1, work1$row ~ work1$col))
platemap2 <<- as.matrix(reshape2::acast(work2, work2$row ~ work2$col))

work1 <- mutate(work1, plate = 1)
work2 <- mutate(work2, plate = 2)


# save to evernote
# write.csv(platemap1, file="data/platemap1.csv")
# write.csv(platemap2, file="data/platemap2.csv")


### ONLY DO THIS ONCE ### generate extract numbers for database ####
lab <- read_db("Laboratory")

# pull in all of the extract data
extracted <- lab %>% tbl("extraction") %>% collect()

# collect the samples to be extracted into a table and arrange by sample_id
extr <- rbind(work1, work2) 
extr <- extr%>% 
  arrange(sample_id) %>% 
  mutate(well = seq(nrow(extr))) # add a row identifier for generating an extr_id

# get the max extraction id used so far
x <- as.numeric(max(substr(extracted$extraction_id, 2,5)))

# generate an extr_id and well location and make a note of plan
extr <- extr %>% 
  mutate(extraction_id = paste("E", well + x, sep = ""), 
    well = paste(row, col, sep = ""), 
    notes = "extracts planned for January 2018 by MRS")

# select columns for db
extr <- extr %>% 
  mutate(date = NA) %>% 
  mutate(method = "DNeasy96") %>% 
  mutate(final_vol = "200") %>% 
  mutate(quant = NA) %>% 
  mutate(gel = NA) %>% 
  mutate(correction = NA) %>%
  mutate(corr_message = NA) %>% 
  mutate(corr_editor = NA) %>% 
  mutate(corr_date = NA) %>% 
  select(extraction_id, sample_id, date, method, final_vol, quant, gel, well, plate, notes, correction, corr_message, corr_editor, corr_date)

# determine plate ranges
mins <- extr %>% 
  filter(well == "A1") %>% 
  arrange(plate)
#### This will change depending on plate length !!!! ####
maxs <- extr %>% 
  filter(well == "F12") %>% 
  arrange(plate)

# name plate based on extraction range
extr <- extr %>% 
  mutate(plate = ifelse(
    plate == 1, 
    paste(mins$extraction_id[1], "-", maxs$extraction_id[1], sep = ""), 
    paste(mins$extraction_id[2], "-", maxs$extraction_id[2], sep = "")))
  
# ### import the extract_list into the database ####
# lab <- write_db("Laboratory")
# 
# dbWriteTable(lab, "extraction", extr, row.names = F, overwrite = F, append = T)
# 
# dbDisconnect(lab)
# rm(lab)
