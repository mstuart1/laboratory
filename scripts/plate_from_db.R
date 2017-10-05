# a script to make platemaps from database location
source("scripts/lab_helpers.R")

# import samples from db
lab <- read_db("Laboratory")

# choose a plate to create a plate map for
plate <- lab %>% 
  # THIS IS WHERE YOU MUST ENTER THE SPECIFICS OF WHAT YOU WANT - extractions, range, etc?
  tbl("extraction") %>% 
  select(sample_id, extraction_id, well, plate) %>% 
  filter(plate == "E4195-E4288") %>% 
  collect()

# split the well out into row and column again
plate$row <- substr(plate$well, 1, 1)
plate$col <- as.numeric(substr(plate$well, 2, 3))

# select columns for plate ( ## YOU MUST CHOOSE HERE IF YOU WANT SAMPLE_ID OR SOME OTHER IDENTIFIER ##)
plate <- plate %>% select(row, col, sample_id) #keep row & col, choose identifier (here is sample_id)

plate <- plate %>% arrange(row, col)
plate <- as.data.frame(plate)

# make map
platemap <- as.matrix(reshape2::acast(plate,plate[,1] ~ plate[,2]))
# write.csv(platemap, file = paste("./maps/",Sys.Date(), "map_from_db.csv", sep = ""))
