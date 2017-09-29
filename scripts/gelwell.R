# gelwell
#' based on plate locations and which pipet used, will identify the wells of a gel
library(stringr)
source("scripts/lab_helpers.R")

# 1. Define pipet ####
pipet <- 12
# pipet <- 8
# pipet <- 1

# 2. Define plate ####
range <- "E3064-E3159"

# 3. Connect to db
lab <- read_db("Laboratory")

# 4. retrieve extraction locations
extr <- lab %>% 
  tbl("extraction") %>% 
  filter(plate == range) %>% 
  select(extraction_id, well) %>% 
  mutate(row = substr(well, 1, 1)) %>% 
  mutate(column = substr(well, 2, 3)) %>% 
  select(-well) %>% 
  collect()

# 5. assign extractions to gel wells based on pipet used
if (pipet == "12"){
  set1 <- extr %>%
    filter(row == "A" | row == "B") %>% # choose the first 2 rows
    filter(column != 10 & column != 11 & column != 12) %>% # remove the 10, 11, 12 that get misplaced
    arrange(column, row)
  set1b <- extr %>%
    filter(row == "A" | row == "B") %>% # choose the first 2 rows
    filter(column == 10 | column == 11 | column == 12) %>% # the 10, 11, 12 that get misplaced
    arrange(column, row)
  set1 <- rbind(set1, set1b)
  (row1a <- set1$extraction_id)
  
  set2 <- extr %>%
    filter(row == "C" | row == "D") %>% # choose the first 2 rows
    filter(column != 10 & column != 11 & column != 12) %>% # remove the 10, 11, 12 that get misplaced
    arrange(column, row)
  set2b <- extr %>%
    filter(row == "C" | row == "D") %>% # choose the first 2 rows
    filter(column == 10 | column == 11 | column == 12) %>% # the 10, 11, 12 that get misplaced
    arrange(column, row)
  set2 <- rbind(set2, set2b)
  (row1b <- set2$extraction_id)
  
  set3 <- extr %>%
    filter(row == "E" | row == "F") %>% # choose the first 2 rows
    filter(column != 10 & column != 11 & column != 12) %>% # remove the 10, 11, 12 that get misplaced
    arrange(column, row)
  set3b <- extr %>%
    filter(row == "E" | row == "F") %>% # choose the first 2 rows
    filter(column == 10 | column == 11 | column == 12) %>% # the 10, 11, 12 that get misplaced
    arrange(column, row)
  set3 <- rbind(set3, set3b)
  (row2a <- set3$extraction_id)
  
  set4 <- extr %>%
    filter(row == "G" | row == "H") %>% # choose the first 2 rows
    filter(column != 10 & column != 11 & column != 12) %>% # remove the 10, 11, 12 that get misplaced
    arrange(column, row)
  set4b <- extr %>%
    filter(row == "G" | row == "H") %>% # choose the first 2 rows
    filter(column == 10 | column == 11 | column == 12) %>% # the 10, 11, 12 that get misplaced
    arrange(column, row)
  set4 <- rbind(set4, set4b)
  (row2b <- set4$extraction_id)
  
}
row1a
row1b
row2a
row2b
