# create the bat database

# in sequel pro, I created a database called "Bats" which I will connect to in R to add the data that SGW provided in an excel spreadsheet.

source("scripts/lab_helpers.R")

dat <- read.csv("data/FinalPoolsConcIndices_AllBats.csv") %>% 
  select(-X, -X.1, -X.2, -X.3)

bat <- write_db("Bats")

# dbWriteTable(bat, "bats", dat, append = T)
# dbDisconnect(bat)
# rm(bat)
										
