# prep samples for plate reader

source("scripts/lab_helpers.R")

# connect to database - read only
lab <- read_db("Laboratory")

# which type of plate are you reading, extraction or digest?
x <- "extraction"

# which plates need to be read?
todo <- lab %>% 
  tbl(x) %>% 
  # filter(is.na(quant)) %>% # if you want any unquantified plates
  filter(plate == "E3161-E3254" | plate == "E3255-E3348" | plate == "E3349-E3442" | plate == "E3443-E3536") %>%  # if you want to specify plates
  filter(!is.na(plate)) %>% # eliminate any plates that haven't been extracted yet
  collect() %>% 
  arrange(plate, well) %>% 
  select(1:2, well, plate) # the first 2 columns of any table are the id columns
  

# all of the first columns are going to be replaced with standards, grab samples to be moved
firsts <- todo %>% 
  filter(grepl("1", well) & !grepl("11", well) & !grepl("10", well) & !grepl("12", well))

# after the first have been moved to a new table, replace them on the todo table with standards
stds <- firsts %>% 
  mutate(extraction_id = "STD", 
    digest_id = "STD",
    sample_id = "STD") %>% 
  select(-5) # remove the last column, which doesn't match todo

# remove those unchanged rows from todo
todo <- anti_join(todo, stds, by = c("well", "plate"))
# add in the changed rows
todo <- rbind(todo, stds)
  

# make a list of plate names
plates <- todo %>% 
  select(plate) %>% 
  distinct() %>% 
  filter(!is.na(plate)) %>% 
  arrange(plate)

# create plates from this db info

for (i in 1:nrow(plates)){ # can't have more than 11 columns of samples on a firsts plate
  # filter down to one plate
  x <- plates$plate[i]
  current <- todo %>% 
    filter(plate == x)
  # prep a map
  plate <- plate_from_db(current, "extraction_id")
  write.csv(platemap, file = paste("./maps/",Sys.Date(), x, ".csv", sep = ""))
  
  # update the location for the firsts
  temp <- firsts %>% 
    filter(plate == x) %>% 
    mutate(
      col = as.numeric(substr(well, 2, 2)) + i, 
      row = substr(well, 1, 1))
  
  # remove the changed rows from firsts
  firsts <- anti_join(firsts, temp, by = c("well", "plate"))
  
  # change the rows
  temp <- temp %>% 
    mutate(well = paste(row, col, sep = "")) %>% 
    select(-row, -col)
  
  # rejoin the rows
  firsts <- rbind(firsts, temp)
  
}

# make a platemap for the firsts

  # isolate the first column
  # temp <- firsts %>% # there is nothing in the 1 column currently
  #   filter(grepl("1", well) & !grepl("11", well) & !grepl("10", well) & !grepl("12", well))
  # firsts <- anti_join(firsts, temp) # remove the column from the firsts table 

# when temp is empty from the code above:
id_1 <- rep("STD", 8)
id_2 <- rep("STD", 8)
row <- rep(LETTERS[1:8])
col <- rep("1", 8)
plate <- rep("firsts", 8)
temp <- data.frame(id_1, id_2, row, col, plate)
temp$well <- paste(row, col, sep = "")
temp$row <- NULL
temp$col <- NULL
temp <- select(temp, id_1, id_2, well, plate)
names(temp) <- names(firsts)


  # replace with standards
  stds <- temp %>% 
    mutate(extraction_id = "STD", 
    digest_id = "STD",
    sample_id = "STD") %>% 
    mutate(plate = "firsts") %>% 
    select(-5) %>%  # remove the last column, which doesn't match todo
    distinct()
  
  # join back to firsts
  firsts <- rbind(firsts, stds)

  # make the plate map
  plate <- plate_from_db(firsts, "extraction_id") 
  write.csv(plate, file = paste("./maps/", Sys.Date(), "_firsts_list.csv", sep = "")) # save this for locating samples when reading in plate data
  
  write.csv(platemap, file = paste("./maps/",Sys.Date(), "_firsts.csv", sep = ""))

