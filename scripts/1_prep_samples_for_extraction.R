# This is a script for adding samples that are not in the laboratory database and have not been extracted yet
library(RMySQL)
library(dplyr)


#### obtain a list of all clownfish sample ids from the Leyte database ####

# connect to leyte fieldwork db
leyte <- dbConnect(MySQL(), dbname = "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# import fish table
# select down to only sample_id numbers and remove any rows without a sample
fish <- dbReadTable(leyte, "clownfish") %>% 
  select(sample_id) %>% 
  filter(!is.na(sample_id))

#### compare that list to samples that have already been extracted ####

# make sure each sample_id is only represented once
fish <- distinct(fish)

#connect to laboratory db
lab <- dbConnect(MySQL(), "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# import extraction table and reduce to only the sample_id column
extr <- dbReadTable(lab, "extraction") %>% 
  select(sample_id)
dbDisconnect(lab)
rm(lab)
# select any samples in "fish" that have been extracted in "extr" (don't know how to do "not in")
done <- fish %>% 
  filter(sample_id %in% extr$sample_id)

todo <- anti_join(fish, done)

# are there any reasons why these haven't been done?  Check the notes - there are no notes about why these haven't been done in the leyte table and they do not appear in the laboratory table, so it is a mystery for any samples from earlier years.  Oh!  Check the old db.
old <- dbReadTable(leyte, "clownfish_old") %>% 
  select(sample_id, Notes) %>% 
  filter(!is.na(sample_id))
todo <- left_join(todo, old, by = "sample_id")
dbDisconnect(leyte)
rm(leyte)


# remove samples that cannot be extracted
errors <- c("APCL12_271", "APCL13_024", "APCL13_547", "APCL13_549", "APCL13_551", "APCL13_553", "APCL14_030", "APCL14_157", "APCL14_161", "APCL14_164", "APCL14_301", "APCL14_304", "APCL14_305", "APCL14_306", "APCL14_492", "APCL14_494", "APCL15_355550", "APCL15_404305", "APCL15_405807")
rem <- todo %>% 
  filter(sample_id %in% errors)
todo <- anti_join(todo, rem)

todo <- select(todo, sample_id)


# how many plates would these make, 94 samples plus 2 blanks per plate
(nplates <- floor(nrow(todo)/94)) # extra parenthesis are to print

# define wells
well <- 1:(94*nplates)

# separate list of samples out into plates

# insert the negative controls
a <- (nrow(todo)+1)
todo[a, ] <- "XXXX"

extr <- data.frame()
for (i in 1:nplates){
  c <- 94*i-93 # well 1 on a plate
  d <- 94*i-83 # 11
  e <- 94*i-82 # 12 negative control well
  f <- 94*i-81 # 13
  g <- 94*i-34 # 60
  h <- 94*i-33 # 61 negative control well
  j <- 94*i-32 # 62
  k <- 94*i + 2 # 96
  l <- 94*i - 35 # 59
  m <- 94 * i #94
  str1 <- as.data.frame(cbind(well[c:d], todo[c:d,])) # 1:11
  names(str1) <- c("well", "sample_id")
  str2 <- as.data.frame(cbind(well[e], todo[a,])) # because the first blank is in the 12th position
  names(str2) <- c("well", "sample_id")
  str3 <- as.data.frame(cbind(well[f:g], todo[e:l,])) #13:60 in plate, 12:59 in list
  names(str3) <- c("well", "sample_id")
  str4 <- as.data.frame(cbind(well[h], todo[a,])) # because the 2nd blank is in the 61st position
  names(str4) <- c("well", "sample_id")
  str5 <- as.data.frame(cbind(well[j:k], todo[g:m,]))# 62:96 in plate, 60:94 in list
  names(str5) <- c("well", "sample_id")
  
  # and stick all of the rows together
  temp <- data.frame(rbind(str1, str2, str3, str4, str5))
  temp$Row <- rep(LETTERS[1:8], 12)
  temp$Col <- unlist(lapply(1:12, rep, 8))
  temp$plate <- paste("plate", i, sep = "")
  extr <- rbind(extr, temp)
  
}

# put the samples in order of extraction (with negative controls inserted)
extr <- arrange(extr, well)
extr$sample_id <- as.character(extr$sample_id)

#### make a plate map of sample IDs (for knowing where to place fin clips) ####

# make a list of all of the plates
platelist <- distinct(extr, plate)
for (i in 1:nrow(platelist)){
  plate <- extr %>% 
    filter(plate == platelist[i,]) %>% 
    select(Row, Col, sample_id)
  
  platemap <- as.matrix(reshape2::acast(plate, plate[,1] ~ plate[,2]), value.var = plate[,3])
  write.csv(platemap, file = paste(Sys.Date(), "extract_map", i, ".csv", sep = ""))
}

### ONLY DO THIS ONCE ### generate extract numbers for database ####
lab <- dbConnect(MySQL(), "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# get the last number used for extract and add extraction_id
extracted <- dbReadTable(lab, "extraction")
dbDisconnect(lab)
rm(lab)

x <- as.numeric(max(substr(extracted$extraction_id, 2,5))) + 1

extr$extraction_id <- paste("E", (as.numeric(extr$well) + x), sep = "")

# combine Row and Col into plate well
extr$well <- paste(extr$Row, extr$Col, sep = "")

# make a note that these are planned extracts that haven't happened yet
extr$notes <- "extracts planned for August 2017 by MRS"

# select columns for db
extr <- extr %>% 
  mutate(date = NA) %>% 
  mutate(method = "DNeasy96") %>% 
  mutate(final_vol = "200") %>% 
  mutate(quant = NA) %>% 
  mutate(DNA_ug = NA) %>% 
  mutate(gel = NA) %>% 
  mutate(correction = NA) %>%
  mutate(corr_message = NA) %>% 
  mutate(corr_editor = NA) %>% 
  mutate(corr_date = NA) %>% 
  select(extraction_id, sample_id, date, method, final_vol, quant, DNA_ug, gel, well, plate, notes, correction, corr_message, corr_editor, corr_date)

# check to make sure there are no errors

  
### import the extract_list into the database ####
lab <- dbConnect(MySQL(), "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

dbWriteTable(lab, "extraction", extr, row.names = F, overwrite = F, append = T)

dbDisconnect(lab)
rm(lab)
