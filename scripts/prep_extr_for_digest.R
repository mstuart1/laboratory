# prep samples for digest - this script will examine the extraction table, find candidates for future digests, and place them in a digest plate plan

# connect to the database
library(dplyr)
source("scripts/lab_helpers.R")

lab <- write_db("Laboratory")

# get a list of digested samples
dig <- dbReadTable(lab, "digest")

# get list of extracted samples that are digestable
extr <- dbReadTable(lab, "extraction") %>% 
  filter(extraction_id > "E0036") %>%  # must be part of the current project
  filter(quant > 5) %>%  # must have at least 5ng/ul concentration
  filter(grepl("APCL", sample_id)) # must be part of the current project

# which extracts are not digested
todo <- anti_join(extr, dig, by = "extraction_id") %>% 
  arrange(extraction_id) %>% 
  select(extraction_id, quant)
rm(dig,extr)

# how many plates would these make, 94 samples plus 2 blanks per plate
(nplates <- floor(nrow(todo)/94)) # extra parenthesis are to print

# define wells
well <- 1:(96*nplates)

# separate list of samples out into plates

# insert the negative controls
a <- (nrow(todo)+1)
todo[a, ] <- "XXXX"

dig_plate <- data.frame() # blank data frame to build upon
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
  names(str1) <- c("well", "extraction_id")
  str2 <- as.data.frame(cbind(well[e], todo[a,])) # because the first blank is in the 12th position
  names(str2) <- c("well", "extraction_id")
  str3 <- as.data.frame(cbind(well[f:g], todo[e:l,])) #13:60 in plate, 12:59 in list
  names(str3) <- c("well", "extraction_id")
  str4 <- as.data.frame(cbind(well[h], todo[a,])) # because the 2nd blank is in the 61st position
  names(str4) <- c("well", "extraction_id")
  str5 <- as.data.frame(cbind(well[j:k], todo[g:m,]))# 62:96 in plate, 60:94 in list
  names(str5) <- c("well", "extraction_id")
  
  # and stick all of the rows together
  temp <- data.frame(rbind(str1, str2, str3, str4, str5))
  temp$Row <- rep(LETTERS[1:8], 12)
  temp$Col <- unlist(lapply(1:12, rep, 8))
  temp$plate <- paste("plate", i, sep = "")
  dig_plate <- rbind(dig_plate, temp)
  
}

names(dig_plate) <- c("well", "extraction_id", "quant", "Row", "Col", "plate")
rm(temp, str1, str2, str3, str4, str5, a, c, d, e, f, g, h, i, j, k, l, m)

# put the samples in order of extraction (with negative controls inserted)
dig_plate <- arrange(dig_plate, Col, Row)
dig_plate$extraction_id <- as.character(dig_plate$extraction_id)

#### make a plate map of extraction IDs (for knowing where to place extractions) ####

# make a list of all of the plates
platelist <- distinct(dig_plate, plate)
for (i in 1:nrow(platelist)){
  plate <- dig_plate %>% 
    filter(plate == platelist[i,]) %>% 
    select(Row, Col, extraction_id)
  
  platemap <- as.matrix(reshape2::acast(plate, plate[,1] ~ plate[,2]), value.var = plate[,3])
  # write.csv(platemap, file = paste("./maps/",Sys.Date(), "digest_map", i, ".csv", sep = ""))
}

### ONLY DO THIS ONCE ### generate extract numbers for database ####
# get the last number used for digest and add digest_id
digested <- dbReadTable(lab, "digest")
dbDisconnect(lab)
rm(lab)

# digested <- digested %>%  # not sure why we would exclude digest ids used for blanks
  # filter(sample_id != "XXXX")

dig_plate <- dig_plate %>% # arrange plate by columns then rows
  arrange(Col, Row) 

# dig_plate$well <- as.numeric(dig_plate$well)

x <- as.numeric(max(substr(digested$digest_id, 2,5))) # find the highest digest_id number in the db

# dig_plate <- dig_plate %>% 
#   mutate(digest_id2 = x + well) # can't do this because it won't put the well numbers in the correct order, puts 10 before 2.

for (i in 1:nrow(dig_plate)){ # for every row in the dig_plate table
  y <- x + well[i] # add the well number to the max digest_id number
  dig_plate$digest_id[i] <- paste("D", y, sep = "") # assign that to the sample as the digest id
}

# combine Row and Col into plate well
dig_plate$well <- paste(dig_plate$Row, dig_plate$Col, sep = "")

# make a note that these are planned extracts that haven't happened yet
dig_plate$notes <- "digests planned for October 2017 by MRS"

# select columns for db
dig_plate <- dig_plate %>% 
  mutate(date = NA) %>% 
  mutate(vol_in = "30") %>% # the volume used in this project
  mutate(ng_in = NA) %>% 
  mutate(enzymes = "PstI_MluCI") %>% # the enzymes used in this project 
  mutate(final_vol = NA) %>% 
  mutate(quant = NA) %>% 
  mutate(DNA_ng = NA) %>% 
  mutate(correction = NA) %>%
  mutate(corr_message = NA) %>% 
  mutate(corr_editor = NA) %>% 
  mutate(corr_date = NA) %>% 
  select(digest_id, extraction_id, date, vol_in, ng_in, enzymes, final_vol, quant, DNA_ng, well, plate, notes, correction, corr_message, corr_editor, corr_date)

# change plate name to match range
for (i in 1:nplates){
  x <- paste("plate", i, sep = "")
  blip <- dig_plate %>% 
    filter(plate == x)
  if (nrow(blip) > 0){
    dig_plate <- anti_join(dig_plate, blip) # remove these rows from dig_plate
    a <- blip %>% filter(well == "A1") %>% select(digest_id)
    b <- blip %>% filter(well == "H12") %>% select(digest_id)
    blip$plate <- paste(a, "-", b, sep = "")
    dig_plate <- rbind(dig_plate, blip) # add rows back in to extr
  }
}

### import the digest list into the database ####
############# BE CAREFUL #################################
# lab <- writedb("Laboratory")
# 
# dbWriteTable(lab, "digest", dig_plate, row.names = F, overwrite = F, append = T)
# 
# dbDisconnect(lab)
# rm(lab)


