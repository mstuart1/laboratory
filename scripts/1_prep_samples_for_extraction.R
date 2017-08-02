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

#connect to laboratory db
lab <- dbConnect(MySQL(), "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# import extraction table and reduce to only the sample_id column
extr <- dbReadTable(lab, "extraction") %>% 
  select(sample_id)

# select any samples in "fish" that have been extracted in "extr" (don't know how to do "not in")
done <- fish %>% 
  filter(sample_id %in% extr$sample_id)

todo <- anti_join(fish, done)

# are there any reasons why these haven't been done?  Check the notes - there are no notes about why these haven't been done in the leyte table and they do not appear in the laboratory table, so it is a mystery for any samples from earlier years.  Oh!  Check the old db.
old <- dbReadTable(leyte, "clownfish_old") %>% 
  select(sample_id, Notes) %>% 
  filter(!is.na(sample_id))
todo <- left_join(todo, old, by = "sample_id")

# remove samples that cannot be extracted
errors <- c("APCL12_271", "APCL13_024", "APCL13_547", "APCL13_549", "APCL13_551", "APCL13_553", "APCL14_030", "APCL14_157", "APCL14_161", "APCL14_164", "APCL14_301", "APCL14_304", "APCL14_305", "APCL14_306", "APCL14_492", "APCL14_494", "APCL15_355550", "APCL15_404305", "APCL15_405807")
rem <- todo %>% 
  filter(sample_id %in% errors)
todo <- anti_join(todo, rem)


# how many plates would these make
(nplates <- floor(nrow(todo)/96)) # extra parenthesis are to print

# define wells
well <- 1:(96*nplates)

# separate list of samples out into plates

# start with just one plate


# insert the negative controls
sampID[95:96] <- "XXXX"

# create a list of well numbers
well <- 1:96

# put the samples in order of extraction (with negative controls inserted)
str1 <- cbind(well[1:11], sampID[1:11])
str2 <- cbind(well[12], sampID[95]) # because the first blank is in the 12th position
str3 <- cbind(well[13:60], sampID[12:59])
str4 <- cbind(well[61], sampID[96]) # because the 2nd blank is in the 61st position
str5 <- cbind(well[62:96], sampID[60:94])

# and stick all of the rows together
extr <- data.frame(rbind(str1, str2, str3, str4, str5))
colnames(extr) <- c("well", "sample_id")

# make a plate map of sample IDs (for knowing where to place fin clips)
plate <- data.frame( Row = rep(LETTERS[1:8], 12), Col = unlist(lapply(1:12, rep, 8)))
platelist <- cbind(plate, extr$sample_id)
names(platelist) <- c("Row", "Col", "ID")
# create a file to store the plate positions until after the extraction
write.csv(platelist, file = paste(Sys.Date(), "extract_list.csv", sep = ""))
platelist$ID <- as.character(platelist$ID)
platemap <- as.matrix(reshape2::acast(platelist,platelist[,1] ~ platelist[,2]), value.var = platelist$ID)
write.csv(platemap, file = paste(Sys.Date(), "extract_map.csv", sep = ""))
# print this platemap and use to place fin clips in wells

# clean up
rm(str1, str2, str3, str4, str5, well, sampid, span, species, year)

# add the date of extraction
extr$date <- as.Date("2016-09-06")

# add the method
extr$method <- "DNeasy 96"

# add the final volume after elution
extr$final_vol <- 200

names(extr) <- c("number", "sample_id", "date", "method", "final_vol")
# fix extraction numbers so the order of extraction numbers matches the order of sample ids
extr$number <- NULL
extr$number <- as.integer(1:96)



# add well and plate data to extr table
platelist$well <- paste(platelist$Row, platelist$Col, sep = "")
platelist <- platelist[ , c("well", "id")]
extr <- left_join(extr, platelist, by = c("sample_id" = "ID"), copy = T)


### ONLY DO THIS ONCE ### generate extract numbers for database

suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# get the last number used for extract
suppressWarnings(n <- data.frame(labor %>% tbl("extraction") %>% summarize(n())))
x <- n[1,]
extr$number <- paste("E", (extr$number + x), sep = "")
extr$plate <- paste(extr$number[1], "-", extr$number[nrow(extr)], sep = "")

# run the quantextr.R to 



# # edit the plate reader document so that it can be imported - delete header/footer, open in excel, check columns and save as csv
# # import plate reader results for quantificaiton
# pr <- read.csv("data/20160908_plate3.csv", stringsAsFactors = F, header = T)
# 
# # # add extract numbers (this plate contains the results for E2968-2975 plus others not on this current plate)
# # 
# # # remove rows for non-plate results
# # pr <- pr[1:8,]
# # # add extract numbers
# # pr$number <- 2968:2975
# # pr$number <- paste("E", pr$number, sep="")
# 
# pr2 <- read.csv("data/20160908_plate2.csv", stringsAsFactors = F)
# 
# pr2$number <- 3072:3159
# pr2$number <- paste("E", pr2$number, sep="")
# names(pr2) <- c("Sample", "Wells", "Value", "R", "Result", "MeanResult", "SD", "CV", "Dilution", "quant","number")
# 
# 
# extr1 <- merge(extr, pr, by.x = "number", by.y = "extraction_ID", all.x = T)
# extr1 <- extr1[1:8,]
# extr2 <- merge(extr, pr2[ , 10:11], by.x = "number", by.y = "number", all.x = T)
# extr2 <- extr2[9:96,]
# 
# extr <- rbind(extr1,extr2)

extr$dna <- extr$final_vol * extr$quant * 0.001

write.csv(extr, file = paste(Sys.Date(), "extract_list.csv", sep = ""))

# import the extract_list into the database

# make a plate map of extraction IDs (for a record of where extractions are stored)
plate <- data.frame( Row = rep(LETTERS[1:8], 12), Col = unlist(lapply(1:12, rep, 8)))
platelist <- cbind(plate, extr[,1])
names(platelist) <- c("Row", "Col", "ID")
first <- platelist$ID[1]
last <- platelist$ID[nrow(platelist)]
write.csv(platelist, file = paste("data/", first, "-", last, "list.csv", sep = ""))
platelist$ID <- as.character(platelist$ID)
platemap <- as.matrix(reshape2::acast(platelist,platelist[,1] ~ platelist[,2]))
write.csv(platemap, file = paste("data/", first, "-",last, "map.csv", sep = ""))




