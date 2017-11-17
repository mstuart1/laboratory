# prep samples for ligation 

# connect to the database
library(dplyr) 
source("scripts/lab_helpers.R")

lab <- write_db("Laboratory")

# get a list of ligated samples
lig <- dbReadTable(lab, "ligation") %>% 
  select(ligation_id, digest_id)

# get list of digested samples that have not been ligated
dig <- dbReadTable(lab, "digest") %>% 
  # filter(!digest_id %in% lig$digest_id) %>%  # not ligated yet
  filter(plate == "D3724-D3819") %>%  # want a specific plate
  filter(extraction_id != "XXXX") %>% # remove blanks
  # filter(quant >= 0.5) %>% # strong enough concentration to be ligated 
  select(digest_id, quant, well, plate) %>% # need the id, concentration, and where it is
  arrange(digest_id)

# TRYING A NEW STRATEGY TO LIGATE AT 10NG FIRST AND THEN UP THE QUANTITY ####
DNA <- 10
ten <- lig_ng(dig, DNA)


if (nrow(dig >=47)){
  fifty <- dig %>%
    mutate(uL_in = round(50/quant, 1)) %>% # round to 1 decimal point
    filter(uL_in < 22.2 & uL_in > 0.5) %>%
    mutate(water = round(22.2-uL_in, 1)) %>%
    mutate(DNA = 50)

  if (nrow(fifty)/47 >= 1){ # if there are more than 47
    x <- nrow(fifty) %% 47 # get the remainder after dividing by 47
    fifty <- fifty %>%
      arrange(desc(uL_in)) %>% # keep the largest pipet volumes
      slice(1:(nrow(fifty) - x))
    dig <- anti_join(dig, fifty, by = "digest_id")
  }else {
    rm(fifty)
  }
}
if (nrow(dig >=47)){
  s75 <- dig %>%
      mutate(uL_in = round(75/quant, 1)) %>% # round to 1 decimal point
      filter(uL_in < 22.2 & uL_in > 0.5) %>%
      mutate(water = round(22.2-uL_in, 1)) %>%
      mutate(DNA = 75)
  
  if (nrow(s75)/47 >= 1){ # if there are more than 47
      x <- nrow(s75) %% 47 # get the remainder after dividing by 47
      s75 <- s75 %>%
        arrange(desc(uL_in)) %>% # keep the largest pipet volumes
        slice(1:(nrow(s75) - x))
      dig <- anti_join(dig, s75, by = "digest_id")
    }else {
      rm(s75)
    }
}

if (nrow(dig >=47)){
  one_hundred <- dig %>%
    mutate(uL_in = round(100/quant, 1)) %>% # round to 1 decimal point
    filter(uL_in < 22.2 & uL_in > 0.5) %>%
    mutate(water = round(22.2-uL_in, 1)) %>%
    mutate(DNA = 100)

  if (nrow(one_hundred)/47 >= 1){ # make sure there are at least 47
    x <- nrow(one_hundred) %% 47 # get the remainder after dividing by 47
    one_hundred <- one_hundred %>%
      arrange(desc(uL_in)) %>% # keep the largest pipet volumes
      slice(1:(nrow(one_hundred) - x))
    dig <- anti_join(dig, one_hundred, by = "digest_id")
  }else {
    rm(one_hundred)
  }
}
if (nrow(dig >=47)){
  one_fifty <- dig %>%
    mutate(uL_in = round(150/quant, 1)) %>% # round to 1 decimal point
    filter(uL_in < 22.2 & uL_in > 0.5) %>%
    mutate(water = round(22.2-uL_in, 1)) %>%
    mutate(DNA = 150)

  if (nrow(one_fifty)/47 >= 1){ # make sure there are at least 47
    x <- nrow(one_fifty) %% 47 # get the remainder after dividing by 47
    one_fifty <- one_fifty %>%
      arrange(desc(uL_in)) %>% # keep the largest pipet volumes
      slice(1:(nrow(one_fifty) - x))
    dig <- anti_join(dig, one_fifty, by = "digest_id")
  }else {
    rm(one_fifty)
  }
}  
if (nrow(dig >=47)){
  two_hundred <- dig %>%
    mutate(uL_in = round(200/quant, 1)) %>% # round to 1 decimal point
    filter(uL_in < 22.2 & uL_in > 0.5) %>%
    mutate(water = round(22.2-uL_in, 1)) %>%
    mutate(DNA = 200)
  
  if (nrow(two_hundred)/47 >= 1){ # make sure there are at least 47
    x <- nrow(two_hundred) %% 47 # get the remainder after dividing by 47
    two_hundred <- two_hundred %>%
      arrange(desc(uL_in)) %>% # keep the largest pipet volumes
      slice(1:(nrow(two_hundred) - x))
    dig <- anti_join(dig, two_hundred, by = "digest_id")
  }else {
    rm(two_hundred)
  }
}

# which digests are left over?











# how many plates would these make, 94 samples plus 2 blanks per plate
(nplates <- floor(nrow(dig)/96)) # extra parenthesis are to print

# define wells
well <- 1:(96*nplates)

# how many samples are not included in this plan?
(not <- nrow(extr) - (96*nplates))

# separate list of samples out into plates

# insert the negative controls and set up the plate
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
  str1 <- as.data.frame(cbind(well[c:d], extr[c:d,])) # 1:11
  names(str1) <- c("well", "extraction_id")
  str2 <- as.data.frame(cbind(well[e], "XXXX")) # because the first blank is in the 12th position
  names(str2) <- c("well", "extraction_id")
  str3 <- as.data.frame(cbind(well[f:g], extr[e:l,])) #13:60 in plate, 12:59 in list
  names(str3) <- c("well", "extraction_id")
  str4 <- as.data.frame(cbind(well[h], "XXXX")) # because the 2nd blank is in the 61st position
  names(str4) <- c("well", "extraction_id")
  str5 <- as.data.frame(cbind(well[j:k], extr[g:m,]))# 62:96 in plate, 60:94 in list
  names(str5) <- c("well", "extraction_id")
  
  # and stick all of the rows together
  temp <- data.frame(rbind(str1, str2, str3, str4, str5))
  temp$Row <- rep(LETTERS[1:8], 12)
  temp$Col <- unlist(lapply(1:12, rep, 8))
  temp$plate <- paste("plate", i, sep = "")
  dig_plate <- rbind(dig_plate, temp)
  
}

names(dig_plate) <- c("well", "extraction_id", "Row", "Col", "plate")
rm(temp, str1, str2, str3, str4, str5, a, c, d, e, f, g, h, i, j, k, l, m)

# put the samples in order of extraction (with negative controls inserted)
dig_plate <- arrange(dig_plate, plate, Col, Row)
dig_plate$extraction_id <- as.character(dig_plate$extraction_id)

#### make a plate map of extraction IDs (for knowing where to place extractions) ####

# make a list of all of the plate names
platelist <- distinct(dig_plate, plate)
for (i in 1:nrow(platelist)){
  plate <- dig_plate %>% 
    filter(plate == platelist[i,]) %>% 
    select(Row, Col, extraction_id)
  
  platemap <- as.matrix(reshape2::acast(plate, plate[,1] ~ plate[,2]), value.var = plate[,3])
  # write.csv(platemap, file = paste("./output/",Sys.Date(), "digest_map", i, ".csv", sep = ""))
}

### ONLY DO THIS ONCE ### generate digest numbers for database ####
# get the last number used for digest and add digest_id
digested <- dbReadTable(lab, "digest") %>% 
  summarize(
    x = max(digest_id)
  )
dbDisconnect(lab)
rm(lab)

dig_plate <- dig_plate %>% # arrange plate by columns then rows
  arrange(plate, Col, Row) 

# dig_plate <- dig_plate %>% 
#   mutate(digest_id2 = x + well) # can't do this because it won't put the well numbers in the correct order, puts 10 before 2.

for (i in 1:nrow(dig_plate)){ # for every row in the dig_plate table
  y <- as.numeric(substr(digested[1,1], 2, 5)) + well[i] # add the well number to the max digest_id number
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
  name <- dig_plate %>% 
    filter(plate == x)
  if (nrow(name) > 0){
    dig_plate <- anti_join(dig_plate, name, by = "extraction_id") # remove these rows from dig_plate
    a <- name %>% filter(well == "A1") %>% select(digest_id) # get the first digest
    b <- name %>% filter(well == "H12") %>% select(digest_id) # get the last digest
    name$plate <- paste(a, "-", b, sep = "")
    dig_plate <- rbind(dig_plate, name) # add rows back in to extr
  }
}

### import the digest list into the database ####
############# BE CAREFUL #################################
# lab <- write_db("Laboratory")
# 
# dbWriteTable(lab, "digest", dig_plate, row.names = F, overwrite = F, append = T)
# 
# dbDisconnect(lab)
# rm(lab)


