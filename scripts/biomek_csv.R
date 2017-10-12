# A script to create a csv for the Biomek from a list of ligation numbers
source("scripts/lab_helpers.R")

# connect to db
lab <- read_db("Laboratory")

# the biomek table has 8 locations available
mek_loc <- c("P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12")

# find the planned ligations by date or notes or plate
# get digest quants numbers for ligations
ligs <- lab %>% 
  tbl("ligation") %>% 
  filter(date == "2016-04-28") %>% 
  select(ligation_id, digest_id, DNA, vol_in, water, well, plate) %>% 
  collect()

digs <- lab %>% 
  tbl("digest") %>% 
  filter(digest_id %in% ligs$digest_id) %>% 
  select(digest_id, quant, well, plate) %>% 
  collect() %>% 
  rename(dig_well = well, dig_plate = plate)

ligs <- left_join(ligs, digs[, c("digest_id", "well", "plate")], by = "digest_id") %>% 
  arrange(ligation_id)

# how many ligation plates are there?
(lig_plates <- ligs %>% 
    distinct(plate))
x <- nrow(lig_plates)
  

# dest$destloc <- "P11" # are you sure?
# dest <- dest[ , c(2,3,4,1,6,5)] # rearrange columns - why?

# add source locations

# digests from the ligations we want
source <- lab %>% 
  tbl("digest") %>% 
  filter(digest_id %in% ligs$digest_id) %>% 
  select(digest_id, well, plate) %>% 
  collect()

# how many digest plates are there?
(dig_plates <- source %>% 
    distinct(plate))
y = nrow(dig_plates)

ligs$dest <- "NA"
if (x + y <= 8){ # if we need 8 or fewer plates
  for (i in 1:nrow(lig_plates)){
      change <- ligs %>% 
        filter(plate == lig_plates$plate[i]) %>% 
        mutate(dest = mek_loc[length(mek_loc)])
      mek_loc <- mek_loc[1:length(mek_loc)-1]
      ligs <- change_rows(ligs, change, "ligation_id")
  }
  for (i in 1:nrow(dig_plates)){
    change <- digs %>% 
      filter(plate == dig_plates$plate[i]) %>% 
      mutate(source = mek_loc[length(mek_loc)])
    mek_loc <- mek_loc[1:length(mek_loc)-1]
    digs <- change_rows(digs, change, "digest_id")
  }
  }
  








# add source positions to each of the source plates
source1$sourceloc <- "P9"
source2$sourceloc <- "P10"
source3$sourceloc <- "P5"

# merge the source files together
ultimate <- rbind(source1, source2, source3)
ultimate$dnavol <- round(ultimate$dnavol, 2)
ultimate$watervol <- round(ultimate$watervol, 2)


# pull out the water information
water <- ultimate
water$wellsource <- "A1"
water$sourceloc <- "P12"

write.csv(ultimate, file = paste(Sys.Date(), "biomek.csv", sep = ""))

write.csv(water, file = paste(Sys.Date(), "water.csv", sep = ""))

penultimate <- rbind(ultimate, water)
penultimate <- penultimate[order(penultimate$digest_ID), ]

write.csv(penultimate, file = paste(Sys.Date(), "combo.csv", sep = ""))
