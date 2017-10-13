# A script to create a csv for the Biomek from a list of ligation numbers
source("scripts/lab_helpers.R")

# connect to db
lab <- read_db("Laboratory")

# the biomek table has 8 locations available
mek_loc <- c("P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12")

# find the planned ligations by plate
# get digest quants numbers for ligations
ligs <- lab %>% 
  tbl("ligation") %>% 
  filter(plate == "L2776-L2871") %>% 
  select(ligation_id, digest_id, DNA, vol_in, water, well, plate) %>% 
  collect() %>% 
  arrange(ligation_id)

# just plan one lig plate at a time but still need plate name for code to work
lig_plates <- ligs %>% 
  distinct(plate)

ligs$dest <- "NA"
tips <- "NA"
water <- "NA"
  
#assign lig plate location in the last open position on the table
ligs <- assign_mek_loc(lig_plates, ligs, "dest", "ligation_id")
  
#assign tips in the last open position on the table
tips <- mek_loc[length(mek_loc)]
mek_loc <- mek_loc[1:length(mek_loc)-1]
 
#assign water in the last open position on the table
water <- mek_loc[length(mek_loc)]
mek_loc <- mek_loc[1:length(mek_loc)-1] 

# get source locations from database
source <- lab %>%
  tbl("digest") %>% 
  filter(digest_id %in% ligs$digest_id) %>% 
  select(digest_id, well, plate) %>% 
  collect()


source$source <- "NA"
dig_plates <- source %>% # there could be more than one source plate
  distinct(plate)
  
#assign sources in the last open position on the table
if (nrow(dig_plates) <= length(mek_loc)){
  source <- assign_mek_loc(dig_plates, source, "source", "digest_id") 
}else{ 
  stop()
}

# now there is a ligs table with a volume of sample to move to a destination 
# location and a volume of water to move to a destination location. There is
# also a source table with the location of the sample and water and tips objects
# with the location of water and tips. from here, need to make a csv that
# contains destination well, source well, destination location, source location
# and source volume for the samples and the water.

# pull the necessary columns from the ligs table and rename columns
biomek <- ligs %>% 
  select(well, dest, vol_in, ligation_id, digest_id) %>% 
  rename(
    lig_well = well,
    lig_loc = dest,
    dig_vol = vol_in
  )

# pull the necessary columns from the source table and rename columns
temp <- source %>% 
  select(well, source, digest_id) %>% 
  rename(
    dig_well = well,
    dig_loc = source
  )

# join the two sets of columns and rearrange order of columns
biomek <- left_join(biomek, temp, by = "digest_id") %>% 
  select(ligation_id, digest_id, lig_well, dig_well, lig_loc, dig_loc, dig_vol) 
rm(temp)

# write.csv(biomek, file = paste("output/",Sys.Date(), "_biomek.csv", sep = ""))

# create a csv for water ####
# pull the needed columns from ligs
water_file <- ligs %>% 
  select(ligation_id, water, well, dest) %>% 
  rename(
    water_vol = water,
    lig_well = well,
    lig_loc = dest
  )%>% 
  mutate(
    water_loc = water,
    water_well = "D6"
    )

# write.csv(water_file, file = paste("output/", Sys.Date(), "_biomek_water.csv", sep = ""))

# just in case you can have one file for both water and samples

combo <- left_join(biomek, water_file, by = c("ligation_id", "lig_well", "lig_loc"))

# write.csv(combo, file = paste("output/", Sys.Date(), "_biomek_combo.csv", sep = ""))
