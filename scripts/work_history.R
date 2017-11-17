# get the work history on any sample

source("scripts/lab_helpers.R")

item <- "E1872"

lab <- read_db("Laboratory")

if (grepl("E", item)){
  info <- lab %>% 
    tbl("extraction") %>% 
    filter(extraction_id == item) %>% 
    collect() %>%
    select(-final_vol, -contains("corr")) %>% 
    rename(extr_date = date, 
           extr_quant = quant, 
           extr_well = well,
           extr_plate = plate, 
           extr_notes = notes)
  temp <- lab %>% 
    tbl("digest") %>% 
    filter(extraction_id == item) %>% 
    collect() %>% 
    select(-contains("corr"), -contains("vol"), -ng_in) %>% 
    rename(dig_date = date,
           dig_quant = quant, 
           dig_well = well, 
           dig_plate = plate, 
           dig_notes = notes)
  info <- left_join(info, temp, by = "extraction_id")
  
  temp <- lab %>% 
    tbl("ligation") %>% 
    filter(digest_id %in% info$digest_id) %>% 
    collect() %>% 
    select(-contains("corr"), -vol_in, -regeno, -water) %>% 
    rename(lig_date = date,
           lig_well = well, 
           lig_plate = plate, 
           lig_notes = notes)
  info <- left_join(info, temp, by = "digest_id")
}

if (grepl("L", item)){
  info <- lab %>% 
    tbl("ligation") %>% 
    filter(ligation_id == item) %>% 
    collect() %>% 
    select(-contains("corr"), -vol_in, -regeno, -water) %>% 
    rename(lig_date = date,
           lig_well = well, 
           lig_plate = plate, 
           lig_notes = notes)
  
  temp <- lab %>% 
    tbl("digest") %>% 
    filter(digest_id %in% info$digest_id) %>% 
    collect() %>% 
    select(-contains("corr"), -contains("vol"), -ng_in) %>% 
    rename(dig_date = date,
           dig_quant = quant, 
           dig_well = well, 
           dig_plate = plate, 
           dig_notes = notes)
  info <- left_join(info, temp, by = "digest_id")
 
  temp <- lab %>% 
    tbl("extraction") %>% 
    filter(extraction_id %in% info$extraction_id) %>% 
    collect() %>%
    select(-final_vol, -contains("corr")) %>% 
    rename(extr_date = date, 
           extr_quant = quant, 
           extr_well = well,
           extr_plate = plate, 
           extr_notes = notes)
  
  info <- left_join(info, temp, by = "extraction_id")
  
}


