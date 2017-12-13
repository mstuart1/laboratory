# get the work history on any sample

source("scripts/lab_helpers.R")

item <- c("L2657", "L2691")
# item <- "L2620"
# item <- regeno
# item <- c("L0304","L0305","L0306","L0308","L0309","L0310","L0313","L0332")

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
    # filter(digest_id %in% info$digest_id) %>% 
    filter(digest_id == info$digest_id) %>%
    collect() %>% 
    select(-contains("corr"), -vol_in, -regeno, -water) %>% 
    rename(lig_date = date,
           lig_well = well, 
           lig_plate = plate, 
           lig_notes = notes)
  info <- left_join(info, temp, by = "digest_id")
}

if (grepl("D", item)){
  info <- try(lab %>% 
                tbl("digest") %>% 
                filter(digest_id %in% item$digest_id | digest_id == item)) %>% 
    collect()
  if("try-error" %in% class(info)){
    info <- lab %>%
      tbl("digest") %>% 
      filter(digest_id == item) %>% 
      collect()
  }else{
    info <- lab %>% 
      tbl("digest") %>% 
      filter(digest_id %in% item$digest_id) %>% 
      collect()
  }
  
  info <- info %>% 
    select(-contains("corr"), -contains("vol"), -ng_in) %>% 
    rename(dig_date = date,
           dig_quant = quant, 
           dig_well = well, 
           dig_plate = plate, 
           dig_notes = notes)
  
  temp <- try(lab %>% 
    tbl("extraction") %>% 
    filter(extraction_id %in% info$extraction_id ) %>% 
    collect())
  if("try-error" %in% class(temp)){
    temp <- lab %>% 
      tbl("extraction") %>% 
      filter(extraction_id == info$extraction_id[1]) %>% 
      collect()
  }else{
    temp <- lab %>% 
      tbl("extraction") %>% 
      filter(extraction_id %in% info$extraction_id ) %>% 
      collect()
  }
  
  temp <- temp %>%
    select(-final_vol, -contains("corr")) %>% 
    rename(extr_date = date, 
           extr_quant = quant, 
           extr_well = well,
           extr_plate = plate, 
           extr_notes = notes)
  
  info <- left_join(info, temp, by = "extraction_id")
  
  temp <- try(lab %>% 
                tbl("ligation") %>% 
                filter(digest_id %in% item$digest_id) %>% 
                collect())
  if("try-error" %in% class(temp)){
    temp <- lab %>%
      tbl("ligation") %>% 
      filter(digest_id == item) %>% 
      collect()
  }else{
    temp <- lab %>% 
      tbl("ligation") %>% 
      filter(digest_id %in% item$digest_id) %>% 
      collect()
  }
  
  info <- left_join(info, temp, by = "digest_id")
}

if (grepl("L", item)){
  info <- try(lab %>% 
    tbl("ligation") %>% 
    filter(ligation_id %in% item) %>% 
    collect())
  if("try-error" %in% class(info)){
    info <- lab %>%
      tbl("ligation") %>% 
      filter(ligation_id == item)%>% 
      collect()
  }else{
    info <- lab %>% 
      tbl("ligation") %>% 
      filter(ligation_id %in% item) %>% 
      collect()
  }
  
  info <- info %>% 
    collect() %>% 
    select(-contains("corr"), -vol_in, -regeno, -water) %>% 
    rename(lig_date = date,
           lig_well = well, 
           lig_plate = plate, 
           lig_notes = notes)
  
  temp <- lab %>% 
    tbl("digest") %>% 
    filter(digest_id %in% info$digest_id) %>%
    # filter(digest_id == info$digest_id) %>%
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
    # filter(extraction_id %in% info$extraction_id) %>% 
    filter(extraction_id == info$extraction_id) %>% 
    collect() %>%
    select(-final_vol, -contains("corr")) %>% 
    rename(extr_date = date, 
           extr_quant = quant, 
           extr_well = well,
           extr_plate = plate, 
           extr_notes = notes)
  
  info <- left_join(info, temp, by = "extraction_id")
  info <- select(info, pool, ligation_id, digest_id, extraction_id, everything())
  
}


