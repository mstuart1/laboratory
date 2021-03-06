# make a pool map # TODO look into ggplot geom.tile to make a binary heat map of whether a cell has been flagged
library(ggplot2)
source("scripts/lab_helpers.R")

lab <- read_db("Laboratory")

# get a list of the pools
non_clown_pools <-c("P028", "P029",  "P034", "P035", "P036", "P037", "P038", "P043", "P044",
                    "P045", "P046", "P047", "P048", "P049", "P050", "P051", "P052") 

pool_list <- lab %>% 
  tbl("ligation") %>% 
  select(pool) %>% 
  distinct(pool) %>% 
  collect() %>% 
  filter(grepl("P", pool), # remove pools that are not part of the clownfish project
         !pool %in% non_clown_pools) 

# make a map of pool

# # make a list of plate names
# plates <- pool_list

# make a blank full plate
blank <- data_frame(row = rep(LETTERS[1:8], 12), col = unlist(lapply(1:12, rep, 8)))
blank$well <- paste(blank$row, blank$col, sep = "")

for (i in 1:nrow(pool_list)){
  # separate out 1 plate from digs
  current <- lab %>% 
    tbl("ligation") %>% 
    filter(pool == pool_list$pool[i]) %>% 
    select(ligation_id, well, pool) %>% 
    collect()
  
  full <- left_join(blank, current, by = "well") %>% 
    select(row, col, ligation_id)
  
  # make a plate map
  platemap <<- as.matrix(reshape2::acast(full, full$row ~ full$col))
  
  # which samples have successfully passed through dDocent?
  success <- readRDS("data/passed_ligs.Rdata")
  
  current <- current %>%
    mutate(pass = ifelse(ligation_id %in% success$ligation_id, NA, "fail"), 
           row = substr(well, 1, 1), 
           col = substr(well, 2, 3))
  
  temp <- samp_from_lig(current)
  current <- left_join(current, temp, by = "ligation_id")
  
  map <- current %>% 
    select(row, col, pass, ligation_id) %>% 
    mutate(row = factor(row, levels = c("H", "G", "F", "E", "D", "C", "B", "A")), 
           col = factor(col, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
  
  if(length(which(map$pass == "fail")) > 1){
  # make the heat map
  plateheatmap <- ggplot(map, aes(x=col, y=row, fill= pass)) + 
    geom_tile()
  
  plateheatmap + 
    geom_text(aes(col, row, label = ligation_id), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank())
  
  ggsave(paste("plots/", pool_list$pool[i], "_pass.pdf", sep = ""))
  }
  
  # which successful samples have been regenotyped?
  
  item <- success
  
  lab <- read_db("Laboratory")
  
  info <- lab %>% 
    tbl("ligation") %>% 
    filter(ligation_id %in% item$ligation_id) %>% 
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
  
  pool_success <- info %>% 
    filter(pool == pool_list$pool[i])
  
  sample_sum <- info %>% 
    filter(sample_id %in% pool_success$sample_id) %>% 
    group_by(sample_id) %>% 
    summarise(regeno = n()) %>% 
    filter(regeno > 1)

    regeno <- current %>% 
      mutate(regeno = ifelse(sample_id %in% sample_sum$sample_id, "Y", NA)) %>% 
      select(regeno, ligation_id)
    
    map <- left_join(map, regeno, by = "ligation_id")
    
    if(length(which(map$regeno == "Y")) > 1){
    # make the heat map
    plateheatmap <- ggplot(map, aes(x=col, y=row, fill= pass)) + 
      geom_tile()
    regenomap <- ggplot(map, aes(x = col, y= row, fill = regeno, color = "blue")) +
      geom_tile(color = "black") # makes black lines between cels
    
    regenomap + 
      geom_text(aes(col, row, label = ligation_id), color = "black", size = 4) 
    ggsave(paste("plots/", pool_list$pool[i], "_regeno.pdf", sep = ""))
    }
    
}

  
  
  



