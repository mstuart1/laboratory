# investigate digests

source("scripts/lab_helpers.R")

lab <- read_db("Laboratory")

# what plate is being examined
range = "D3340-D3435"
  # pull in the data
  digs <- lab %>% 
    tbl("digest") %>% 
    filter(plate == range) %>%
    collect()

# which samples are too low
  trouble <- digs %>% 
    filter(quant < 5, extraction_id != "XXXX")
  
# what was the extraction quant?
  extr <- lab %>% 
    tbl("extraction") %>% 
    filter(extraction_id %in% trouble$extraction_id) %>% 
    select(extraction_id, quant, sample_id) %>% 
    collect() %>% 
    rename(extr_quant = quant)
  trouble <- left_join(trouble, extr, by = "extraction_id") %>% 
    arrange(extraction_id)
  
  trouble$extraction_id
  
# this would be a good place to put a graph that looks for any extr_quants that stand out
  
# this would be a good place to look at how the ng_in relates to the dig_quant
  leyte <- read_db("Leyte")
  
fish <- leyte %>% 
  tbl("clownfish") %>% 
  filter(sample_id %in% extr$sample_id) %>%
  collect()
  
  
