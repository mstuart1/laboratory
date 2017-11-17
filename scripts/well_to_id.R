# for a text file of well positions, pull the sample names of what is in the well


temp <- read.csv("~/Desktop/temporary.txt", stringsAsFactors = F, header = F)

extr <- lab %>% 
  tbl("extraction") %>% 
  filter(plate == "E2968-E3063") %>% 
  select(well, extraction_id) %>% 
  collect()

temp <- left_join(temp, extr, by = c("V1" = "well"))
(temp$extraction_id)
