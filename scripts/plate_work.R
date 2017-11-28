# make a heat map of which samples were pulled for labwork
source("scripts/lab_helpers.R")
library(ggplot2)

# get samples and plate locations from db
lab <- read_db("Laboratory")

# want to know which digests have been pulled into pools
  digs <- lab %>% 
    tbl("digest") %>% 
    filter(plate == "D0460-D0555") %>% 
    select(digest_id, extraction_id, well) %>% 
    collect() %>% 
    mutate(row = factor(substr(well, 1, 1), levels = c("H", "G", "F", "E", "D", "C", "B", "A")), 
      col = factor(substr(well, 2, 3), levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
  
  pools <- lab %>% 
    tbl("ligation") %>% 
    filter(digest_id %in% digs$digest_id) %>% 
    select(digest_id, pool) %>% 
    collect()
  
  digs <- left_join(digs, pools, by  = "digest_id")
  digs <- digs %>%
    mutate(pool = ifelse(grepl("P", pool), pool, NA))
  
  plate <- ggplot(data = digs, aes(x= col, y = row, fill = pool))+
    geom_tile(alpha = 0.05, color = "black") +
    geom_text(aes(col, row, label = digest_id), color = "black", size = 4) +
    theme(
      axis.text.x.top = element_text(), 
      axis.title.x = element_blank(), # removes x-axis title
      axis.title.y = element_blank(), # removes y-axis title
      panel.grid.major = element_blank(), # removes grid from background
      panel.grid.minor = element_line("black"), # did nothing
      panel.border = element_blank(), # I don't notice a difference
      panel.background = element_blank(),  # removes grey background
      axis.ticks = element_blank()) #removes axis ticks\
  print(plate)
  
  ggsave(paste("plots/D0460-D0555.pdf", sep = ""))
  
  
  
  
    
  

  




# make a map of sources
plate <- ggplot(data = full, aes(x= col, y = row))+
  geom_tile(alpha = 0, color = "black") +
  geom_text(aes(col, row, label = extraction_id), color = "black", size = 4) +
  theme(
    axis.text.x.top = element_text(), 
    axis.title.x = element_blank(), # removes x-axis title
    axis.title.y = element_blank(), # removes y-axis title
    panel.grid.major = element_blank(), # removes grid from background
    panel.grid.minor = element_line("black"), # did nothing
    panel.border = element_blank(), # I don't notice a difference
    panel.background = element_blank(),  # removes grey background
    axis.ticks = element_blank()) #removes axis ticks\
print(plate)
# ggsave(paste("plots/", plate_id, "_extrdest.pdf", sep = ""))

# make a map of the source plate
extr_plate <- lab %>%
  filter()
    

# # # make a list of plate names
# # plates <- digs %>% 
# #   distinct(plate)
# 
# # make a blank full plate
# blank <- data_frame(row = rep(LETTERS[1:8], 12), col = unlist(lapply(1:12, rep, 8)))
# blank$well <- paste(blank$row, blank$col, sep = "")
# 
# # separate out 1 plate from digs
# # current <- digs %>% 
#   # filter(plate == "D4204-D4299")
# current <- digs
# # current <- ligs
# 
# full <- left_join(blank, current, by = "well") %>% 
#   select(row, col, digest_id)
#   # select(row, col, ligation_id)
# 
# # make a plate map
# platemap <<- as.matrix(reshape2::acast(full, full$row ~ full$col))
# 
# # make a source map
# source <- current %>% 
#   select(well, extraction_id)
#   # select(well, digest_id)
# 
# fulls <- left_join(blank, source, by = "well") %>% 
#   select(row, col, extraction_id)
#   # select(row, col, digest_id)
# 
# platemap <<- as.matrix(reshape2::acast(fulls, fulls$row ~ fulls$col))


  
