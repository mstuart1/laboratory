# get source plates in heatmaps for ligations

source("scripts/lab_helpers.R")
library(ggplot2)

lab <- read_db("Laboratory")

ligs <- lab %>%
  tbl("ligation") %>% 
  collect()

digs <- lab %>% 
  tbl("digest") %>% 
  collect()

# filter out the pools you are looking for
pools <- ligs %>% 
  filter(pool == "P097" | pool == "P098")
pools <- pools %>% 
  arrange(ligation_id) %>% 
  mutate(plate = paste(min(pools$ligation_id), max(pools$ligation_id), sep = "-"), 
    barcode_num = rep(1:48, 2))

lig_dest_plate <- plate_from_db(pools, "ligation_id")

dig_dest_plate <- plate_from_db(pools, "digest_id")

sources <- digs %>% 
  filter(digest_id %in% pools$digest_id) %>% 
  select(plate) %>% 
  distinct()
  

# make a heatmap based on the volume in

for (i in sources$plate){
  map <- digs  %>%
    filter(plate == i) %>% 
    arrange(digest_id) %>% 
    mutate(row = substr(well, 1, 1),
      row = factor(row, levels = c("H", "G", "F", "E", "D", "C", "B", "A")), 
      col = substr(well, 2, 3),
      col = factor(col, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), 
      fill = ifelse(digest_id %in% pools$digest_id, "yes", "no")) %>% 
    select(row, col, digest_id, fill)  
  
  plateheatmap <- ggplot(map, aes(x=col, y=row, fill=fill)) + 
    geom_tile()
  
  plateheatmap + 
    geom_text(aes(col, row, label = digest_id), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank())
  ggsave(paste("plots/lig_sources_",i, ".pdf", sep = ""))
}

test <- change_rows(ligs, pools, "ligation_id")
labs <- write_db("Laboratory")
# dbWriteTable(labs, "ligation", test, row.names=F, overwrite = T)
