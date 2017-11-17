# samples that need to be sequenced 
# library(dbplyr)
source("../genomics/scripts/gen_helpers.R")
leyte <- read_db("Leyte")
lab <- read_db("Laboratory")



# samples that have been collected but not extracted

# all the samples we have ever collected
samples <- leyte %>% 
  tbl("clownfish") %>% 
  filter(!is.na(sample_id)) %>% 
  select(sample_id, size) %>% 
  arrange(sample_id) %>% 
  collect()

# for each sample, did we extract it, 
# and if we extracted it more than once, 
# which extraction has the highest quantity?
extr <- lab %>% 
  tbl("extraction") %>% 
  filter(sample_id %in% samples$sample_id) %>% 
  select(sample_id, extraction_id, quant) %>% 
  rename(extr_quant = quant) %>% 
  collect()
samples <- left_join(samples, extr, by = "sample_id")
need_extr <- anti_join(samples, extr, by = "sample_id") 
samples <- anti_join(samples, need_extr, by = "sample_id")


multi <- samples %>% # these are the 2012 testing phase samples
  group_by(sample_id) %>% 
  summarize(extractions = n()) %>% 
  filter(extractions > 1)

new <- data.frame()
for(i in 1:nrow(multi)){
  x <- samples %>% 
    filter(sample_id == multi$sample_id[i])
  x <- x[which.max(x$extr_quant), ]
  new <- rbind(new, x)
}

# remove multi from samples
samples <- samples %>% 
  filter(!sample_id %in% multi$sample_id)
# add in the "new" rows
samples <- rbind(samples, new)
# these are all samples that have been extracted.  


# which extractions were not digested
digs <- lab %>% 
  tbl("digest") %>% 
  filter(extraction_id %in% samples$extraction_id) %>% 
  select(digest_id, extraction_id) %>% 
  collect()
samples <- left_join(samples, digs, by = "extraction_id")
no_digs <- anti_join(samples, digs, by = "extraction_id")
samples <- anti_join(samples, no_digs, by = "extraction_id")

# some of the no-digs are in progress, some were never extracted, 

