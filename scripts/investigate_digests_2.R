# why did plates fail?
source("scripts/lab_helpers.R")
library(tidyr)

# what % of ligations that are sent for sequencing are genotyped?
lab <- read_db("Laboratory")

# which pools were sent for sequencing?
seqs <- lab %>% 
  tbl("sequencing") %>% 
  select(seq_id, contents) %>% 
  collect() %>% 
  filter(grepl("SEQ", seq_id)) %>% 
  filter(seq_id != "SEQ10", seq_id != "SEQ11", seq_id != "SEQ14", seq_id != "SEQ01", seq_id != "SEQ02" , seq_id != "SEQ06")

pools <- seqs %>% 
  separate(contents, into = c("pool1", "pool2", "pool3", "pool4"), sep = ", ") %>% 
  gather(pool1:pool4, key = "number", value = "pool") %>% 
  select(-number) %>% 
  filter(!is.na(pool))


ligs <- lab %>% 
  tbl("ligation") %>% 
  filter(pool %in% pools$pool) %>% 
  collect()
s <- nrow(ligs)

print(paste(s, "ligations have been sent to the sequencer", sep = " "))

# how many sequences have come back from the sequencer
source("../genomics/scripts/gen_helpers.R")
genfile <- "../genomics/data/seq17_03g95maf2q30dp15.gen"
genedf <- read_genepop(genfile)
g <- nrow(genedf)

print(paste(g, "genotypes have come back from sequencer", sep = " "))

diff <- formatC(g/s * 100)

print(paste(diff, "% individuals sent to the sequencer have come back useable", sep = ""))

# how many digests were ligated?
digs <- lab %>% 
  tbl("digest") %>% 
  filter(enzymes == "PstI-MluCI" | enzymes == "PstI_MluCI") %>% 
  filter(!is.na(quant)) %>% # remove those in the process of being made
  collect() %>% 
  filter(date != "2017-10-19")
d <- nrow(digs)

print(paste(d, "digests have been created with the APCL enzymes"))

dig_ligs <- ligs %>% 
  filter(digest_id %in% digs$digest_id)
l <- nrow(dig_ligs)

print(paste(l, "digests have become ligations, that is", formatC(l/d * 100), "%"))

# which digests have not become ligations
non_ligs <- digs %>% 
  filter(!digest_id %in% ligs$digest_id & extraction_id != "XXXX")
  

list_of_extr <- non_ligs %>% 
  select(extraction_id) %>% 
  distinct(extraction_id) %>% 
  arrange(extraction_id)

# are any of these in the "in progress" digests?
in_progress <- lab %>% 
  tbl("digest") %>% 
  filter(enzymes == "PstI-MluCI" | enzymes == "PstI_MluCI") %>% 
  filter(is.na(quant)) %>% # remove those in the process of being made
  collect() %>% 
  filter(date == "2017-10-19") %>% 
  filter(extraction_id %in% list_of_extr)

