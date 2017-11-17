# did a ligation pass dDocent filters?

library(stringr)
source("../genomics/scripts/gen_helpers.R")

# which ligations passed
genfile <- "../genomics/data/seq17_03g95maf2q30dp15.gen"

genedf <- read_genepop(genfile) %>% 
  select(names) %>% 
  rename(ligation_id = names)

# strip away sample names so it is only ligation_id

# create a search string to find ligation ids
strg<- "(.+)(L\\d\\d\\d\\d)" 
# test # str_detect(genedf$ligation_id, strg)
# change all of the names to ligation id only
genedf$ligation_id <- genedf$ligation_id %>% str_replace(strg,"\\2")

saveRDS(genedf, file = "data/passed_ligs.Rdata")
