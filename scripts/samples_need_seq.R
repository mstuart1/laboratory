# samples that haven't been sequenced "well"
# they might've been sequenced but didn't pass read or dDocent filters

source("../genomics/scripts/gen_helpers.R")
leyte <- read_db("Leyte")
lab <- read_db("Laboratory")

# all the samples we have ever collected
samples <- leyte %>% 
  tbl("clownfish") %>% 
  filter(!is.na(sample_id)) %>% 
  select(sample_id, size) %>% 
  arrange(sample_id) %>% 
  collect()

# get work history on each sample
extr <- lab %>% 
  tbl("extraction") %>% 
  select(sample_id, extraction_id) %>% 
  collect()

samples <- left_join(samples, extr, by = "sample_id")
rm(extr)

not_extr <- filter(samples, is.na(extraction_id))

# # get notes to see why not extracted
# temp <- leyte %>%
#   tbl("clownfish") %>%
#   filter(sample_id %in% not_extr$sample_id) %>%
#   select(sample_id, notes) %>%
#   collect()
# not_extr <- left_join(not_extr, temp, by = "sample_id")
# rm(temp)
# 
# # remove samples that cannot be extracted
# not_extr <- not_extr %>%
#   filter(!grepl("no", notes))
# 
# # save list of samples to be extracted
# save(not_extr, file="data/need_extract.Rdata")
  

# remove un-extracted samples from work list
samples <- anti_join(samples, not_extr, by = "sample_id")
# remove extracted samples from not_extr list
not_extr <- anti_join(not_extr, samples, by = "sample_id")

dig <- lab %>% 
  tbl("digest") %>% 
  select(extraction_id, digest_id) %>% 
  collect()

samples <- left_join(samples, dig, by = "extraction_id")
rm(dig)

not_dig <- filter(samples, is.na(digest_id))

# remove un-extracted samples from work list
samples <- anti_join(samples, not_dig, by = "extraction_id")
# remove extracted samples from not_extr list
not_dig <- anti_join(not_dig, samples, by = "sample_id")

lig <- lab %>% 
  tbl("ligation") %>% 
  select(digest_id, ligation_id, total_reads, retained) %>% 
  collect()

samples <- left_join(samples, lig, by = "digest_id")
rm(lig)

not_lig <- filter(samples, is.na(ligation_id))

# remove un-extracted samples from work list
samples <- anti_join(samples, not_lig, by = "digest_id")
# remove processes samples from not done list
not_lig <- anti_join(not_lig, samples, by = "sample_id")

# compare to the genepop
# locate the genepop file and read as data frame
genfile <- "../genomics/data/seq17_03g95maf2q30dp15.gen"

genedf <- read_genepop(genfile)


# 2) strip any named samples down to pure ligation number ---- 
# create a search term to search for ligation ids within a name
ligid <- "(.+)(L\\d\\d\\d\\d)" 
# change all of the names to ligation id only
genedf$names <- genedf$names %>% str_replace(ligid,"\\2")

genedf <- genedf %>% 
  select(names) %>% # remove columns of loci
  rename(ligation_id=names) %>% 
  mutate(seq = "yes")

samples <- left_join(samples, genedf, by = "ligation_id")
rm(genedf)

not_pass <- filter(samples, is.na(seq)) 
  
# remove un-extracted samples from work list
samples <- anti_join(samples, not_pass, by = "ligation_id")
# remove processes samples from not done list
not_pass <- anti_join(not_pass, samples, by = "sample_id")

# as of 12-20-2017 it looks like 311 of the not_pass samples are not in progress and could use a look to see if they can be made to work.
