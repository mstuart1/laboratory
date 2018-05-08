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
  select(sample_id, extraction_id, quant) %>% 
  collect()

samples <- left_join(samples, extr, by = "sample_id") %>% 
  rename(extr_quant = quant)
rm(extr)

dig <- lab %>% 
  tbl("digest") %>% 
  select(extraction_id, digest_id, quant) %>% 
  collect()

samples <- left_join(samples, dig, by = "extraction_id") %>% 
  rename(dig_quant = quant)
rm(dig)

# # which samples were digested more than once?
# multi_dig <- samples %>% 
#   group_by(sample_id) %>% 
#   summarise(n = n()) %>% 
#   filter(n > 1)

lig <- lab %>% 
  tbl("ligation") %>% 
  select(digest_id, ligation_id, total_reads, retained) %>% 
  collect()

samples <- left_join(samples, lig, by = "digest_id")
rm(lig)


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

seq <- samples %>% 
  filter(seq == "yes")
not_pass <- samples %>% 
  filter(!sample_id %in% seq$sample_id)
  
# get notes  for the not_pass samples
lig_notes <- lab %>% 
  tbl("ligation") %>% 
  select(ligation_id, notes) %>% 
  collect() 
not_pass <- left_join(not_pass, lig_notes, by = "ligation_id") %>% 
  rename(lig_notes = notes)
rm(lig_notes)

dig_notes <- lab %>% 
  tbl("digest") %>% 
  select(digest_id, notes) %>% 
  collect()
not_pass <- left_join(not_pass, dig_notes, by = "digest_id") %>% 
  rename(dig_notes = notes)
rm(dig_notes)

extr_notes <- lab %>% 
  tbl("extraction") %>% 
  select(extraction_id, notes) %>% 
  collect() 
# %>% 
#   filter(extraction_id != "E0726", # sample is empty but notes aren't filtering
#     !grepl("empty", notes), 
#     !grepl("no band", notes), 
#     !grepl("No band", notes))
not_pass <- left_join(not_pass, extr_notes, by = "extraction_id") %>% 
  rename(extr_notes = notes)
rm(extr_notes)

samp_notes <- leyte %>% 
  tbl("clownfish") %>% 
  select(sample_id, notes) %>% 
  collect()
not_pass <- left_join(not_pass, samp_notes, by = "sample_id") %>% 
  rename(samp_notes = notes)
rm(samp_notes)

not_pass <- not_pass %>% 
  select(sample_id, size, samp_notes, extraction_id, extr_quant, extr_notes, digest_id, dig_quant, dig_notes, ligation_id, lig_notes, total_reads, retained, seq)

# # are there any trends between extraction quantification or fish size of samples that passed sequencing and samples that did not?
# library(ggplot2)

# # is there a size difference between successful samples and not?
# samples <- samples %>% 
#   mutate(size = as.numeric(size), 
#     seq = ifelse(is.na(seq), "no", seq))
# subset <- samples %>% 
#   filter(!is.na(ligation_id)) # samples that were passed to sequencer
# ggplot(data = subset, mapping = aes(x = seq, y = size)) +
#   geom_boxplot()
# # not really

# # # is there a extr_quant difference between successful samples and not?
# samples <- samples %>%
#   mutate(seq = ifelse(is.na(seq), "no", seq))
# subset <- samples %>%
#   filter(!is.na(ligation_id)) # samples that were passed to sequencer
# ggplot(data = subset, mapping = aes(x = seq, y = extr_quant)) +
#   geom_boxplot()
# # not really
# # what about all samples?
# ggplot(data = samples, mapping = aes(x = seq, y = extr_quant)) +
#   geom_boxplot()
# # get rid of high outliers to zoom in on box
# subset <- samples %>% 
#   filter(!is.na(extr_quant), 
#     extr_quant < 150)
# ggplot(data = subset, mapping = aes(x = seq, y = extr_quant)) +
#   geom_boxplot()
# # no difference

  # not_pass <- not_pass %>%
#   # remove extractions that can't be digested
#   filter(extraction_id >= "E0247", 
#     extraction_id != "E0726")
# 
# # for ligations that are in progress, remove those sample_ids from the list
# lig_prog <- not_pass %>% 
#   filter(grepl("planned", lig_notes), 
#     !grepl("failed", lig_notes)) %>% 
#   select(sample_id)
# not_pass <- anti_join(not_pass, lig_prog, by = "sample_id")
# rm(lig_prog)
# 
# # which of these samples have not been digested? ####
# no_dig <- not_pass %>% 
#   filter(is.na(digest_id)) %>% 
#   arrange(sample_id)

# which not_pass samples are unextracted?
unextracted <- not_pass %>% 
  filter(is.na(extraction_id), 
    !grepl("APCL18", sample_id))

# remove samples that are unextractable (no fin in tube, no EtOH)
not_pass <- not_pass %>% 
  filter(!sample_id %in% unextracted$sample_id)

# remove samples that are 2018 collection
not_pass <- not_pass %>% 
  filter(!grepl("APCL18", sample_id))

# remove samples that are in progress 
mid_progress <- not_pass %>% 
  filter(grepl("plan", lig_notes)| 
    grepl("plan", dig_notes))
not_pass <- not_pass %>% 
  filter(!sample_id %in% mid_progress$sample_id)

# remove samples with no extract left
not_pass <- not_pass %>% 
  filter(!grepl("empty", extr_notes))

# remove samples that have been ligated more than once and still haven't succeeded
multi_lig <- not_pass %>% 
  filter(!is.na(ligation_id)) %>% 
  group_by(sample_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
not_pass <- not_pass %>% 
  filter(!sample_id %in% multi_lig$sample_id)

extr_loc <- lab %>% 
  tbl("extraction") %>% 
  select(extraction_id, plate, well, gel) %>% 
  collect()
not_pass <- left_join(not_pass, extr_loc, by = "extraction_id")

write.csv(not_pass, file = "data/attempt_to_rescue.csv", row.names = F)
