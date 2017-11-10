# which samples have been sequenced successfully, and which have not?
library(dbplyr)
source("../genomics/scripts/gen_helpers.R")
leyte <- read_db("Leyte")
lab <- read_db("Laboratory")

# Coming at it from the sample direction
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
rm(extr)

multi <- samples %>% 
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

# for each sample, did we digest it, 
# and if we digested it more than once, 
# which digest has the highest quantity?
dig <- lab %>% 
  tbl("digest") %>% 
  filter(extraction_id %in% samples$extraction_id) %>% 
  select(digest_id, extraction_id, quant) %>% 
  rename(dig_quant = quant) %>% 
  collect()
samples <- left_join(samples, dig, by = "extraction_id")
rm(dig)

multi <- samples %>% 
  group_by(sample_id) %>% 
  summarize(digests = n()) %>% 
  filter(digests > 1)

new <- data.frame()
for(i in 1:nrow(multi)){
  x <- samples %>% 
    filter(sample_id == multi$sample_id[i])
  x <- x[which.max(x$dig_quant), ]
  new <- rbind(new, x)
}

# remove multi from samples
samples <- samples %>% 
  filter(!sample_id %in% multi$sample_id)
# add in the "new" rows
samples <- rbind(samples, new)

# for each sample, did we ligate it, 
# and if we ligated it more than once, 
# which ligation has the highest quantity?
lig <- lab %>% 
  tbl("ligation") %>% 
  filter(digest_id %in% samples$digest_id) %>% 
  select(digest_id, ligation_id, DNA, retained) %>% 
  rename(lig_ng_in = DNA) %>% 
  collect()
samples <- left_join(samples, lig, by = "digest_id")
rm(lig)

multi <- samples %>% 
  group_by(sample_id) %>% 
  summarize(ligations = n()) %>% 
  filter(ligations > 1)

new <- data.frame()
for(i in 1:nrow(multi)){
  x <- samples %>% 
    filter(sample_id == multi$sample_id[i])
  x <- x[which.max(x$retained), ]
  new <- rbind(new, x)
}

# remove multi from samples
samples <- samples %>% 
  filter(!sample_id %in% multi$sample_id)
# add in the "new" rows
samples <- rbind(samples, new)

# are there any samples that didn't get sequenced? 
no_seq <- samples %>% 
  filter(is.na(retained))

# what samples are in the genepop?

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
  rename(ligation_id=names)

# Add sample IDs ----------------------------------------------------------
samples <- samp_from_lig(genedf)

# are there any ligations that were sequenced that aren't on this list?
missing_ligs <- genedf %>% 
  filter(!ligation_id %in% samples$ligation_id)
# what are the sample_ids for these ligations?
missing_ligs <- samp_from_lig(missing_ligs)

#remove these samples from the no_seq list
no_seq <- anti_join(no_seq, missing_ligs, by = "sample_id") %>% 
  filter(extraction_id > "E0151")

# make a list of extractions and separate them into what will need to be done:
# empty, 
empty <- lab %>% 
  tbl("extraction") %>% 
  collect()%>% 
  filter(sample_id %in% no_seq$sample_id, 
         grepl("empty", x = notes, ignore.case = T) | 
           grepl("used up", notes) |
           grepl("no sample", notes)
         ) 
no_seq <- anti_join(no_seq, empty, by = "sample_id")

# no DNA present
no_DNA <- no_seq %>% 
  filter(extr_quant < 1.5)
no_seq <- anti_join(no_seq, no_DNA, by = "sample_id")
write.csv(no_DNA, file = "data/When_you_get_a_chance_check_these.csv", row.names = F)

# low_conc digest
low_enzyme <- no_seq %>% 
  filter(extr_quant >= 1.5 & dig_quant <= 5)
no_seq <- anti_join(no_seq, low_enzyme, by = "sample_id")
write.csv(low_enzyme, file = "data/low_enzyme_samples.csv", row.names = F,  quote = F)

# regular digest
write.csv(no_seq, file = "data/redigest_these_samples.csv", row.names = F)


# samples were deleted for having an NA for a quant or retained reads - find them

# ____________________________

# Coming at it from the genepop direction ----


# what samples are in the genepop?

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
  rename(ligation_id=names)

# Add sample IDs ----------------------------------------------------------
samples <- samp_from_lig(genedf)

# Merge the two dataframes so that lig IDs match up -----------------------
ligs <- left_join(genedf, samples, by = "ligation_id")
rm(samples, genedf)

# which samples aren't on the ligation list? #1910 on 11/8/2017
fish <- leyte %>% 
  tbl("clownfish") %>% 
  collect()%>% 
  filter(!is.na(sample_id), !sample_id %in% ligs$sample_id) %>% 
  select(sample_id, size) 

# get extraction record
extr <- lab %>% 
  tbl("extraction") %>% 
  filter(sample_id %in% fish$sample_id) %>% 
  select(sample_id, extraction_id, quant) %>% 
  rename(extr_quant = quant) %>% 
  collect()

unseq <- left_join(fish, extr, by = "sample_id")
rm(extr)

# get a digest record
dig <- lab %>% 
  tbl("digest") %>% 
  filter(extraction_id %in% unseq$extraction_id) %>% 
  select(digest_id, extraction_id, quant) %>% 
  rename(dig_quant = quant) %>% 
  collect()

unseq <- left_join(unseq, dig, by = "extraction_id")
rm(dig)

# get ligation record
lig <- lab %>% 
  tbl("ligation") %>% 
  filter(digest_id %in% unseq$digest_id) %>% 
  select(ligation_id, digest_id, DNA, total_reads, retained) %>% 
  rename(lig_ng_in = DNA) %>% 
  collect()

unseq <- left_join(unseq, lig, by = "digest_id")
rm(lig)

# take a look at samples with more than one record
multi <- unseq %>% 
  group_by(sample_id) %>% 
  summarise(work = n()) %>% 
  filter(work > 1)

# remove those from the group because they have to be treated differently
unseq <- anti_join(unseq, multi, by = "sample_id")

# never extracted 
no_extr <- unseq %>% 
  filter(is.na(extraction_id))

unseq <- anti_join(unseq, no_extr, by = "sample_id")

# never digested
no_dig <- unseq %>% 
  filter(is.na(digest_id))

# # # pull out sammple_id and extraction_id
# # test <- no_dig %>% 
# #   select(sample_id, extraction_id) %>% 
# #   arrange(extraction_id)
# # write.csv(test, file = "data/test.csv", row.names = F, quote = F)
# 
# # break up no dig into samples that are empty extraction wells
# empty <- c("E0013", "E0032", "E0141", "E0142","E0143", "E0726", "E1223", "E1239")
# no_dig <- filter(no_dig, !extraction_id %in% empty)
# 
# # separate out samples that need to be re-checked for presence of DNA
# low_dig_check <- no_dig %>% 
#   filter(extr_quant <= 1.5)
# no_dig <- anti_join(no_dig, low_dig_check, by = "extraction_id")
# 
# # samples that can be digested with a low conc enzyme
# low_enzyme <- no_dig %>% 
#   filter(extr_quant <= 5)
# no_dig <- anti_join(no_dig, )
#   
unseq <- anti_join(unseq, no_dig, by = "extraction_id")

# never ligated
no_lig <- unseq %>% 
  filter(is.na(ligation_id))

unseq <- anti_join(unseq, no_lig, by = "digest_id")

# didn't go through dDocent/filtering
no_seq <- unseq %>% 
  filter(is.na(total_reads))

# the remaining samples had bad sequences
unseq <- anti_join(unseq, no_seq, by = "ligation_id")

# is there a trend between total DNA in and read numbers
plot(x = unseq$lig_ng_in, y = unseq$retained)

# strange high number of reads
test <- filter(unseq, lig_ng_in < 50)
plot(x = test$lig_ng_in, y = test$retained)

test2 <- filter(unseq, lig_ng_in != 25)
plot(x = test2$lig_ng_in, y = test2$retained)

