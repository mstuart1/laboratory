# get work progress on all samples

source("../genomics/scripts/gen_helpers.R")
source("scripts/lab_helpers.R")
leyte <- read_db("Leyte")
lab <- read_db("Laboratory")

# Sequencing Success ####
# get all of the samples that have passed the dDocent filters and are considered successfully sequenced
# locate the genepop file and read as data frame
genfile <- "../genomics/data/seq17_03g95maf2q30dp15.gen"

genedf <- read_genepop(genfile)

# strip any named samples down to pure ligation number ---- 
# create a search term to search for ligation ids within a name
ligid <- "(.+)(L\\d\\d\\d\\d)" 
# change all of the names to ligation id only
genedf$names <- genedf$names %>% str_replace(ligid,"\\2")

genedf <- genedf %>% 
  select(names) %>% # remove columns of loci
  rename(ligation_id=names) %>% 
  mutate(seq = "yes")

# get sample ids for these ligations
samples <- samp_from_lig(genedf)

if (nrow(samples) != nrow(genedf)){
  # are there duplicate ligation ids in genedf?
  no_dups <- distinct(genedf, ligation_id) 
    # that wasn't the issue, same number of rows in no_dups as in genedf

  # which ligation ids are in genedf that are not in samples?
  no_sample <- genedf %>% filter(!ligation_id %in% samples$ligation_id)
    # these 11 samples are problem samples that can't be matched to a sample
}else{
  print("All sequenced samples accounted for")
}

write.csv(samples, file = paste("data/samples_successfully_seq_as_of_", Sys.Date(), ".csv", sep = "" ))

rm(no_dups, no_sample, genedf)

# Unable to Extract ####

# all the samples we have ever collected
all_samples <- leyte %>% 
  tbl("clownfish") %>% 
  filter(!is.na(sample_id)) %>% 
  select(sample_id, size) %>% 
  arrange(sample_id) %>% 
  collect()

# remove the samples that have successfully been sequenced
all_samples <- all_samples %>% 
  filter(!sample_id %in% samples$sample_id)

# get work history on each sample
extr <- lab %>% 
  tbl("extraction") %>% 
  select(sample_id, extraction_id, quant, date) %>% 
  collect() %>% 
  rename(extr_date = date)

# which samples have not been extracted?
no_extr <- all_samples %>% 
  filter(!sample_id %in% extr$sample_id)

# of the unextracted samples, which are planned to be extracted?
future_extr <- no_extr %>% 
  filter(grepl("APCL18", sample_id))
write.csv(future_extr, file = paste("data/to_be_extracted_as_of_", Sys.Date(), ".csv", sep = ""))

# which can never be extracted?
no_extr <- anti_join(no_extr, future_extr, by = "sample_id")
write.csv(no_extr, file = paste("data/unextractable_as_of_", Sys.Date(), ".csv", sep = ""))

# remove those from the list of samples that need work
samples <- all_samples %>% 
  filter(!sample_id %in% future_extr$sample_id, 
    !sample_id %in% no_extr$sample_id)

samples <- left_join(samples, extr, by = "sample_id")

rm(all_samples, extr, future_extr, no_extr)

dig <- lab %>% 
  tbl("digest") %>% 
  select(extraction_id, digest_id, quant, date) %>% 
  collect() %>% 
  rename(dig_quant = quant, 
    dig_date = date)

samples <- left_join(samples, dig, by = "extraction_id") 
rm(dig)

# # which samples were digested more than once?
# multi_dig <- samples %>% 
#   group_by(sample_id) %>% 
#   summarise(n = n()) %>% 
#   filter(n > 1)

lig <- lab %>% 
  tbl("ligation") %>% 
  select(digest_id, ligation_id, total_reads, retained, date) %>% 
  collect() %>% 
  rename(lig_date = date)

samples <- left_join(samples, lig, by = "digest_id")
rm(lig)

# # get notes  for the remaining samples
lig_notes <- lab %>%
  tbl("ligation") %>%
  select(ligation_id, notes) %>%
  collect()
samples <- left_join(samples, lig_notes, by = "ligation_id") %>%
  rename(lig_notes = notes)
rm(lig_notes)

dig_notes <- lab %>%
  tbl("digest") %>%
  select(digest_id, notes) %>%
  collect()
samples <- left_join(samples, dig_notes, by = "digest_id") %>%
  rename(dig_notes = notes)
rm(dig_notes)

extr_notes <- lab %>%
  tbl("extraction") %>%
  select(extraction_id, notes) %>%
  collect()

samples <- left_join(samples, extr_notes, by = "extraction_id") %>%
  rename(extr_notes = notes)
rm(extr_notes)

samp_notes <- leyte %>%
  tbl("clownfish") %>%
  select(sample_id, notes) %>%
  collect()
samples <- left_join(samples, samp_notes, by = "sample_id") %>%
  rename(samp_notes = notes)
rm(samp_notes)
 


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


# remove empty extracts
empty <- samples %>% 
  filter(grepl("empty", extr_notes))

samples <- samples %>% 
  filter(!sample_id %in% empty$sample_id)
write.csv(empty, file = paste("data/empty_extracts_as_of_", Sys.Date(), ".csv", sep = ""))


# how many samples have a ligation without a date? (planned to ligate) ####
lig_in_progress <- samples %>% 
  filter(!is.na(ligation_id) &  # has been planned in a ligation
    is.na(lig_date)) # has not been ligated
write.csv(lig_in_progess, file = paste("data/lig_in_progress_as_of_", Sys.Date(), ".csv", sep = ""))
samples <- samples %>% 
  filter(!sample_id %in% lig_in_progress$sample_id)

# how many samples are beyond ligation but not seq'd yet ####
in_progress <- samples %>% 
  filter(grepl("2018", lig_date) |    
    grepl("2018", dig_date))
samples <- samples %>% 
  filter(!sample_id %in% in_progress$sample_id)

# how many samples have a digest planned ####
dig_in_progress <- samples %>% 
  filter(!is.na(digest_id) &  # has been planned in a ligation
      is.na(dig_date)) # has not been ligated
write.csv(dig_in_progress, file = paste("data/dig_in_progress_as_of_", Sys.Date(), ".csv", sep = ""))
samples <- samples %>% 
  filter(!sample_id %in% dig_in_progress$sample_id)

# are any of the remaining samples multi-lig samples that have still failed?
multi_lig <- samples %>% 
  filter(!is.na(ligation_id), 
    !is.na(lig_date)) %>% 
  group_by(sample_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
write.csv(multi_lig, file = paste("data/multiple_lig_failures_as_of_", Sys.Date(), ".csv", sep = ""))
samples <- samples %>% 
  filter(!sample_id %in% multi_lig$sample_id)

# get the extract locations for the remaining samples so they can be rescued
extr_loc <- lab %>% 
  tbl("extraction") %>% 
  filter(sample_id %in% samples$sample_id) %>% 
  select(extraction_id, plate, well, gel) %>% 
  collect()
samples <- left_join(samples, extr_loc, by = "extraction_id")

samples <- samples %>% 
  select(sample_id, extraction_id, quant, plate, well) %>% 
  distinct()

write.csv(samples, file = paste("data/attempt_to_rescue_as_of_", Sys.Date(), ".csv", sep = ""), row.names = F)


# make a platemap of samples ####
samples <- read.csv("data/attempt_to_rescue_as_of_2018-05-22.csv", stringsAsFactors = F)

# extraction plates to pull from and the # of samples in each plate
plates <- samples %>% 
  group_by(plate) %>% 
  summarise(samps = n()) %>% 
  arrange(desc(samps))

# list of available wells in a plate
wells <- data.frame( row = rep(LETTERS[1:8], 12), col = unlist(lapply(1:12, rep, 8)))
wells <- wells %>% 
  mutate(row = factor(row, levels = c("H", "G", "F", "E", "D", "C", "B", "A")), 
col = factor(col, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), 
    well = paste(row, col, sep = ""))

need_wells <- data.frame()
# have_wells <- data.frame()
for(i in 1:nrow(plates)){
  have_wells <- samples %>% 
    filter(plate == plates$plate[i]) %>% 
    filter(well %in% wells$well)
  # have_wells <- rbind(have_wells, x)
  # are there any wells that are not available?
  unavail <- samples %>% 
    filter(plate == plates$plate[i]) %>% 
    filter(!well %in% wells$well)
  wells <- anti_join(wells, have_wells, by = "well")
  if (nrow(unavail) > 0){
    need_wells <- rbind(need_wells, unavail)
  }
}

in_tubes <- need_wells %>%
  filter(plate == "E0439-E0534") # need to get rid of 7 samples
write.csv(in_tubes, file = paste("digest_in_tubes_as_of_", Sys.Date(), ".csv", sep =""))

wells <- rename(wells, new_well = well) 
need_wells <- anti_join(need_wells, in_tubes, by = "extraction_id")
# wells <- slice(wells, 1:28)
need_wells <- cbind(need_wells, wells)

samples <- anti_join(samples, need_wells, by = "extraction_id")
samples <- samples %>% 
  mutate(row = NA, col = NA, new_well = well)
samples <- rbind(samples, need_wells)

samples <- anti_join(samples, in_tubes, by = "extraction_id")

samples <- samples %>% 
  mutate(row = substr(new_well, 1,1), 
    col = substr(new_well, 2, 3), 
    row = factor(row, levels = c("H", "G", "F", "E", "D", "C", "B", "A")), 
    col = factor(col, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
    
