# import qubit results to db

source("scripts/lab_helpers.R")

lab <- read_db("Laboratory")

infile <- "data/QUBIT_2018-05-15_2-06-PM.csv"

### HAVE TO CHANGE THE MICROLITER SYMBOL TO UL INSTEAD OF THE SPECIAL CHARACTER ####
qubit <- read.csv(infile, header = T, stringsAsFactors = F)

### you get an error because there is a special symbol in the header row, change micro symbol to u ####

# eliminate unwanted data by date
qubit <- qubit %>% 
  filter(grepl("5-15", Date.Time)) %>% 
  select(Date.Time, Assay.Conc., Name) %>% 
  arrange(Name)

qubit <- qubit %>% 
  filter(grepl("2:05", Date.Time)| grepl("2:06", Date.Time)) %>% 
  slice(2:nrow(qubit))

# create a table of samples that were measured
samples <- lab %>%
  tbl("extraction") %>%
  filter(plate == "E4478-E4550" & is.na(quant)) %>%
  collect() %>%
  arrange(extraction_id) %>%
  select(extraction_id)

# samples <- lab %>%
#   tbl("digest") %>%
#   filter(plate == "D5220-D5308") %>%
#   collect() %>%
#   arrange(digest_id) %>%
#   select(digest_id)

# attach to qubit results
qubit <- cbind(qubit, samples)

# calculate ng/ul
qubit <- qubit %>% 
  # mutate(quant = as.numeric(Assay.Conc.) * 0.2) %>% # for high sensitivity
  mutate(quant = as.numeric(Assay.Conc.) * 200) %>% # for broad range 
  select(extraction_id, quant)

extr <- lab %>%
  tbl("extraction") %>%
  collect()

change <- extr %>%
  filter(extraction_id %in% qubit$extraction_id) %>% 
  select(-quant)


change <- left_join(change, qubit, by = "extraction_id") 

extr <- change_rows(extr, change, "extraction_id")

# lab <- write_db("Laboratory")
# dbWriteTable(lab, "extraction", extr, row.names = F, overwrite = T)
# rm(lab, infile, change, extr, qubit, samples)

# dig <- lab %>%
#   tbl("digest") %>%
#   collect()
# 
# change <- dig %>%
#   filter(digest_id %in% qubit$digest_id) %>%
#   mutate(quant = qubit$new_quant, 
#     date = "2018-05-10",
#     final_vol = 40, 
#     notes = NA)
# 
# 
# dig <- change_rows(dig, change, "digest_id")

# lab <- write_db("Laboratory")
# dbWriteTable(lab, "digest", dig, row.names = F, overwrite = T)
# rm(lab, infile, change, dig, qubit, samples)


# compare <- left_join(select(change, extraction_id, quant), select(qubit, extraction_id, new_quant))
