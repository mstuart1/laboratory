# import qubit results to db

source("scripts/lab_helpers.R")

lab <- read_db("Laboratory")

infile <- "data/QUBIT_2018-05-14_3-29-PM.csv"

### HAVE TO CHANGE THE MICROLITER SYMBOL TO UL INSTEAD OF THE SPECIAL CHARACTER ####
qubit <- read.csv(infile, header = T, stringsAsFactors = F)

### you get an error because there is a special symbol in the header row, change micro symbol to u ####

# eliminate unwanted data by date
qubit <- qubit %>% 
  filter(grepl("5/14", Date.Time)) %>% 
  select(Date.Time, Assay.Conc., Name) %>% 
  arrange(Name)

# # create a table of samples that were measured
# samples <- lab %>% 
#   tbl("digest") %>% 
#   filter(plate == "D5220-D5308") %>% 
#   filter(extraction_id < "E4412") %>% 
#   collect() %>% 
#   arrange(extraction_id) %>% 
#   select(extraction_id)

samples <- lab %>%
  tbl("digest") %>%
  filter(plate == "D5220-D5308") %>%
  collect() %>%
  arrange(digest_id) %>%
  select(digest_id)

# attach to qubit results
qubit <- cbind(qubit, samples)

# calculate ng/ul
qubit <- qubit %>% 
  mutate(new_quant = as.numeric(Assay.Conc.) * 0.2)

# extr <- lab %>% 
#   tbl("extraction") %>% 
#   collect()
# 
# change <- extr %>% 
#   filter(extraction_id %in% qubit$extraction_id) %>% 
#   mutate(correction = "Y", 
#     corr_editor = "MRS", 
#     corr_date = "2018-05-09", 
#     corr_message = paste("Changed quant from ", quant, " based on qubit results"))
# 
# compare <- left_join(select(change, extraction_id, quant), select(qubit, extraction_id, new_quant))
# 
# change <- left_join(change, select(qubit, extraction_id, new_quant), by = "extraction_id") %>% 
#   select(-quant) %>% 
# rename(quant = new_quant)
# 
# extr <- change_rows(extr, change, "extraction_id")
# 
# lab <- write_db("Laboratory")
# dbWriteTable(lab, "extraction", extr, row.names = F, overwrite = T)
# rm(lab, infile, change, compare, extr, qubit, samples)

dig <- lab %>%
  tbl("digest") %>%
  collect()

change <- dig %>%
  filter(digest_id %in% qubit$digest_id) %>%
  mutate(quant = qubit$new_quant, 
    date = "2018-05-10",
    final_vol = 40, 
    notes = NA)


dig <- change_rows(dig, change, "digest_id")

# lab <- write_db("Laboratory")
# dbWriteTable(lab, "digest", dig, row.names = F, overwrite = T)
# rm(lab, infile, change, dig, qubit, samples)
