# prep samples for ligation 

# SETUP ####
source("scripts/lab_helpers.R")

# define samples to be ligated (if known)
# dig_min <- "D3340"
# dig_max <- "D4587"


# define regenotype pool (if known)
regeno_min <- "D4588"
regeno_max <- "D4613"

# connect to the db
lab <- write_db("Laboratory")

# pull digest info from db

dig <- readRDS("../methods/procedural_notebooks/digs_need_seq.Rdata") %>% 
  select(digest_id, quant, well, plate) %>% 
    arrange(digest_id)
# dig <- dbReadTable(lab, "digest") %>% 
#   filter(digest_id >= dig_min & digest_id <= dig_max & extraction_id != "XXXX") %>%
#   select(digest_id, quant, well, plate) %>% 
#   arrange(digest_id)

regeno <- dbReadTable(lab, "digest") %>% 
  filter(digest_id >= regeno_min & digest_id <= regeno_max & extraction_id != "XXXX") %>%
  select(digest_id, quant, well, plate) %>% 
  arrange(quant)

# use the lig_ng function to assign ligation amounts to samples
# debugonce(lig_ng)
# out <- lig_ng(dig, regeno)
out <- data.frame() # make a blank data frame to write to
y <- nrow(dig) # get the number of beginning rows
for(i in c(200, 150, 100, 75, 50)){
  if (nrow(out) < y){ # if we haven't placed all of the candidates yet
    # define the samples that can be ligated at the current ng
    ng <- dig %>%
      mutate(uL_in = round(i/quant, 1)) %>% # round to 1 decimal point
      filter(uL_in < 22.2 & uL_in > 0.5) %>%
      mutate(water = round(22.2-uL_in, 1), 
        DNA = i) %>% 
      arrange(plate)
    # define regenos that can be ligated at the current ng
    reg <- regeno %>%
      mutate(uL_in = round(i/quant, 1)) %>% # round to 1 decimal point
      filter(uL_in < 22.2 & uL_in > 0.5) %>%
      mutate(water = round(22.2-uL_in, 1), 
        DNA = i) %>% 
      arrange(quant)
    # pull out  pools
      for (j in seq(nrow(reg))){
        while(nrow(ng) >=47){
        pool <- ng %>% 
          slice(1:47) %>% 
          mutate(pool = paste("pool", i, j, sep = "_"))
        re <- reg %>%
          slice(1) %>% 
          mutate(pool = paste("pool", i, j, sep = "_"))
      ng <- anti_join(ng, pool, by ="digest_id")
      reg <- anti_join(reg, re, by = "digest_id")
      out <- rbind(out, pool, re)
      
    }
    }
    dig <- anti_join(dig, out, by = "digest_id")
    regeno <- anti_join(regeno, out, by = "digest_id")
  }
  
}
# the above script stops when you run out of regenos - use below to keep going
for(i in c(200, 150, 100, 75, 50)){
  if (nrow(out) < y){ # if we haven't placed all of the candidates yet
    # define the samples that can be ligated at the current ng
    ng <- dig %>%
      mutate(uL_in = round(i/quant, 1)) %>% # round to 1 decimal point
      filter(uL_in < 22.2 & uL_in > 0.5) %>%
      mutate(water = round(22.2-uL_in, 1), 
        DNA = i) %>% 
      arrange(plate)
    # pull out  pools
      while(nrow(ng) >=48){
        pool <- ng %>% 
          slice(1:48) %>% 
          mutate(pool = paste("pool", i, sep = "_"))
        ng <- anti_join(ng, pool, by ="digest_id")
        out <- rbind(out, pool)
        
      }
    }
    dig <- anti_join(dig, out, by = "digest_id")
  
  }
  




out <- out %>% 
  rename(source_well = well, 
    source_plate = plate) %>% 
  mutate(source_loc = NA)



## testing
## what is left over - quantities that don't fit into a pool
# left <- anti_join(dig, out, by = "digest_id")
## Do any pools have less than 48 samples?
# test <- out %>%
  # group_by(pool) %>% 
  # summarise(num_samples = n()) %>% 
  # filter(num_samples != 48)

rm(dig, ng, reg, regeno, pool, re, i, j, y)

# # This section is if you are searching for regeno candidates ####
# # pull db info for regenotype samples - these should be samples that passed dDocent and haven't been regenotyped # regeno <- readRDS("data/passed_ligs.Rdata")
# 
# # OPEN WORK HISTORY SCRIPT to find sample info
# # which samples are only represented once?
# once <- info %>% 
#   group_by(sample_id) %>% 
#   summarise(num_geno = n()) %>% 
#   filter(num_geno == 1)
# 
# candidates <- info %>% 
#   filter(sample_id == once$sample_id[3] | sample_id == once$sample_id[4])
# 
# # need to redigest the candidates before this information can be filled in
# # current candidates are E1687 and E1688 - one has to ligate at 10ng and one at 50
#   
# out[95, ] <- c("digest_id", "quant", "well", "plate", "uL_in", "water", "DNA")
# out[96, ] <- c("digest_id", "quant", "well", "plate", "uL_in", "water", "DNA")



# define destination and source plates ####
# how many plates are there?
nplates <- nrow(out)/96
# what are the pool names
pool_list <- distinct(out, pool)
# set up the plate
plate <- data_frame()
# define wells
well <- data.frame("well" = 1:(96*nplates))

for (i in seq(nplates)){
  pool1 <- out %>% 
    filter(pool == pool_list$pool[1]) %>% 
    select(digest_id)
  pool2 <- out %>% 
    filter(pool == pool_list$pool[2]) %>% 
    select(digest_id)
    a <- 96*i-95 # position 1
    b <- 96*i-48     # position 48
    c <- 96*i-47 # position 49
    d <- 96*i # position 96
    temp <- cbind(well$well[a:b], pool1)
    colnames(temp) <- c("well", "digest_id")
    temp2 <- cbind(well$well[c:d], pool2)
    colnames(temp2) <- c("well", "digest_id")
    temp <- rbind(temp, temp2) %>% 
      mutate(row = rep(LETTERS[1:8], 12), 
        col = unlist(lapply(1:12, rep, 8)), 
        round = i)
    plate <- rbind(plate, temp)
    # remove pools from list because they have been used
    pool_list <- pool_list %>% 
      slice(3:nrow(pool_list))
}

plate <- plate %>% 
  mutate(dest_loc = "P12", 
    dest_well = paste(row, col, sep = ""), 
    water_loc = "P11", 
    water_well = "A1", 
    tip_loc = "P8")

rm(pool_list, pool1, pool2, temp, temp2, a,b,c,d, i)

ligations <- left_join(plate, out, by = "digest_id") %>% 
  select(-well, -row, -col)


# create source plates for each of the destination plates ####
plate_list <- distinct(ligations, round)

# for each round, define which source plates are present and assign source locations
for (i in seq(nrow(plate_list))){
  # narrow the info down to only the round in question
  dest_plate <- ligations %>% filter(round == plate_list$round[i])
  # which plates are source plates
  sources <- distinct(dest_plate, source_plate)
  # define possible source locations
  #P8 is tips, P11 is water, P12 is destination
  positions <- c("P5", "P6", "P7", "P9", "P10") 
  #assign sources to positions
  for (j in seq(nrow(sources))){
    ligations <- ligations %>% 
      mutate(source_loc = ifelse(source_plate == sources$source_plate[j], 
        positions[1], source_loc))
    # remove position from list
    positions <- positions[2:length(positions)]
  }
  # separate the rounds into csv files
  temp <- ligations %>% 
    filter(digest_id %in% dest_plate$digest_id)
  write.csv(temp, file = paste("data/", Sys.Date(), "robot_round", i, ".csv", sep = ""))
}
rm(dest_plate, out, plate, plate_list, sources, temp, well, i, j, nplates, positions)


# add ligations to database ####

# fetch last used ligation_id
lig_max <- dbReadTable(lab, "ligation") %>% 
  summarise(lig_id = max(ligation_id)) %>% 
  mutate(lig_id = as.numeric(substr(lig_id, 2, 5)))

pool_max <- dbReadTable(lab, "ligation") %>% 
  summarise(pool = max(pool)) %>% 
  mutate(pool = as.numeric(substr(pool, 2, 5)))

ligations <- ligations %>% 
  mutate(ligation_id = (1+lig_max$lig_id):(nrow(ligations) + lig_max$lig_id), 
    ligation_id = paste("L", ligation_id, sep = ""),
    pool = substr(pool, 6, 11), 
    pool = ifelse(substr(pool, 1, 2) == 75, 25, pool), 
    pool = ifelse(substr(pool, 1, 2) == 50, substr(pool, 4, 5), pool), 
    pool = ifelse(substr(pool, 1, 3) == 100, 26, pool),
    pool = as.numeric(pool) + pool_max$pool,
    pool = paste("P", formatC(pool, width = 3, format = "d", flag = 0),sep = "" ), 
    date = NA,
    barcode_num = NA,
    plate = NA, 
    notes = NA, 
    regeno = NA,
    total_reads = NA,
    lack_rad_tag = NA,
    low_quality = NA,
    retained = NA,
    correction = NA,
    corr_message = NA,
    corr_editor = NA,
    corr_date = NA) %>% 
  rename(vol_in = uL_in, 
    well = dest_well) %>% 
  select(ligation_id, digest_id, date, DNA, vol_in, water, barcode_num, pool, well, plate, notes, regeno,  everything(), -contains("loc"), -contains("source"), -water_well, -round, -quant)

## add to db
# dbWriteTable(lab, "ligation", ligations, append = T, row.names = F)
# dbDisconnect(lab)
# rm(lab)
