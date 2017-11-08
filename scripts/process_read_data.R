# This script imports the read number data from the tsv created on amphiprion.
# It assigns Ligation ID to barcode
# It appends the existing data file that keeps a running tally of read number stats

source("scripts/lab_helpers.R")

# look up the data files - fetched from amphiprion
pools <- list.files(path = "data/", pattern = "process.out.tsv")
names <- list.files(path = "data/", pattern = "names-")
seq <- 17

# create an empty dataframe
reads <- data.frame()
for (i in 1:length(pools)){
  # determine which pool is active
  pool <- substr(pools[i], 1,2)
  # assign the nth file to filename
  # using grep makes sure the same pool is used regardless of position in the list
  filename1 <- paste("data/", pools[grep(pool, pools)], sep = "") 
  # read in the data file
  data <- read.delim(filename1, header = F) 
  # rename the columns
  colnames(data) <- c("barcode", "total_reads", "no_rad_tag", "low_quality", "retained") 
  # assign the name file to a filename
  filename2 <- paste("data/", names[grep(pool, names)], sep = "")
  name <- read.delim(filename2, header = F) 
  # rename columns in name file
  colnames(name) <- c("ligation_id", "barcode") 
  # add the ligation names to the read data
  read <- dplyr::left_join(data, name, by = "barcode") 
  read$seq <- seq
  read$pool <- substr(filename1, 6,7)
  read$barcode <- NULL
  read$percent_retained <- read$retained/read$total_reads
  # add the finished product to the running tally
  reads <- rbind(reads, read) 
}

# read in the old data
old <- read.csv("data/APCL_read_data.csv", stringsAsFactors = F)

# # get rid of extra characters in ligation_ids
# # first eliminate the .Fs
# old$ligation_id <- gsub('|.F', '', old$ligation_id)
# # get rid of all the numbers between APCL and L that are in dDocent format
# old$ligation_id <- gsub('APCL_[0-9]{5}L', 'APCL_L', old$ligation_id, perl=TRUE)
# # get rid of all the numbers between APCL and L that are in regular format
# old$ligation_id <- gsub('APCL[0-9]{2}_[0-9]{3}L', 'APCL_L', old$ligation_id, perl=TRUE)
# # get rid of the APCL
# old$ligation_id <- gsub('APCL_|', '', old$ligation_id, perl=TRUE)


# add new data
old <- rbind(old, reads)

# write to file to keep running tally
write.csv(old, "data/APCL_read_data.csv", row.names = F)

# analyze the read data
plot(x = old$total_reads, y = old$percent_retained)

# the above plot shows that most of our samples retain the majority of their reads.
# Let's make a histogram of total reads to see where our low end performers are
brk <- seq(0,10000000,100000)
x <- hist(old$total_reads, breaks = brk)


# I want to take a look at the low performers
plot(x = old$total_reads, y = old$percent_retained, type = "p", xlim = c(0,50000))

# I want a list of samples where total reads are less than 25,000
suppressMessages(library(dplyr))
old_low_tot <- old %>% filter(total_reads < 50000)

# These samples need to be evaluated to see if they have been genotyped in the past or if they need to be regenotyped
write.csv(old_low_tot, file = "data/low_performers.csv", row.names = F)
