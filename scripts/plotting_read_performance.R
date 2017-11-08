# analyze the read data

source("scripts/lab_helpers.R")

lab <- read_db("Laboratory")
lig <- lab %>% 
  tbl("ligation") %>% 
  filter(!is.na(total_reads)) %>% 
  mutate(percent_retained = retained/total_reads) %>% 
  collect()

plot(x = lig$total_reads, y = lig$percent_retained)

# the above plot shows that most of our samples retain the majority of their reads.
# Let's make a histogram of total reads to see where our low end performers are
brk <- seq(0,10000000,100000)
x <- hist(as.numeric(lig$total_reads), breaks = brk)
x <- hist(as.numeric(lig$retained), breaks = brk)

# I want to take a look at the low performers
plot(x = lig$total_reads, y = lig$percent_retained, type = "p", xlim = c(0,50000))

# I want a list of samples where total reads are less than threshold
suppressMessages(library(dplyr))
low_tot <- lig %>% filter(total_reads < 20000)

# These samples need to be evaluated to see if they have been genotyped in the past or if they need to be regenotyped
# write.csv(low_tot, file = "data/low_performers.csv", row.names = F)
