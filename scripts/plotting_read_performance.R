# analyze the read data

source("scripts/lab_helpers.R")
library(ggplot2)

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

# plot ng used vs. num_loci passed dDocent - need genepop
num_loci <- readRDS("data/num_loci.Rdata")

dat <- left_join(lig, num_loci, by = "ligation_id")
dat <- dat %>% 
  select(ligation_id, sample_id, DNA, numloci, total_reads, retained, percent_retained) %>% 
  mutate(numloci = ifelse(is.na(numloci), 0, numloci))

ggplot(dat)+
  geom_point(mapping = aes(x=DNA, y=numloci)) # not very helpful

ggplot(dat, mapping = aes(x = DNA, y = numloci)) +
  geom_violin()

ggplot(dat, mapping = aes(group = DNA, x = DNA, y = numloci)) +
  geom_boxplot()

dat2 <- dat %>% 
  group_by(DNA)

saveRDS(dat, file = "data/plot_data.Rdata")

ggplot(dat, aes(x=DNA, y=numloci))+ 
geom_point() + 
geom_smooth(color="red")


tab <- as.matrix(table(dat$DNA, dat$numloci>800))
rs <- rowSums(tab)
tab/rs
plot(as.numeric(rownames(tab)), (tab/rs)[,2])
