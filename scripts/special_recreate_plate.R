# special - recreate a list of samples and plate locations from a platemap csv

map <- "data/redigest2_map.csv" # this is actually redigest plate 2

sheet <- read.csv(map)

# rename the columns
colnames(sheet) <- c("Row", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
# turn the info into table format
data <- reshape2::melt(sheet, id.vars = "Row", variable.name = "Col", value.name = "Sample")

# remove empty wells
data <- data %>% 
  filter(!is.na(Sample), Sample != "") %>% 
  rename(extraction_id = Sample) %>% 
  mutate(plate = "redigest2")

redigest <- data
