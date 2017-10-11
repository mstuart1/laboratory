# A script to create a csv for the Biomek from a list of ligation numbers
source("scripts/lab_helpers.R")

# connect to db
lab <- read_db("Laboratory")

# get extract numbers for all of these - why?  That doesn't make sense

# get ligations for a specific date
ligs <- lab %>% 
  tbl("ligation") %>% 
  filter(date == "2016-04-28") %>% 
  select(ligation_id, digest_id) %>% 
  collect()

# make a platemap_list of the destination ligations
plate <- data.frame( Row = rep(LETTERS[1:8], 12), Col = unlist(lapply(1:12, rep, 8)))

dest <- cbind(plate, allmeta[1:96, c("ligation_ID", "digest_ID", "quant")])
dest  <- dest %>% mutate(dnavol = 200/quant)
dest <- dest %>% mutate(watervol = 22.2-dnavol)

dest$welldest <- paste(dest$Row, dest$Col, sep = "")

# clean up 
dest$Row <- NULL
dest$Col <- NULL
dest$quant <- NULL
dest$destloc <- "P11"
dest <- dest[ , c(2,3,4,1,6,5)]

# add source locations

# digests from the date we want
digest <- data.frame(labor %>% tbl("digest") %>% filter(date == '2016-04-04'), stringsAsFactors = F)

# the specific digest plate
source1 <- cbind(plate, digest[97:192,1])
names(source1) <- c("Row", "Col", "ID")
first <- source1$ID[1]
last <- source1$ID[nrow(source1)]
write.csv(source1, file = paste(first, "-", last, "_list.csv", sep = ""))
source1$ID <- as.character(source1$ID)
platemap <- as.matrix(reshape2::acast(source1, source1[,1] ~ source1[,2]))
write.csv(platemap, file = paste(first, "-",last, "_map.csv"))

final <- merge(dest, source1, by.x = "digest_ID", by.y = "ID", all.x = T)
final$wellsource <- paste(final$Row, final$Col, sep = "")

# clean up
final$Row <- NULL
final$Col <- NULL

# split out digests by plate

source1 <- final[which(final$wellsource != "NANA"), ]

remaining <- final[which(final$wellsource == "NANA"), ]

# next digests from the date we want
digest <- data.frame(labor %>% tbl("digest") %>% filter(date == '2016-04-25'), stringsAsFactors = F)

source2 <- cbind(plate, digest[1:96,1])
names(source2) <- c("Row", "Col", "ID")
first <- source2$ID[1]
last <- source2$ID[nrow(source2)]
write.csv(source2, file = paste(first, "-", last, "_list.csv", sep = ""))
source2$ID <- as.character(source2$ID)
platemap <- as.matrix(reshape2::acast(source2,source2[,1] ~ source2[,2]))
write.csv(platemap, file = paste(first, "-",last, "_map.csv"))

final <- merge(remaining, source2, by.x = "digest_ID", by.y = "ID", all.x = T)
final$wellsource <- paste(final$Row, final$Col, sep = "")


# clean up
final$Row <- NULL
final$Col <- NULL

# split out digests by plate

source2 <- final[which(final$wellsource != "NANA"), ]

remaining <- final[which(final$wellsource == "NANA"), ]

# next digests from the date we want
digest <- data.frame(labor %>% tbl("digest") %>% filter(date == '2016-04-25'), stringsAsFactors = F)

source3 <- cbind(plate, digest[97:192,1])
names(source3) <- c("Row", "Col", "ID")
source3 <- source3[!is.na(source3$ID), ]
first <- source3$ID[1]
last <- source3$ID[nrow(source3)]
write.csv(source3, file = paste(first, "-", last, "_list.csv", sep = ""))
source3$ID <- as.character(source3$ID)
platemap <- as.matrix(reshape2::acast(source3,source3[,1] ~ source3[,2]))
write.csv(platemap, file = paste(first, "-",last, "_map.csv"))

final <- merge(remaining, source3, by.x = "digest_ID", by.y = "ID", all.x = T)
final$wellsource <- paste(final$Row, final$Col, sep = "")


# clean up
final$Row <- NULL
final$Col <- NULL

# split out digests by plate

source3 <- final[which(final$wellsource != "NANA"), ]

remaining <- final[which(final$wellsource == "NANA"), ]

# add source positions to each of the source plates
source1$sourceloc <- "P9"
source2$sourceloc <- "P10"
source3$sourceloc <- "P5"

# merge the source files together
ultimate <- rbind(source1, source2, source3)
ultimate$dnavol <- round(ultimate$dnavol, 2)
ultimate$watervol <- round(ultimate$watervol, 2)


# pull out the water information
water <- ultimate
water$wellsource <- "A1"
water$sourceloc <- "P12"

write.csv(ultimate, file = paste(Sys.Date(), "biomek.csv", sep = ""))

write.csv(water, file = paste(Sys.Date(), "water.csv", sep = ""))

penultimate <- rbind(ultimate, water)
penultimate <- penultimate[order(penultimate$digest_ID), ]

write.csv(penultimate, file = paste(Sys.Date(), "combo.csv", sep = ""))
