# lab helpers - helper functions for lab work

# read_db ####
#' views all of the fish recaptured at a given site
#' @export
#' @name read_db
#' @author Michelle Stuart
#' @param x = which db?
#' @examples 
#' db <- read_Db("Leyte")

read_db <- function(db_name){
  library(dplyr)
  db <- src_mysql(dbname = db_name, default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  return(db)
}

# write_db ####
#' access db with intent to change it
#' @export
#' @name write_db
#' @author Michelle Stuart
#' @param x = which db?
#' @examples 
#' db <- write_db("Leyte")

write_db <- function(db_name){
  library(RMySQL)
  db <- dbConnect(MySQL(), dbname = db_name, default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  return(db)
}


# plate_from_db ####
#' recreate a plate map from info in the db
#' @export
#' @name plate_from_db
#' @author Michelle Stuart
#' @param x = the table of samples to turn into a plate
#' @param y = the sample identifier you want to use
#' @examples 
#' plate <- plate_from_db(todo, extraction_id)

plate_from_db <- function(table_name, id_type){
  # split the well out into row and column 
  table_name$row <- substr(table_name$well, 1, 1)
  table_name$col <- as.numeric(substr(table_name$well, 2, 3))
  
  # select columns for plate 
  if (id_type == "extraction_id"){
    table_name <- table_name %>% 
      select(row, col, id_type) %>% #keep row & col, identifier
      arrange(row, col)
  }
  if (id_type == "sample_id"){
    table_name <- table_name %>% 
      select(row, col, sample_id) %>% #keep row & col, identifier
      arrange(row, col)
  }
  if (id_type == "digest_id"){
    table_name <- table_name %>% 
      select(row, col, digest_id) %>% #keep row & col, identifier
      arrange(row, col)
  }
  if (id_type == "ligation_id"){
    table_name <- table_name %>% 
      select(row, col, ligation_id) %>% #keep row & col, identifier
      arrange(row, col)
  }

  table_name <- as.data.frame(table_name)
  
  # make map
  platemap <<- as.matrix(reshape2::acast(table_name,table_name[,1] ~ table_name[,2]))
  return(table_name)
}


# make_plate_with_negs ####
#' turn a table into plates with negative controls
#' @export
#' @name make_plate_with_negs
#' @author Michelle Stuart
#' @param x = list of ids
#' @param y = id_type
#' @examples 
#' plate <- make_plate_with_negs(todo)

make_plate_with_negs <- function(list_of_ids, id_type){
  # make a dataframe of the list_of_ids
  list_of_ids <- as.data.frame(list_of_ids)
  
  # how many rows are in the table (how many samples)?
  y <- nrow(list_of_ids)

  # how many plates would these make, 94 samples plus 2 blanks per plate
  (nplates <- floor(y/94)) # extra parenthesis are to print

  # define wells
  well <- 1:(96*nplates)

  # insert the negative controls and set up the plate
  plate <- data.frame() # blank data frame to build upon
  for (i in 1:nplates){
    c <- 94*i-93 # well 1 on a plate
    d <- 94*i-83 # 11
    e <- 94*i-82 # 12 negative control well
    f <- 94*i-81 # 13
    g <- 94*i-34 # 60
    h <- 94*i-33 # 61 negative control well
    j <- 94*i-32 # 62
    k <- 94*i + 2 # 96
    l <- 94*i - 35 # 59
    m <- 94 * i #94
    str1 <- as.data.frame(cbind(well[c:d], list_of_ids[c:d,])) # 1:11
    names(str1) <- c("well", "id_type")
    str2 <- as.data.frame(cbind(well[e], "XXXX")) # because the first blank is in the 12th position
    names(str2) <- c("well", "id_type")
    str3 <- as.data.frame(cbind(well[f:g], list_of_ids[e:l,])) #13:60 in plate, 12:59 in list
    names(str3) <- c("well", "id_type")
    str4 <- as.data.frame(cbind(well[h], "XXXX")) # because the 2nd blank is in the 61st position
    names(str4) <- c("well", "id_type")
    str5 <- as.data.frame(cbind(well[j:k], list_of_ids[g:m,]))# 62:96 in plate, 60:94 in list
    names(str5) <- c("well", "id_type")
    
    # and stick all of the rows together
    temp <- data.frame(rbind(str1, str2, str3, str4, str5))
    temp$Row <- rep(LETTERS[1:8], 12)
    temp$Col <- unlist(lapply(1:12, rep, 8))
    temp$plate <- paste("plate", i, sep = "")
    plate <- rbind(plate, temp)
    
  }
  
  # put the samples in order of id (with negative controls inserted)
  plate <- arrange(plate, plate, Col, Row)
  
  return(plate)

  

}
