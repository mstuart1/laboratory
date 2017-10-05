# lab helpers - helper functions for lab work

# read_db ####
#' views all of the fish recaptured at a given site
#' @export
#' @name read_db
#' @author Michelle Stuart
#' @param x = which db?
#' @examples 
#' db <- read_Db("Leyte")

read_db <- function(x){
  library(dplyr)
  db <- src_mysql(dbname = x, default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
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

write_db <- function(x){
  library(RMySQL)
  db <- dbConnect(MySQL(), dbname = x, default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
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

plate_from_db <- function(x, y){
  # split the well out into row and column 
  x$row <- substr(x$well, 1, 1)
  x$col <- as.numeric(substr(x$well, 2, 3))
  
  # select columns for plate 
  if (y == "extraction_id"){
    x <- x %>% 
      select(row, col, extraction_id) %>% #keep row & col, identifier
      arrange(row, col)
  }
  if (y == "sample_id"){
    x <- x %>% 
      select(row, col, sample_id) %>% #keep row & col, identifier
      arrange(row, col)
  }
  if (y == "digest_id"){
    x <- x %>% 
      select(row, col, digest_id) %>% #keep row & col, identifier
      arrange(row, col)
  }
  if (y == "ligation_id"){
    x <- x %>% 
      select(row, col, ligation_id) %>% #keep row & col, identifier
      arrange(row, col)
  }

  x <- as.data.frame(x)
  
  # make map
  platemap <<- as.matrix(reshape2::acast(x,x[,1] ~ x[,2]))
  return(x)
}











# make_plate ####
#' access db with intent to change it
#' @export
#' @name make_plate
#' @author Michelle Stuart
#' @param x = the table of samples to turn into a plate
#' @examples 
#' table <- make_plate(todo)

# make_plate <- function(x){  ########## IN PROGRESS ################
#   # how many rows are in the table (how many samples)?
#   y <- nrow(x)
#   
#   # how many plates would these make, 94 samples plus 2 blanks per plate
#   (nplates <- floor(y/94)) # extra parenthesis are to print
#   
#   # define wells
#   well <- 1:(96*nplates)
#   
#   # separate list of samples out into plates
#   
#   # insert the negative controls
#   a <- (nrow(todo)+1)
#   todo[a, ] <- "XXXX"
#   
#   extr <- data.frame()
#   for (i in 1:nplates){
#     c <- 94*i-93 # well 1 on a plate
#     d <- 94*i-83 # 11
#     e <- 94*i-82 # 12 negative control well
#     f <- 94*i-81 # 13
#     g <- 94*i-34 # 60
#     h <- 94*i-33 # 61 negative control well
#     j <- 94*i-32 # 62
#     k <- 94*i + 2 # 96
#     l <- 94*i - 35 # 59
#     m <- 94 * i #94
#     str1 <- as.data.frame(cbind(well[c:d], todo[c:d,])) # 1:11
#     names(str1) <- c("well", "sample_id")
#     str2 <- as.data.frame(cbind(well[e], todo[a,])) # because the first blank is in the 12th position
#     names(str2) <- c("well", "sample_id")
#     str3 <- as.data.frame(cbind(well[f:g], todo[e:l,])) #13:60 in plate, 12:59 in list
#     names(str3) <- c("well", "sample_id")
#     str4 <- as.data.frame(cbind(well[h], todo[a,])) # because the 2nd blank is in the 61st position
#     names(str4) <- c("well", "sample_id")
#     str5 <- as.data.frame(cbind(well[j:k], todo[g:m,]))# 62:96 in plate, 60:94 in list
#     names(str5) <- c("well", "sample_id")
#     
#     # and stick all of the rows together
#     temp <- data.frame(rbind(str1, str2, str3, str4, str5))
#     temp$Row <- rep(LETTERS[1:8], 12)
#     temp$Col <- unlist(lapply(1:12, rep, 8))
#     temp$plate <- paste("plate", i, sep = "")
#     extr <- rbind(extr, temp)
#     
#   }
#   
# }
