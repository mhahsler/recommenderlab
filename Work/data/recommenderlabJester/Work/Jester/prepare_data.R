library(recommenderlab)


J1 <- read.csv("jester-data-1.csv", header=FALSE)
dim(J1)
J1[J1 == 99] <- NA

#J2 <- read.csv("jester-data-2.csv", header=FALSE)
#dim(J2)
#J2[J2 == 99] <- NA

#J3 <- read.csv("jester-data-3.csv", header=FALSE)
#dim(J3)
#J3[J3 == 99] <- NA

### first row is number of votes
Jester <- J1[,-1]
#Jester <- rbind(J1,J2,J3)


#Jester <- as(as.matrix(Jester+11), "realRatingMatrix")
Jester <- as(as.matrix(Jester), "realRatingMatrix")
colnames(Jester) <- paste('j',1:ncol(Jester),sep='')
rownames(Jester) <- paste('u',1:nrow(Jester),sep='')



### read jokes "init<nr>.html"
jokes <- lapply(1:100, function(i) readLines(paste("jokes/init",i,".html", sep="")))

# get rid of first 5 lines (title)
jokes <- lapply(jokes, "[", -c(1:5))

filter_tags <- function(txt) gsub("<[^>]*>", "", txt)
filter_nbsp <- function(txt) gsub("&nbsp;", "", txt)
collapse <- function(txt) {
  txt <- paste(txt, collapse = " ")
  txt <- gsub("^\\s+|\\s+$", "", txt)
  txt <- gsub("\\s+", " ", txt)
  txt
}
jokes <- lapply(jokes, filter_tags)
jokes <- lapply(jokes, filter_nbsp)
jokes <- sapply(jokes, collapse)
names(jokes) <- paste('j',1:length(jokes),sep='')

JesterJokes <- jokes

save(Jester, JesterJokes, file="Jester.rda")


### a 5k sample of Jester
set.seed(1234)
Jester5k <- sample(Jester, 5000)

save(Jester5k, JesterJokes, file="Jester5k.rda")
