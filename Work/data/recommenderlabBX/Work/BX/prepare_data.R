library(recommenderlab)

set.seed(1234)

BX <- read.csv("BX-Book-Ratings.csv", header=TRUE, sep=';')
BX <- as(BX, "realRatingMatrix")

save(BX, file="BX.rda")

## remove non ascii characters
## perl -i.bak -pe 's/[^[:ascii:]]//g' BX-Books_clean.csv

BXBooks <- read.csv("BX-Books_clean.csv", header=TRUE, sep=';')
BXBooks <- BXBooks[,1:5]

BXBooks[,1] <- as.character(BXBooks[,1])
BXBooks[,2] <- as.character(BXBooks[,2])
BXBooks[,3] <- as.character(BXBooks[,3])
BXBooks[,4] <- as.integer(as.character(BXBooks[,4]))
BXBooks[BXBooks[,4]==0,4] <- NA

save(BXBooks, file="BXBooks.rda")

#BXUsers <- read.csv("BX-Users.csv", header=TRUE, sep=';')
#save(BXUsers, file="BXUsers.rda")

