library("recommenderlab")


## read db (3+ stars is good)
data <- read.table("ml-data/u.data",
    col.names=c("user", "item", "rating", "time"))


db <- as(data, "realRatingMatrix")

## add movie labels
genres <- read.table("ml-data/u.genre", sep ="|", quote="\"", stringsAsFactors = TRUE)
colnames(genres) <- c("genre", "id")
summary(genres)

movies <- read.table("ml-data/u.item", sep ="|", quote="\"", stringsAsFactors = TRUE)
colnames(movies) <- c("id", "title", "year", "NA", "url",
as.character(genres$genre))
summary(movies)

users <- read.table("ml-data/u.user", sep ="|", quote="\"", stringsAsFactors = TRUE)
colnames(users) <- c("id", "age", "sex", "occupation", "zipcode")
summary(users)

m <- match(colnames(db), movies$id)
movies <- movies[m,]
ilabels <- movies$title
gen <- movies[c(2,3,5,6:24)]

## clean
gen[,"title"] <- as.character(gen[,"title"])
gen[,"url"] <- as.character(gen[,"url"])

gen[,"year"] <- as.Date(gen[,"year"], "%d-%b-%Y")
gen[,"year"] <- as.numeric(format(gen[,"year"],'%Y'))

## remove duplicated movies
dup <- which(duplicated(ilabels))

# movies$name[dup]

ilabels <- ilabels[-dup]
db <- db[,-dup]
gen <- gen[-dup,]

colnames(db) <- ilabels

#itemInfo(db) <- cbind(itemInfo(db), gen)


MovieLense <- db
MovieLense

MovieLenseMeta <- gen
summary(MovieLenseMeta)

MovieLenseUser <- users
summary(MovieLenseUser)

save(MovieLense, MovieLenseMeta, MovieLenseUser, file = "MovieLense.rda", compress = "bzip2", compression_level = 9)

