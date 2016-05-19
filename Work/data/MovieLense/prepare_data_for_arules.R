library("recommenderlab")


## read db (3+ stars is good)
data <- read.table("ml-data/u.data", 
    col.names=c("user", "item", "rating", "time"))


db <- as(data, "ratingMatrix")

## add movie labels
movies <- read.table("ml-data/u.item", sep ="|", quote="\"")
genres <- read.table("ml-data/u.genre", sep ="|", quote="\"")
colnames(genres) <- c("genre", "id")

colnames(movies) <- c("id", "name", "date", "NA", "URL", 
    as.character(genres$genre))


m <- match(itemLabels(db), movies$id)
movies <- movies[m,]
ilabels <- movies$name
gen <- movies[6:24]

## remove duplicated movies
dup <- which(duplicated(ilabels))

# movies$name[dup]

ilabels <- ilabels[-dup]
db <- db[,-dup]
gen <- gen[-dup,]

itemLabels(db) <- ilabels

itemInfo(db) <- cbind(itemInfo(db), gen)

dim(db)

rm(movies, ilabels, m, dup)

MovieLenseBin <- as(db, "itemMatrix")

save(MovieLenseBin, file = "MovieLenseBin.rda")

