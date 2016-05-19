library("recommenderlab")

tab <- scan("anonymous-msweb.data", what = "character", sep="\n", quote="\"")

tab <- sapply(tab, strsplit, ",")


##A,1288,1,"library","/library"
##C,"10017",10017
##V,1027,1

trans <- list()
items <- integer()
cust <- NULL
t <- 1
id_to_name <- list()

for(i in 1:length(tab)) {
    if(tab[[i]][1] == "C")  {
        if(length(items)>0) {
            trans[[t]] <- items
            t <- t+1
        }
        items <- integer()
        cust <- as.integer(tab[[i]][3])
    }
    
    if(tab[[i]][1] == "A") {
        id_to_name[[tab[[i]][2]]] <- tab[[i]][4]   
    }

    if(tab[[i]][1] == "V") {
        items <- c(items, as.integer(tab[[i]][2]))
    }
}


db <- new("binaryRatingMatrix", data = as(trans, "itemMatrix"))

## add labels
ids <- itemInfo(db@data)$labels
#ids <- colnames(db)
id_to_name <- unlist(id_to_name)
## fix 2 occurences of "MS Project"
id_to_name["1169"] <- "MS Project 2"
labels <- id_to_name[match(ids, names(id_to_name))]

colnames(db) <- labels
#itemInfo(db@data)$labels <- labels

#db <- db[size(db)>=5]

MSWeb <- db

save(MSWeb, file = "MSWeb.rda")


