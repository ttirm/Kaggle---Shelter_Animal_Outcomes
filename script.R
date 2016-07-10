#Animal Sherter Outcome
setwd("C:/Users/tiago_000/Documents/GitHub/Kaggle - Shelter_Animal_Outcomes")



train <- read.csv("./data/train.csv", na.strings = c("", "Unknown"))
str(train)

train$SexuponOutcome <- as.character(train$SexuponOutcome)

sum(is.na(train$SexuponOutcome))

#Distinguish animals with and without name
train$Hasname <- !is.na(train$Name)
head(train)

isna <- apply(train, 2, function(x){sum(is.na(x))})



#impute strategy - sex
unique(train$SexuponOutcome)
table(train$SexuponOutcome)
table(train$OutcomeSubtype, train$OutcomeType)
#Most of the animals with name have been neutered
table(train$Hasname, train$SexuponOutcome)
#table(train$MultiCol, train$SexuponOutcome)


#Create Name table
#.....................
t <- train[which(!is.na(train$SexuponOutcome) & train$Hasname == TRUE),]
res <- apply(as.matrix(t$SexuponOutcome), 1, function(x) {grepl("Male", x)})
t$Sex <- as.data.frame(res)$res
length(t$Sex)
length(t$Name)
tab <- as.matrix(table(t$Name, t$Sex))
head(tab)
ds <- data.frame(names = row.names(tab), male = tab[,2] > tab[,1])
#.........................


#Impute function to define sex
#......................................
impute1 <- function(x, ds){
    
    index <-grep(paste0("^",trimws(as.character(x)),"$"), ds$names)
    if(length(index) == 0){
        sex0 <- sample(c("Male", "Female"),1, replace = TRUE, prob = c(0.5,0.5))
        int <- "Intact"
        #print(index)
    }else{

        if(ds[as.numeric(index),2] == TRUE){

            sex0 <- "Male"
            int <- "Neutered"
        }else if(ds[as.numeric(index),2] == FALSE){
            sex0 <- "Female"
            int <- "Neutered"
        }


    }
    result <- paste(int, sex0)
    
}
t1 <- train[is.na(train$SexuponOutcome),]
res <-sapply(t1$Name, impute1, ds = ds)
train[is.na(train$SexuponOutcome),"SexuponOutcome"] <- unlist(res)

#There is no NAs in SexuponOutcome
sum(is.na(train$SexuponOutcome))

#Create two new columns; one for Male/Female; One for Intact/Neutered
res <- apply(as.matrix(train$SexuponOutcome), 1, function(x) {grepl("Male", x)})
train$Sex <- as.data.frame(res)$res
head(train$Sex)
res <- apply(as.matrix(train$SexuponOutcome), 1, function(x) {grepl("Intact", x)})
train$Intact <- as.data.frame(res)$res
head(train)



#Distinguish animals with just one color
res <- apply(as.matrix(train$Color), 1, function(x) {grepl("/", x)})
train$MultiCol <- as.data.frame(res)$res
head(train)


#Distinguish pure bred animals from mixed bred animals
res <- apply(as.matrix(train$Breed), 1, function(x) {!grepl("/|Mix", x)})
train$Pure <- as.data.frame(res)$res
head(train)
table(train$Pure, train$OutcomeType)


#Subtype evalutation
str(train$OutcomeSubtype)
summary(train$OutcomeSubtype)
# More than 50% of the results don't have an outcome sub type
sum(is.na(train$OutcomeSubtype))/length(train$OutcomeSubtype)*100



train[is.na(train$AgeuponOutcome),]
train$AgeuponOutcome
grep("[0-9]+ ", x)
x <- train$AgeuponOutcome[1]
values <- train$AgeuponOutcome[!is.na(train$AgeuponOutcome)]
res <- apply(as.matrix(values), 1, function(x) {trimws(substr(x, grep("[0-9] ", x)+2, nchar(as.character(x))))})
mult <- substr(res, grep(" ", res)+1, nchar(as.character(res)))
unique(res)
mult_times <- function(x){
    if(x == "year" || x == "years"){
        mult <- 365
        
    }else if (x == "month" || x == "months"){
        mult <- 30
        
    }else if (x == "week" || x == "weeks"){
        mult <- 7
        
    }else if (x == "day" || x == "days"){
        mult <- 1
    }
}


mult <- sapply(res, mult_times)


res <- apply(as.matrix(values), 1, function(x) {trimws(substr(x, grep("[0-9]", x), grep(" ", x)))})
num <- substr(res, grep("[0-9]", x), grep(" ", x))
num <- as.numeric(res)
num[num == 0] <- 0.5

train$Days <- num*mult
unique(res)
unique(num)





