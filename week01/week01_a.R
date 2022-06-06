# install.packages('tidyverse')
library(tidyverse)

# current directory
getwd()

# dir tree
data_dir <- paste0(getwd(), '/_data/')
analytics_dir <- paste0(getwd(), '/_analytics/')
reporting_dir <- paste0(getwd(), '/_reporting/')
img_dir <- paste0(getwd(), '/_img/')

# always use RStudio projects
list.files(getwd())


# remove all objects
# rm(list = ls())

# garbage collect
# gc()

w1 <- "Hello"
w2 <- 'World'
hw <- c(w1, w2)
hw[1]
print(c(w1, w2))

a <- c(1, 2, 3)
class(a)
a <- 5
class(a)
v2 <- c("a", "b")
class(v2)


nv <- 1:10
nv1 <- 11:20

nv2 <- nv + nv1 

as.integer(3.6)

is.integer(3.14)


# varijansa, deljenje sa n-1, zbog stepeni slobode, kada postoji srednja vrednost, 
# onda jedna od vrednosti uzorka moze da se izostavi, to znaci da broj posmatranja mozemo
# da smanjimo za 1, odatle n - 1

v1 <- c(1, 7, 8, 2, 11, 14, 22)
mean(v1)
var(v1)
sse <- numeric()
for(i in 1:length(v1)) {
  sse[i] <- v1[i] - mean(v1)
}
sse <- sum(sse^2)
sse <- sse/(length(v1)-1)
sse
var(v1)


v1 <- c(1, 7, 8, 2, 11, NA, 22)
var(v1)
var(v1, na.rm = TRUE)

v1 > 5
which(v1 > 5)
w <- which(v1 > 5)
w
v1[w]

v1[w] <- NA
v1

head(v1)

v1 <- c(1, 7, 8, 2, 11,  22)
find5 <- v1 > 5
as.numeric(find5)
sum(as.numeric(find5))


a <- "This is a sentence in English"
a_split <- strsplit(a, split = " ")
a_split
class(a_split)
length(a_split)
length(a_split[[1]])


a
length(a)
nchar(a)


sum2 <- function(x, y) {
  s <- x + y
  return(s)
}

sum2(5, 6)


# - more functions 
a <- list(name = c("George", "Maria"),
          age = c(48, 42))
b <- list(name = c("Marko", "Nataša"),
          age = c(51, 41))

cmp_couples <- function(l1, l2) {
  if (l1$age[1] > l2$age[1]) {
    output1 <- paste0(l1$name[1], " is older than ", l2$name[1])
  } else {
    output1 <- paste0(l1$name[1], " is not older than ", l2$name[1])
  }
  if (l1$age[2] > l2$age[2]) {
    output2 <- paste0(l1$name[2], " is older than ", l2$name[2])
  } else {
    output2 <- paste0(l1$name[2], " is not older than ", l2$name[2])
  }
  return(list(output1, output2))
}


a <- list("Belgrade", 2022, TRUE)
class(a)
length(a)
a[[1]]
a[[2]]
a[[3]]
class(a[[1]])
class(a[[2]])
class(a[[3]])
# - apply a function over a list
lapply(a, class)



# - data.frame
num <- c(1, 2, 3, 4)
city <- c("Paris", "Belgrade", "NYC", "Tokyo")
timezone <- c("CET", "CET", "EDT", "JST")
population <- c(2.23, 1.4, 8.83, 14)
cities <- data.frame(no = num,
                     city = city,
                     tz = timezone,
                     pop = population)


cities
str(cities)

cities$city

cities[1,]
cities[1:2, ]
cities[, 1]
cities[, 1:2]

cities[which(cities$pop > 3), ]

dim(cities)[1]

cities$score = c(2,3,6,4)
cities

cities$score <-NULL
cities


head(cities, 2)
tail(cities, 2)


names(cities)
colnames(cities)[1] <- 'redni broj'
cities

cities[1:2, c('city', 'pop')]
cities[, 'pop']
cities$tz <- paste0('tz_', cities$tz)


print(mtcars)
summary(mtcars)
plot(mtcars$mpg)


colnames(mtcars)
colnames(mtcars)[1]

rownames(mtcars)
mtcars[1:2, c('mpg', 'wt')]


rm(list = ls())

# - the data are found in the /_data directory
# - relative to the project path:
getwd()
list.dirs()
data_dir <- paste0(getwd(), "/_data/")
# - now we have the _data directory path

# - read.csv() to read in a .csv file format
# - into an R data.frame
air_quality_data <- read.csv(paste0(data_dir, "AirQualityUCI.csv"), 
                             header = TRUE,
                             sep = ";",
                             check.names = FALSE, 
                             stringsAsFactors = FALSE)
# - sep = ";" - to be discussed in the classroom
head(air_quality_data, 10)

# let's load another one (a pure `.csv`)
# - there is an in-built mtcars dataset, look:
head(mtcars)
# - write.csv mtcars to our data_dir
write.csv(mtcars, 
          paste0(data_dir, "mtcars.csv"))
# - load mtcars.csv into a data.frame
mtcats_data <- read.csv(paste0(data_dir, "AirQualityUCI.csv"),
                        header = TRUE,
                        sep = ";",
                        check.names = FALSE,
                        stringsAsFactors = FALSE)
# - sep = ";" - to be discussed in the classroom
head(air_quality_data, 10)

# - load several .csv files and put them together 
# - in one data.frame
# - first, let's produce the data
data_sets <- list() 
for (i in 1:4) {
  dat <- data.frame(measure_A = runif(100, 0, 100),
                    measure_B = runif(100, 0, 500), 
                    code = sample(letters, 100, replace = TRUE), 
                    month = sample(month.name, 100, replace = TRUE),
                    stringsAsFactors = FALSE)
  data_sets[[i]] <- dat
}
# - inspect
data_sets[[1]]
# - write all elements of data_sets as separate .csv files
lapply(data_sets, function(x) {
  filename = paste0("data_chunk_", round(runif(1, 1, 100), 0), ".csv")
  write.csv(x, paste0(data_dir, "/", filename))
})
# read; first we need to recognize the files that we need
lF <- list.files(data_dir)
lF <- lF[grepl("data_chunk", lF)]
print(lF)
collect_set <- lapply(lF, function(x) {
  read.csv(paste0(data_dir, x), 
           header = TRUE, 
           check.names = FALSE,
           row.names = 1,
           stringsAsFactors = FALSE)
})
# - check
length(collect_set)
class(collect_set)
collect_set[[1]]
# - put them together
final_data_set <- Reduce(rbind, collect_set)
head(final_data_set, 50)

### DATA TABLE U R


