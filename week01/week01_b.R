
# Pivotiranje
# stack/unstack u python
# https://towardsdatascience.com/reshaping-a-dataframe-with-pandas-stack-and-unstack-925dc9ce1289#:~:text=Pandas%20provides%20various%20built%2Din,s)%20from%20row%20to%20column.
library(tidyverse)

DATA_DIR = paste0(getwd(), '/_data/')
CSV_FILE = paste0(DATA_DIR, 'airbnb.csv')

# - read listings.csv.gz for AirBnB
# - from: http://insideairbnb.com/get-the-data/
# data_url <- "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2022-03-08/data/listings.csv.gz"
# con <- gzcon(url(data_url))
# txt <- readLines(con)
# data_set <- read.csv(textConnection(txt))
# 
# write.csv(data_set, CSV_FILE)

data_set <- read.csv(CSV_FILE,
                     header = TRUE,
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
# rm(list = ls())


# str(data_set)
glimpse(data_set)


data_set$host_response_time[data_set$host_response_rate == "N/A"] <- NA
data_subset <- dplyr::select(data_set, id, name, host_response_time)

table(data_subset$host_response_time)
summary(data_subset)


as.data.frame(
  table(data_set$host_response_time,
        data_set$host_is_superhost)
)


data_subset <- dplyr::select(data_set, id, name, host_response_time)

dplyr::filter(data_subset, host_response_time == 'within an hour')


data_set %>% 
  dplyr::select(id, name, host_response_time) %>%
  dplyr::filter(host_response_time == 'within an hour') -> data_subset


data_set %>% 
  dplyr::select(host_response_time) %>%
  dplyr::group_by(host_response_time) %>%  summarise(count = n()) -> data_subset



# Sesija 2, sa sajta datakolektiva, poglavlje 3.2
# Knjiga o R, poglavlja 2, 3, 4, 5, 6, 7