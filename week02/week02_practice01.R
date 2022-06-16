library(tidyverse)

WORK_DIR = getwd()
DATA_DIR = paste0(WORK_DIR, '/_data')

data_frame = read.csv(paste0(DATA_DIR, '/Fish.csv'), 
                      header = TRUE, 
                      check.names = FALSE, 
                      stringsAsFactors = FALSE)

glimpse(data_frame)

lapply(data_frame, function(x) is.na(x)) %>% lapply(sum) %>% cbind()

# to select only complete rows
# complete(data_frame)

summary(data_frame)

data_frame.longer <- pivot_longer(data_frame, 
             cols = c(Weight, 
                      Length1,
                      Length2,
                      Length3, 
                      Height, 
                      Width), 
             names_to = "Attribute", values_to = "Values") %>%  as.data.frame()


l <- length(unique(iris$Species))

pairs(~ Weight+Length1+Length2+Length3+Height+Width,
      data = data_frame, 
      col = hcl.colors(l, "Temps")[iris$Species])


data_frame.longer %>%
  ggplot(mapping=aes(x=Values, fill=Species)) +
  geom_boxplot() +
  facet_wrap(Attribute ~ ., scales="free" )

select(data_frame, -(Weight: Species))

ggplot(data=data_frame, mapping=aes(x=Weight, y=Length1)) +
  geom_point() +
  facet_wrap(Species ~ ., scale="free")



data.frame(NO=1:10, VAL=seq(1:10)^2^0.3) %>%
  ggplot(mapping=aes(x=NO, y=VAL)) +
  geom_point()
