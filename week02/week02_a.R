#load module
library(tidyverse)

WORK_DIR = getwd()
DATA_DIR = paste0(WORK_DIR, '/_data')

fish_data <- read.csv(paste0(DATA_DIR, '/Fish.csv'), 
                      check.names = FALSE, 
                      header = TRUE, 
                      stringsAsFactors = FALSE)

glimpse(fish_data)

head(fish_data)

table(fish_data$Species)

model_frame <- fish_data %>% select(Height, Weight, Species)

# View(model_frame)
glimpse(model_frame)

model_frame <- lm(formula=Weight ~ Height, data=model_frame)

print(model_frame)


ggplot(data=model_frame, aes(x=Height, y=Weight)) + 
  geom_smooth(formula=y~x, se=TRUE) + 
  geom_point(aes(color=Species)) 
# + 
  # facet_wrap(~ Species)




