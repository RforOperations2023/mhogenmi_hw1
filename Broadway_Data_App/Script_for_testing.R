
library(dplyr)
bway <- read.csv('Broadway_Prettified_LFG.csv', sep = ',', encoding = 'utf-8')
print(bway)
str(bway)

library(ggplot2)
bway_group_type_count <- bway %>% 
      group_by(Date.Year,Show.Theatre) %>% 
      count(Show.Type)

gb_type <- bway %>%
  group_by(Show.Theatre) %>%
  mutate(count(Show.Type))
gb_type$fraction <- gb_type$n/ sum(gb_type$n)
gb_type$ymax = cumsum(gb_type$fraction)
gb_type$ymin = c(0, head(gb_type$ymax, n=-1))

show(gb_type)
# Make the plot
ggplot(gb_type, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Show.Type)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart


show(gb_type)


print(bway_group_month)






