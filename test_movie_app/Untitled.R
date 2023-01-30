bway <- read.csv('Broadway_Prettified.csv', sep = ',', encoding = 'utf-8')
print(bway)
str(bway)

bway <- toTitleCase(str_replace_all(bway$Show.Title, " ", "."))
print(unique(bway$Theatre))

p <- ggplot(bway,aes(x="Show.Title")) + geom_bar()
print(p)

