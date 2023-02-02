
library(dplyr)
bway <- read.csv('Broadway_Prettified_LFG.csv', sep = ',', encoding = 'utf-8')
str(bway)
print


#Barplot----------------------------------------------------------------------------------------
bway <- bway%>%
  select(Show.Theatre,
         Show.Name,
         Show.Type,
         Date.Full,
         Date.Year,
         Date.Month,
         Statistics.Attendance,
         Statistics.Gross,
         Statistics.Capacity,
          )

bway <- bway %>% 
  rename( "Show" = "Show.Name",
          "Date" = "Date.Full",
          "Year" = "Date.Year",
          "Month" = "Date.Month",
          "Theatre" = "Show.Theatre",
          "Avg_Attendance" = "Statistics.Attendance",
          "Avg_Capacity" = "Statistics.Capacity",
          "Avg_Gross" = "Statistics.Gross",
          "Type" = "Show.Type"
  )

write.csv(bway, "/Users/morganhogenmiller/mhogenmi_hw1/Broadway_Data_App/Final_Bway.csv")

#Scatterplot----------------------------------------------------------------------------------------
bway_group_month <- bway %>% 
  group_by(Theatre,Year,Show, Month) %>% 
  summarise(Capacity=mean(Avg_Capacity),
            Attendance=mean(Avg_Attendance),
            Gross=mean(Avg_Gross),
            .groups = 'drop')

write.csv(bway_group_month, "/Users/morganhogenmiller/mhogenmi_hw1/Broadway_Data_App/Monthly_Avg_Metrics.csv")


#Donut Chart----------------------------------------------------------------------------------------
gb_type <- bway %>%
  group_by(Year, Theatre) %>%
  count(Type)
gb_type$fraction <- gb_type$n/ sum(gb_type$n)
gb_type$ymax = cumsum(gb_type$fraction)
gb_type$ymin = c(0, head(gb_type$ymax, n=-1))
gb_type$labelPosition <- (gb_type$ymax + gb_type$ymin) / 2
gb_type$label <- paste0(gb_type$Type, "\n (# of weeks): ", gb_type$n)
write.csv(gb_type, "/Users/morganhogenmiller/mhogenmi_hw1/Broadway_Data_App/Donut_Chart_Manipulated.csv")




#----------------REMOVE THIS AND PUT IN SCRIPT
# bway <- bway %>% 
#   rename( "Show" = "Show.Name",
#           "Date" = "Date.Full",
#           "Year" = "Date.Year",
#           "Month" = "Date.Month",
#           "Theatre" = "Show.Theatre",
#           "Avg_Attendance" = "Statistics.Attendance",
#           "Avg_Capacity" = "Statistics.Capacity",
#           "Avg_Gross" = "Statistics.Gross",
#           "Type" = "Show.Type"
#   )




# #Special version for scatterplot
# bway_group_month <- bway %>%
#   group_by(Theatre,Year,Show, Month) %>%
#   summarise(Capacity=mean(Avg_Capacity),
#             Attendance=mean(Avg_Attendance),
#             Gross=mean(Avg_Gross),
#             .groups = 'drop')


# #for donut chart -------------------------- PUT THIS IN ANOTHER SCRIPT
# gb_type <- bway %>%
#   group_by(Year, Theatre) %>%
#   count(Type)
# gb_type$fraction <- gb_type$n/ sum(gb_type$n)
# gb_type$ymax = cumsum(gb_type$fraction)
# gb_type$ymin = c(0, head(gb_type$ymax, n=-1))
# gb_type$labelPosition <- (gb_type$ymax + gb_type$ymin) / 2
# gb_type$label <- paste0(gb_type$Type, "\n (# of weeks): ", gb_type$n)