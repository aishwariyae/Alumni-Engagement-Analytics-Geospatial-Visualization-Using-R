alumnidata<-read.csv("C:/Users/Owner/Downloads/Project1_Alumni_Meet.csv")
alumnidata
head(alumnidata)
colnames(alumnidata)<-c("city", "state", "zip", "grad_year", "major")
city <- alumnidata$city
state <- alumnidata$state
grad_year <- alumnidata$grad_year

table(alumnidata$state)
names(state_freq[state_freq == max_alums])


top5_table <- head(sort(table(alumnidata$state), decreasing = TRUE), 5)
bar_positions <- barplot(
  top5_table,
  main = "Top 5 States by Number of Alumni",
  xlab = "State",
  ylab = "Number of Alumni",
  ylim = c(0, 10000),
  col = c("blue", "dodgerblue", "deepskyblue", "lightskyblue", "powderblue"))
text(x = bar_positions, y = top5_table, labels = top5_table, pos = 3)


ma_data <- alumnidata[alumnidata$state == "Massachusetts", ]
top_ma_cities <- head(sort(table(ma_data$city), decreasing = TRUE), 10)
bar_positions<- barplot(
  top_ma_cities,
  main = "Top 10 Cities in Massachusetts by Number of Alumni",
  xlab = "City",
  ylab = "Number of Alumni",
  ylim = c(0, 410),
  las = 1,
  col = c("darkblue", "blue", "dodgerblue", "deepskyblue", "skyblue",
    "slateblue", "mediumslateblue", "mediumpurple", "blueviolet", "purple"))
text(x = bar_positions, y = top_ma_cities + 0.5, labels = top_ma_cities, pos=3)


nh_data <- alumnidata[alumnidata$state == "New Hampshire", ]
top_nh_cities <- head(sort(table(nh_data$city), decreasing = TRUE), 10)
bar_position <- barplot(
  top_nh_cities,
  main = "Top 10 Cities in New Hampshire by Number of Alumni",
  xlab = "City",
  ylab = "Number of Alumni",
  ylim = c(0,65),
  las = 1,
  col = c( "navy","royalblue","dodgerblue","deepskyblue","lightskyblue",
"paleturquoise","mediumaquamarine","mediumseagreen","seagreen","forestgreen"))
text(x = bar_position, y = top_nh_cities + 0.5, labels = top_nh_cities, pos=3)


cities <- c("Manchester","Nashua","Salem","Derry","Dover")
city_subset <- alumnidata[alumnidata$city %in% cities &alumnidata$state == "New Hampshire",]
city_counts <- sort(table(city_subset$city), decreasing = TRUE)
bp <- barplot(
  city_counts,
  main = "Top 5 Metropolitan Cities in New Hampshire by Number of Alumni",
  xlab = "City",
  ylab = "Number of Alumni",
  ylim = c(0,65),
  las = 1,
col = c("lightskyblue","paleturquoise","mediumaquamarine","mediumseagreen","seagreen"))
text(x = bp,y = city_counts,labels = city_counts,pos = 3)




install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)


alumnidata<-read.csv("C:/Users/Owner/Downloads/Project1_Alumni_Meet.csv")
alumnidata
head(alumnidata)
colnames(alumnidata)<-c("city", "state", "zip", "grad_year", "major")
city <- alumnidata$city
state <- alumnidata$state
grad_year <- alumnidata$grad_year


nh_cities_school <- nh_cities %>%
  mutate(
    school = case_when(
      major %in% c("Biostatistics", "Biology", "Chemistry", "Developmental Psychology", "Mathematics", "Neuroscience", "Psychology", "Psychology-General", "Psychology-Health/Counseling", "Psychology with Neuroscience concentration") ~ 
        "School of Science & Health",

      major %in% c("Business Management", "Management", "Management-General", "Marketing") ~ 
        "School of Business & Management",

      major %in% c("Art-Painting & Printmaking", "Art-Studio", "Art Education", "Communications & Media Studies", "Criminology & Criminal Justice", "English", "English Communications", "English Literature", "French", "German", "Graphic Design", "History", "Italian", "Latin", "Literature", "Media", "Philosophy", "Pol Sci with IR & CG conc", "Political Science", "Russian", "Sociology", "Sociology-Social Inequity & Social Justice", "Writing and Literature", "Writing, Editing & Publishing") ~ 
        "School of Humanities & Social Sciences",

      major %in% c("Education", "Education-Elementary", "Education-General", "Secondary Education") ~ 
        "School of Education",

      major %in% c("Nursing") ~ 
        "School of Nursing & Clinical Sciences",

      TRUE ~ "Other"
    )
  )

ggplot(nh_cities_school, aes(x = city, fill = school)) +
  geom_bar() +
  labs(
    title = "Distribution of Alumni by School Across Three New Hampshire Cities",
    x = "City",
    y = "Number of Alumni",
    fill = "School"
  ) +
  theme_minimal()









