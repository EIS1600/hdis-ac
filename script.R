# SCRIPT for TESTING PURPOSES

library(tidyverse)

# some helping functions

AH2CEa <- function(AH) {
  CE <- round(AH - AH/33 + 622)
  return(CE)
}

AH2CEb <- function(AH) {
  CE <- round(AH - AH/33 + 622)
  AH <- ifelse(AH == 0, 1, AH)
  final <- paste0(AH, " AH\n", CE, " CE")
  return(final)
}

periodsAH <- seq(0, 1400, 50)
periodsCEa <- AH2CEa(periodsAH)
periodsCEb <- AH2CEb(periodsAH)

# loading data

ac_data <- readRDS("data/AS_data.rds")
ac_metadata <- readRDS("data/AS_metadata.rds")

# data: prepare dates data

ac_dates <- ac_data %>%
  filter(attribute_type == "attributes.year") %>%
  mutate(attribute_value = as.numeric(attribute_value))


# histograms

"""
How histograms work: 1) they group values into bins; 2) they draw “bars/columns”, whose height equals the number of values in a bin;
This is the most useful and easy to use type when you need to understand the distribution of values: it is much easier to get correct distribution; 
Disadvantages: hard to do comparative graphs (although `freq_poly` may help).
(Bar charts might seem similar, but they group data categorically.)
"""


# VERY SIMPLE
ggplot() +
  geom_histogram(data = ac_dates,
                 aes(x = attribute_value),
                 binwidth = 10)

# PRETTIFIED
ggplot() +
  geom_histogram(data = ac_dates, aes(x = attribute_value), binwidth = 10) +
  scale_x_continuous(breaks = periodsAH, labels = periodsCEb) +
  labs(x = "", y = "") +
  theme_set(theme_minimal())+
  theme(text = element_text(family = "Brill"),
        # face = "italic", # makes all italic...
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.175,0.65), legend.title = element_blank(),
  ) +
  labs(title = "Chronological Distribution of Audition Certificates (per decade)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill")


ggsave("./tests/chronological_distribution_AC_per_decade_histogram.jpg",
       plot = last_plot(), width = 400, height = 200,
       units = "mm", dpi = "retina")


ggplot() +
  geom_freqpoly(data = ac_dates,
                aes(x = attribute_value),
                binwidth = 10)


"""
1. What will change if we change the value of bins?
2. What other histograms can we generate with our data?
"""
  
# line charts

"""
Line charts draw lines between a series of dots. They are a most common type of graphs for chronological distributions, however,
they require much more care at the step of data preparation, especially to ensure that missing values are reflected at the final graph.
"""

# gaps lost

chronologyA <- ac_data %>%
  filter(attribute_type == "attributes.year") %>%
  mutate(attribute_value = as.integer(attribute_value)) %>%
  mutate(attribute_value = as.integer(ceiling(attribute_value / 10) * 10)) %>% # convert into decades
  group_by(attribute_value) %>%
  summarize(count = n()) %>%
  rename(year = attribute_value, certificates = count) %>%
  ungroup()

ggplot() + 
  geom_line(data = chronologyA, aes(x = year, y = certificates))


# gaps kept

dummy <- tibble(year = seq(350, 1400, 10), certificates = 0)

chronologyB <- ac_data %>%
  filter(attribute_type == "attributes.year") %>%
  mutate(attribute_value = as.integer(attribute_value)) %>%
  mutate(attribute_value = as.integer(ceiling(attribute_value / 10) * 10)) %>% # convert into decades
  group_by(attribute_value) %>%
  summarize(count = n()) %>%
  rename(year = attribute_value, certificates = count) %>%
  add_row(dummy) %>%
  group_by(year) %>%
  summarize(count = sum(certificates)) %>%
  ungroup() #%>% mutate(count = ifelse(count == 0, NA, count))

ggplot() + 
  geom_line(data = chronologyB, aes(x = year, y = count))

# together

ggplot() + 
  geom_line(data = chronologyA, aes(x = year, y = certificates), col = "blue") +
  geom_line(data = chronologyB, aes(x = year, y = count), col = "darkgreen") + 
  geom_segment(data = chronologyB, aes(x = year, xend = year, y = 0, yend = count), col = "red", linetype = 3)
  

# prettified

ggplot() + 
  geom_line(data = chronologyB, aes(x = year, y = count))+ 
  scale_x_continuous(breaks = periodsAH, labels = periodsCEb) +
  labs(x = "", y = "") +
  theme_set(theme_minimal())+
  theme(text = element_text(family = "Brill"),
        # face = "italic", # makes all italic...
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.175,0.65), legend.title = element_blank(),
  ) +
  labs(title = "Chronological Distribution of Audition Certificates (per decade)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill")

ggsave("./tests/chronological_distribution_AC_per_year.jpg",
       plot = last_plot(), width = 400, height = 200,
       units = "mm", dpi = "retina")

"""
1. Generate graphs for 20 year periods; for 50 year periods;
2. Generate graphs for different geographical locations;
"""

# scatterplots

"""
Scatterplots are used to plot relationships between two variables in order to see if there is any corelation between them. 
Let's take a look at how it works on a completely different example, using `mtcars` standard dataset.
"""

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Scatterplot of MPG vs Car Weight") +
  xlab("Weight (1000 lbs)") +
  ylab("Miles per Gallon")

"""
In this graph we compare weights of cars and their mileage per gallon (i.e. how many miles they can run on about 4 liters of gas).
Each dot represents a car (a specific make and model) and it is plotted using its weight (x-axis) and its mileage (y-axis).
Even without the red line, which is meant to represent the trend in data, you can see that the heavier the car, the shorter the distance
it can run per gallon.

The graph would be usually interpreted in the following manner: This scatterplot visually represents the relationship between the weight
of the cars and their fuel efficiency. The points show the actual data, while the red line represents the best linear fit, illustrating
the general trend. In this case, the negative slope of the line indicates a negative correlation, suggesting that as the weight of the
car increases, its fuel efficiency (miles per gallon) tends to decrease.
"""

"""
Now, let's try with our data:

- check if the number of participants was increasing with time? I.e., in the later period these groups were larger than in earlier;
- check if there is some correlation between the day of the week and the number of participants;

"""

ac_participants <- ac_data %>%
  filter(type == "person") %>%
  group_by(AS_ID) %>%
  summarize(count = n())


# year Vs participants

ac_participants_year <- ac_data %>%
  filter(attribute_type == "attributes.year") %>%
  mutate(attribute_value = as.numeric(attribute_value)) %>%
  left_join(ac_participants) %>%
  # REMOVE UNDATES certificates: losing about 200 certificates
  filter(!is.na(attribute_value))
  
ggplot(ac_participants_year, aes(x = attribute_value, y = count)) +
  geom_point(color = "blue", alpha = 0.25) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(breaks = periodsAH, labels = periodsCEb) +
  theme_set(theme_minimal())+
  theme(text = element_text(family = "Brill"),
        # face = "italic", # makes all italic...
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.175,0.65), legend.title = element_blank(),
  ) +
  labs(title = "Year Vs Participants",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "year AH", y = "# of participants", family = "Brill")


ac_participants_year_filt <- ac_participants_year %>%
  filter(attribute_value >= 500) %>%
  filter(attribute_value <= 700) %>%
  filter(count <= 100)


ggplot(ac_participants_year_filt, aes(x = attribute_value, y = count)) +
  geom_point(color = "blue", alpha = 0.25) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(breaks = periodsAH, labels = periodsCEb) +
  theme_set(theme_minimal())+
  theme(text = element_text(family = "Brill"),
        # face = "italic", # makes all italic...
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.175,0.65), legend.title = element_blank(),
  ) +
  labs(title = "Year Vs Participants",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "year AH", y = "# of participants", family = "Brill")
  

# day Vs participants

unique(ac_participants_dow$attribute_value)

DOWtibble <- tibble(
  attribute_value = c("Yawm al-ahad", "Yawm al-ithnayn", "Yawm al-thalatha", "Yawm al-arba'a", "Yawm al-khamis", "Yawm al-juma", "Yawm al-sabt"),
  attribute_label = c("1. yawm al-aḥad", "2. yawm al-iṯnayn", "3. yawm al-ṯulaṯāʾ", "4. yawm al-arbiʿāʾ", "5. yawm al-ḫamīs", "6. yawm al-jumʿaŧ", "7. yawm al-sabt"),
  attribute_index = c(1,2,3,4,5,6,7)
)

ac_participants_dow <- ac_data %>%
  filter(attribute_type == "attributes.weekday") %>%
  left_join(ac_participants) %>%
  # REMOVE UNDATES certificates: losing about 200 certificates
  filter(!is.na(attribute_value)) %>%
  left_join(DOWtibble) %>%
  filter(count <= 100)

ggplot(ac_participants_dow, aes(x = attribute_index, y = count)) +
  geom_point(color = "blue", alpha = 0.25, position = position_jitter(width=0.25, height=0.25)) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(breaks = DOWtibble$attribute_index, labels = DOWtibble$attribute_label) +
  theme_set(theme_minimal())+
  theme(text = element_text(family = "Brill"),
        # face = "italic", # makes all italic...
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.175,0.65), legend.title = element_blank(),
  ) +
  labs(title = "Day of the week Vs Participants",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "day of the week", y = "# of participants", family = "Brill")

## perhaps, types of books would be more relevant?!



# bar charts

"""
Bar chart represent values per category. For example, we can check how many sessions we have per day of the week. 
"""

ac_participants_dow <- ac_data %>%
  filter(attribute_type == "attributes.weekday") %>%
  left_join(ac_participants) %>%
  # REMOVE UNDATES certificates: losing about 200 certificates
  filter(!is.na(attribute_value)) %>%
  left_join(DOWtibble)


ac_participants_per_dow <- ac_participants_dow %>%
  group_by(attribute_label, attribute_index) %>%
  summarize(
    sessions = n(),
    people_total = sum(count),
    participants_mean = mean(count),
    participants_median = median(count)
  ) %>%
  arrange(attribute_index)

ac_participants_per_dow

ggplot(ac_participants_per_dow, aes(x = factor(attribute_label), y = sessions)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_set(theme_minimal())+
  theme(text = element_text(family = "Brill"),
        # face = "italic", # makes all italic...
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.175,0.65), legend.title = element_blank(),
  ) +
  ggtitle("Total number of sessions by DOW") +
  xlab("") +
  ylab("people")

ggplot(ac_participants_per_dow, aes(x = factor(attribute_label), y = people_total)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_set(theme_minimal())+
  theme(text = element_text(family = "Brill"),
        # face = "italic", # makes all italic...
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.175,0.65), legend.title = element_blank(),
  ) +
  ggtitle("Total number of people by DOW") +
  xlab("") +
  ylab("people")


ggplot(ac_participants_per_dow, aes(x = factor(attribute_label), y = participants_mean)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_set(theme_minimal())+
  theme(text = element_text(family = "Brill"),
        # face = "italic", # makes all italic...
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.175,0.65), legend.title = element_blank(),
  ) +
  ggtitle("Mean number of people by DOW") +
  xlab("") +
  ylab("people")


ggplot(ac_participants_per_dow, aes(x = factor(attribute_label), y = participants_median)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_set(theme_minimal())+
  theme(text = element_text(family = "Brill"),
        # face = "italic", # makes all italic...
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        legend.position = c(0.175,0.65), legend.title = element_blank(),
  ) +
  ggtitle("Median number of people by DOW") +
  xlab("") +
  ylab("people")



# Alternatives

"""
- `summary(vector)`
- library(gtExtras)
- summary table with pregenerated stats;
"""

ac_participants_per_dow <- ac_participants_dow %>%
  group_by(attribute_label, attribute_index) %>%
  summarize(
    sessions = n(),
    people_total = sum(count),
    participants_mean = mean(count),
    participants_median = median(count),
    participants_sd = sd(count),
    participants_IQRa = IQR(count),
    Lower_Quartile = quantile(count, 0.25),
    Upper_Quartile = quantile(count, 0.75),
    participants_IQRb = Upper_Quartile - Lower_Quartile
  ) %>%
  arrange(attribute_index)


ac_participants_per_dow

knitr::kable(ac_participants_per_dow)

# DATE CONVERSION FUNCTION

# Written by ChatGPT, so, use with caution: <https://chat.openai.com/share/e1c86951-6507-44dc-aaa8-0d5a79e38bb2>
# Wüstenfeld's Tables: https://archive.org/details/vergleichungstab01wust/mode/2up

# Vergleichungs -Tabellen der Muhammedanischen und Christlichen Zeitrechnung
# by Wüstenfeld, Ferdinand, 1808-1899; Mahler, Eduard, 1857-1945
# Fortsetzung der Wüstenfeld'schen Vergleichungs-Tabellen der Muhammedanischen und Christliche
# Zeitrechnung von 1300 bis 1500 der Hedschra; Deutsche Morgenländische Gesellschaft

# Intro from Wüstenfeld

# Da die Muhammedaner nach Mondjahren zu 354 Tagen rechnen, so ist ihr Jahr durchschnittlich um 11 Tage kürzer als unser Sonnenjahr, was auf hundert Jahre einen Unterschied von etwa drei Jahren ausmacht. Zur genauern Vergleichung dieser beiden Zeitrechnungen sind mun besonders in dem letzten Jahrhundert eine nicht unbedeutende Anzahl von Tabellen erschienen 1) und man hat auch verschiedene Methoden zu ihrer Berechnung bekannt gemacht 2). Das Verfahren hierbei ist indess theils nicht leicht im Gedächtniss zu behalten, wenn man nicht täglich damit umgeht, theils erfordert es einige Gewandtheit im Rechnen, theils ist es immer mit einigem Aufwande von Zeit verbunden, - Grund genug, warum es gewiss mur höchst selten von Gelehrten in Anwendung gebracht ist. Jene Tabellen aber waren zum Theil nicht jedem zugänglich, zum Theil waren sie nicht vollständig oder nicht zweckmässig geordnet, einige sogar fehlerhaft berechnet, und selbst die ausführlichsten beschränkten sich darauf, nur für den Anfang jedes Muhammedanischen Jahres das entsprechende Datum unsrer Zeitrechnung anzugeben. Deshalb hatte ich schon vor mehreren Jahren angefangen, zum eigenen Gebrauch etwas ausführlichere Tabellen anzufertigen und bin nun dem durch einen Freund, welcher davon Kenntniss erhielt, veranlassten Auftrage des Vorstandes der Deutschen Morgenländischen Gesellschaft bereitwillig nachgekommen, diese Tabellen bis auf die neueste Zeit fortzuführen und der Öffentlichkeit zu übergeben.
# Sie sind also zunächst, wie der Titel besagt, nach dem ersten Tage jedes Muhammedanischen Monats berechnet, so dass man aus ihnen z. B. sogleich ersehen kann, dass der 1. Ramadhan 151 dem 18. September 768 entspricht; zugleich weist das neben diesem Datum stehende A darauf hin, dass dieser Tag ein Sonntag war. Eine weitere Thätigkeit des Nachschlagenden findet dann mur noch für die Tage innerhalb eines Monates statt. Da es für manche Berechnung von Wichtigkeit ist, den so genannten SonntagsBuchstaben zu kennen, so ist dieser gleich vorn neben die Jahrszahl geselzt, wobei indess zu merken, dass der Anfang des betreffenden Christlichen Jahres in den meisten Fällen schon in die vorhergehende Reihe fällt und also der Sonntags- Buchstab auch rückwärts bis zu diesem Anfange gilt; z. B. für das Christliche Jahr 771 ist F als Sonntagsbuchstab neben dem Muhammedanischen Jahre 155 angemerkt, ungeachtet das Jahr 771 fast ganz mit dem Jahre 154 zusammenfällt.
# Da Christliche Schaltjahre alle diejenigen sind, welche sich mit 4 theilen lassen, ausgenommen 1700 und 1800, so war hier eine Bezeichnung unnötig; dagegen sind die Muhammedanischen Schaltjahre mit einem Sternchen versehen und zwar rechts, um darauf aufmerksam zu machen, dass der letzte Monat, neben welchem das Sternchen steht, 30 Tage habe, weil der Schalttag immer am Ende des Jahres hinzugerechnet wird.
# In den beiden Streitfragen, ob die Muhammedanische Zeitrechnung Donnerstag den 15. Juli oder Freilag den 16. Juli 622 beginne, und ob in dem 30jährigen Cyclus das 15. oder 16. Jahr ein Schaltjahr sei, habe ich mich jedesmal für die zweite Meinung entschieden.
# Die Einführung des Gregorianischen Calenders und die in Folge davon überschlagenen zehn Tage vom 5. bis 14. October 1582 sind an der betreffenden Stelle Seite 41 bemerklich gemacht und von da an die Berechnung nur nach dem Gregorianischen Calender forigeführt.

# EARLY

library(lubridate)

hijri_to_gregorian <- function(hijri_year, hijri_month = 1, hijri_day = 1) {
  
  # Subtract 1 from the Hijri year, because the Hijri calendar starts from the year 1, not 0.
  h_year <- hijri_year - 1
  
  # Calculate the approximate Gregorian year and day of the year.
  g_year <- 621.57 + (h_year * 0.97)
  day_of_year <- ((hijri_month - 1) * 29.5) + hijri_day + 3
  
  # If the day of the year is more than 366, add 1 to the Gregorian year.
  g_year <- ifelse(day_of_year <= 366, floor(g_year), floor(g_year) + 1)
  
  # Create a date object from the Gregorian year and day of the year.
  g_date <- as.Date(paste(g_year, "01", "01", sep = "-")) + days(day_of_year - 1)
  
  # Map the weekday integer to a day name.
  weekday <- weekdays(g_date, abbreviate = FALSE)
  
  # Return the date and the day of the week.
  list("date" = g_date, "day_of_week" = weekday)
}

# Example usage
result <- hijri_to_gregorian(597)
print(paste("Date:", result$date, ", Day of the Week:", result$day_of_week))



library(lubridate)


# UPDATED:

hijri_to_gregorian <- function(hijri_year, hijri_month = 1, hijri_day = 1) {
  
  # Subtract 1 from the Hijri year, because the Hijri calendar starts from the year 1, not 0.
  h_year <- hijri_year - 1
  
  # Calculate the approximate Gregorian year and day of the year.
  g_year <- 621.57 + (h_year * 0.97)
  day_of_year <- ((hijri_month - 1) * 29.5) + hijri_day + 3
  
  # If the day of the year is more than 366, add 1 to the Gregorian year.
  g_year <- ifelse(day_of_year <= 366, floor(g_year), floor(g_year) + 1)
  
  # Create a date object from the Gregorian year and day of the year.
  g_date <- as.Date(paste(g_year, "01", "01", sep = "-")) + days(day_of_year - 1)
  
  # Determine the season
  month <- as.numeric(format(g_date, "%m"))
  season <- ifelse(month %in% c(12, 1, 2), "Winter",
                   ifelse(month %in% 3:5, "Spring",
                          ifelse(month %in% 6:8, "Summer", "Fall")))
  
  # Map the weekday integer to a day name.
  weekday <- weekdays(g_date, abbreviate = FALSE)
  
  # Return the date, the day of the week, and the season.
  list("date" = g_date, "day_of_week" = weekday, "season" = season)
}


convert_month_name_to_number <- function(month_name) {
  month_map <- c(
    "Muharram" = 1,
    "محرم" = 1, 
    "Safar" = 2,
    "صفر" = 2, 
    "Rabi' al-awwal" = 3,
    "ربيع الأول" = 3,
    "Rabi' I" = 3,
    "Rabi' al-thani" = 4,
    "ربيع الثاني" = 4,
    "Rabi' II" = 4, 
    "Jumada al-awwal" = 5,
    "جمادى الأول" = 5,
    "Jumada I" = 5,
    "Jumada al-thani" = 6, 
    "جمادى الآخر" = 6, 
    "Jumada II" = 6, 
    "Rajab" = 7, 
    "رجب" = 7, 
    "Sha'ban" = 8, 
    "شعبان" = 8,
    "Ramadan" = 9, 
    "رمضان" = 9, 
    "Shawwal" = 10, 
    "شوال" = 10, 
    "Dhu al-Qi'dah" = 11, 
    "ذو القعدة" = 11, 
    "Dhu al-Hijjah" = 12, 
    "ذو الحجة" = 12)
  
  return(month_map[month_name])
}

# Example usage:
convert_month_name_to_number("رمضان")
convert_month_name_to_number("Rabi' al-awwal")


# Example usage:
convert_month_name_to_number("Ramadan")


hijri_to_gregorian(597, 7, 1)


