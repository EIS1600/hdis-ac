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


