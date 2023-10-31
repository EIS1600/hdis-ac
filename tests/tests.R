# TESTS

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


library(tidyverse)

asData <- readRDS("./data/AS_data.rds")
asMetadata <- readRDS("./data/AS_metadata.rds")


dummy <- tibble(year = seq(350, 1400), certificates = 0)


chronology <- asData %>%
  filter(attribute_type == "attributes.year") %>%
  group_by(attribute_value) %>%
  summarize(count = n()) %>%
  mutate(attribute_value = as.integer(attribute_value)) %>%
  rename(year = attribute_value, certificates = count) %>%
  add_row(dummy) %>%
  group_by(year) %>%
  summarize(count = sum(certificates)) %>%
  ungroup() #%>% mutate(count = ifelse(count == 0, NA, count))
  

ggplot() + 
  geom_line(data = chronology, aes(x = year, y = count)) + 
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
  labs(title = "Chronological Distribution of Audition Certificates (per year)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill")

ggsave("./tests/chronological_distribution_AC_per_year.jpg",
       plot = last_plot(), width = 400, height = 200,
       units = "mm", dpi = "retina")

chronology10 <- chronology %>%
  mutate(year = ceiling(year / 10) * 10) %>%
  group_by(year) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

ggplot() + 
  geom_step(data = chronology10, aes(x = year, y = count)) + 
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

ggsave("./tests/chronological_distribution_AC_per_decade.jpg",
       plot = last_plot(), width = 400, height = 200,
       units = "mm", dpi = "retina")


chronology20 <- chronology %>%
  mutate(year = ceiling(year / 20) * 20) %>%
  group_by(year) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

ggplot() + 
  geom_step(data = chronology20, aes(x = year, y = count)) + 
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
  labs(title = "Chronological Distribution of Audition Certificates (per 20 year periods)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill")

ggsave("./tests/chronological_distribution_AC_per_2decades.jpg",
       plot = last_plot(), width = 400, height = 200,
       units = "mm", dpi = "retina")


locations <- asData %>%
  filter(attribute_type == "location_2") %>%
  #select(attribute_value) %>%
  group_by(attribute_value) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


# DISTRIBUTION PER CITY (TOP 10)

chronologyA <- asData %>%
  filter(attribute_type == "attributes.year") %>%
  mutate(year = as.numeric(attribute_value)) %>%
  select(AS_ID, year)

locationsA <- asData %>%
  filter(attribute_type == "location_2") %>%
  mutate(location = attribute_value) %>%
  select(AS_ID, location)

yearLocation <- chronologyA %>%
  left_join(locationsA)

distribution <- yearLocation %>%
  group_by(location) %>%
  summarize(certificates = n()) %>%
  arrange(desc(certificates))

places10 <- distribution %>% top_n(11) %>% filter(!is.na(location)) %>% select(location)

places10$location

1496 / length(unique(asData$AS_ID))

# ABOUT 42% (1,496) of DATED CERTIFICATES HAVE NO LOCATION;
# # A tibble: 51 × 2
# location   certificates
#   <chr>              <int>
#  1 NA                 1496
#  2 damascus           1090
#  3 cairo               309
#  4 baghdad             132
#  5 aleppo               60
#  6 alexandria           35
#  7 mecca                22
#  8 jerusalem            19
#  9 baalbek              17
# 10 mossul               17

yearLocationSummed <- yearLocation %>%
  group_by(year, location) %>%
  summarize(certificates = n()) %>%
  ungroup() %>%
  filter(location %in% places10$location)

dummyA <- tibble(year = seq(350, 1400), location = "dummy", certificates = 0)

for (loc in places10$location){
  dummyTemp <- tibble(year = seq(350, 1400), location = loc, certificates = 0)
  dummyA <- dummyA %>%
    add_row(dummyTemp) %>%
    filter(location != "dummy")
}


yearLocationSummed_Final <- yearLocationSummed %>%
  add_row(dummyA) %>%
  group_by(location, year) %>%
  summarize(count = sum(certificates)) %>%
  ungroup() #%>% mutate(count = ifelse(count == 0, NA, count))

ggplot() + 
  geom_line(data = yearLocationSummed_Final, aes(x = year, y = count)) + 
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
  labs(title = "Chronological Distribution of Audition Certificates in the Top 10 Cities (per year)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill") +
  facet_wrap(~location, ncol = 2)

ggsave("./tests/per_city_chronological_distribution_AC_per_year.jpg",
       plot = last_plot(), width = 600, height = 400,
       units = "mm", dpi = "retina")

yearLocationSummed_Final10 <- yearLocationSummed_Final %>%
  mutate(year = ceiling(year / 10) * 10) %>%
  group_by(location, year) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

ggplot() + 
  geom_step(data = yearLocationSummed_Final10, aes(x = year, y = count)) + 
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
  labs(title = "Chronological Distribution of Audition Certificates in the Top 10 Cities (per decade)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill") +
  facet_wrap(~location, ncol = 2)

ggsave("./tests/per_city_chronological_distribution_AC_per_decade.jpg",
       plot = last_plot(), width = 600, height = 400,
       units = "mm", dpi = "retina")


yearLocationSummed_Final20 <- yearLocationSummed_Final %>%
  mutate(year = ceiling(year / 20) * 20) %>%
  group_by(location, year) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

ggplot() + 
  geom_step(data = yearLocationSummed_Final20, aes(x = year, y = count)) + 
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
  labs(title = "Chronological Distribution of Audition Certificates in the Top 10 Cities (per 20 year periods)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill") +
  facet_wrap(~location, ncol = 2)

ggsave("./tests/per_city_chronological_distribution_AC_per_2decades.jpg",
       plot = last_plot(), width = 600, height = 400,
       units = "mm", dpi = "retina")


# DAMASCUS

yearLocationSummed_Final <- yearLocationSummed %>%
  add_row(dummyA) %>%
  group_by(location, year) %>%
  summarize(count = sum(certificates)) %>%
  ungroup() %>%
  filter(location == "damascus") #%>% mutate(count = ifelse(count == 0, NA, count))

ggplot() + 
  geom_line(data = yearLocationSummed_Final, aes(x = year, y = count)) + 
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
  labs(title = "Chronological Distribution of Audition Certificates Damascus (per year)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill")

ggsave("./tests/damascus_chronological_distribution_AC_per_year.jpg",
       plot = last_plot(), width = 400, height = 150,
       units = "mm", dpi = "retina")

yearLocationSummed_Final10 <- yearLocationSummed_Final %>%
  mutate(year = ceiling(year / 10) * 10) %>%
  group_by(location, year) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

ggplot() + 
  geom_step(data = yearLocationSummed_Final10, aes(x = year, y = count)) + 
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
  labs(title = "Chronological Distribution of Audition Certificates in Damascus (per decade)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill")

ggsave("./tests/damascus_chronological_distribution_AC_per_decade.jpg",
       plot = last_plot(), width = 400, height = 150,
       units = "mm", dpi = "retina")


yearLocationSummed_Final20 <- yearLocationSummed_Final %>%
  mutate(year = ceiling(year / 20) * 20) %>%
  group_by(location, year) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

ggplot() + 
  geom_step(data = yearLocationSummed_Final20, aes(x = year, y = count)) + 
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
  labs(title = "Chronological Distribution of Audition Certificates in Damascus (per 20 year periods)",
       caption = "Data Source: Audition Certificates Platform (2023)",
       x = "", y = "", family = "Brill")

ggsave("./tests/damascus_chronological_distribution_AC_per_2decades.jpg",
       plot = last_plot(), width = 400, height = 150,
       units = "mm", dpi = "retina")


            
