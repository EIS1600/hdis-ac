# TESTS

library(jsonlite)
library(tidyverse)


pathToJSON <- "/Users/romanov/Dropbox/_03_Resources/KONRAD_Samacat/msnotesannotator_audition_certificates_data-master/"

files <- list.files(pathToJSON, pattern = "^audition.*\\.json$", full.names = TRUE)


tempFile <- files[777]

# Read the JSON file into a list
json_data <- fromJSON(tempFile)

json_data

#json_data$properties

json_tibble <- as_tibble(json_data$properties$attributes)
json_tibble <- tibble(json_data$properties)

json_tibble

json_tibble_new <- json_tibble %>%
  bind_cols(as_tibble(json_tibble[[1]]))

(json_tibble[[1]])


view(as.data.frame(json_tibble[[1]]))


library(dplyr)
library(tidyr)

# Extract the $location column
location_col <- json_tibble$attributes$location

# Unlist the column and check its content
unlisted_location <- purrr::map(location_col, ~ifelse(is.null(.x), NA, as.character(.x)))
print(unlisted_location)

# Convert the unlisted content to character
char_location <- purrr::map_chr(unlisted_location, ~ ifelse(is.null(.x), NA_character_, paste(.x, collapse = ", ")))
print(char_location)


view(tibble((as.data.frame(json_data$properties[1][[1]]))))

json_data$properties[1][[1]][1][[1]]

result <- map_chr(json_data$properties[[1]][[2]], ~ ifelse(is.null(.x), NA_character_, .x))
print(result)



json_data$properties[[1]][3]
