# SCRIPT for TESTING PURPOSES

library(tidyverse)

PUA <- readRDS("./data/PUA_allDataTables_asList.rds")

personaje <- PUA$personaje

write_delim(personaje, "./data/PUA_personaje.tsv", delim = "\t")
