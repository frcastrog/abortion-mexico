#-------------------------------Data Preparation-------------------------------#
#-Author: Francisca Castro ------------------------- Created: January 22, 2024-#
#-R Version: 4.3.1 --------------------------------- Revised: January 22, 2024-#

# 0) Load Packages

pacman::p_load(haven, dplyr, tidyr, magrittr, ggplot2, xtable, knitr)

# 1) Open LAPOP Mexico datasets

# Relative path
data_path <- "01-data/00-raw-data"

# Loop from 2004 to 2023
for(year in 2004:2023) {
  file_name <- paste0(data_path, "/Mexico_", year, "_LAPOP.dta")
  
  # Check if the file exists before reading
  if(file.exists(file_name)) {
    assign(paste0("lapop_", year), read_dta(file_name))
  }
}


# 2) Explore data

# - We first want to assess the N of each entidad federativa
# - The entidad federativas are stored in the following variables in each dataset:
# `lapop_2004` -> mprov
# `lapop_2006` -> MEXESTADO
# `lapop_2008` `lapop_2010` `lapop_2012` `lapop_2014` `lapop_2017` `lapop_2018` `lapop_2023` -> prov
# `lapop_2021` -> prov1t

## Standardize province codes
# - lapop_2004 and lapop_2006 range from 1 to 32 so we will use that to standardize the entidades
# - under the name `entidad`

lapop_2004$entidad <- lapop_2004$mprov
lapop_2006$entidad <- lapop_2006$MEXESTADO

# - for `lapop_2008` `lapop_2010` `lapop_2012` `lapop_2014` `lapop_2017` `lapop_2018` `lapop_2023`
# - extract the last two numbers of prov and store in entidad
for (year in c(2008, 2010, 2012, 2014, 2017, 2018, 2023)) { # Loop through each year
  # Construct the dataset variable name
  dataset_name <- paste0("lapop_", year)
  
  # Check if the dataset exists in the global environment
  if (exists(dataset_name)) {
    # Get the dataset
    dataset <- get(dataset_name)
    
    # Create the 'entidad' variable by extracting the last two digits of 'prov'
    dataset$entidad <- dataset$prov %% 100
    
    # Assign the modified dataset back to its original variable
    assign(dataset_name, dataset)
  }
}

# - check if the loop worked
table(lapop_2008$entidad)
table(lapop_2010$entidad)
table(lapop_2012$entidad)
table(lapop_2014$entidad)
table(lapop_2017$entidad)
table(lapop_2018$entidad)
table(lapop_2023$entidad)

# - for `lapop_2021`
lapop_2021 <- lapop_2021[!is.na(lapop_2021$prov1t), ]  # Remove rows with NAs
lapop_2021$entidad <- lapop_2021$prov1t %% 100  # Create "entidad" variable
table(lapop_2021$entidad)

## Merge all entidad to get the N for each, per survey
datasets_list <- list(
  lapop_2004 %>% select(entidad) %>% mutate(year = 2004),
  lapop_2006 %>% select(entidad) %>% mutate(year = 2006),
  lapop_2008 %>% select(entidad) %>% mutate(year = 2008),
  lapop_2010 %>% select(entidad) %>% mutate(year = 2010),
  lapop_2012 %>% select(entidad) %>% mutate(year = 2012),
  lapop_2014 %>% select(entidad) %>% mutate(year = 2014),
  lapop_2017 %>% select(entidad) %>% mutate(year = 2017),
  lapop_2018 %>% select(entidad) %>% mutate(year = 2018),
  lapop_2021 %>% select(entidad) %>% mutate(year = 2021),
  lapop_2023 %>% select(entidad) %>% mutate(year = 2023))

merged_data <- bind_rows(datasets_list)

summary_entidad <- merged_data %>%
  group_by(year, entidad) %>%
  summarize(entidad_count = n()) %>%
  ungroup()

## Create a table
table_entidad <- summary_entidad %>%
  pivot_wider(names_from = year, values_from = entidad_count, values_fill = 0) %>%
  ungroup() %>%
  mutate(Total = rowSums(.[-1])) %>%  # Add a "Total" column
  arrange(entidad)  # Arrange rows by the "entidad" column in ascending order


# Create a named vector for entidad_mapping
entidad_mapping <- c(
  "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
  "Coahuila", "Colima", "Chiapas", "Chihuahua",
  "Distrito Federal", "Durango", "Guanajuato", "Guerrero", "Hidalgo",
  "Jalisco", "México", "Michoacán", "Morelos", "Nayarit",
  "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo",
  "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas",
  "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")

# Add the entidad names based on the mapping
table_entidad <- table_entidad %>%
  mutate(entidad = entidad_mapping[as.integer(entidad)])

# Get a latex table
table_entidad_latex <- kable(table_entidad, format = "latex", booktabs = TRUE)
table_entidad_latex
