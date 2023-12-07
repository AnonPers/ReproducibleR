## ---------------------------
##
## Script name: Cleaning.r
##
## Purpose of script: 
##      Cleaning up the raw penguin data  
##
##
##
## Date Created: 2023-12-04
##
##
## ---------------------------
##

# A function to make sure the column names are cleaned up, 
# eg lower case and snake case
clean_column_names <- function(penguins_data) {
  penguins_data %>%
    clean_names()
}

# A function to make sure the species names are shortened
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# A function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_data) {
  penguins_data %>%
    remove_empty(c("rows", "cols"))
}


# A function to subset the data based on the list of column names
subset_columns <- function(penguins_data, column_names) {
  penguins_data %>%
    select(all_of(column_names))
}

# A function to subset the penguins data set based on species
filter_by_species <- function(penguins_data, selected_species) {
  penguins_data %>%
    filter(species == selected_species)
}


#A function to subset body mass, species, and flipper length, and remove NAs 

remove_empty_mass_flippers <- function(data_clean){
  data_clean %>%
    filter(!is.na(flipper_length_mm)) %>%
    filter(!is.na(body_mass_g)) %>%
    select(species, flipper_length_mm, body_mass_g)
}


# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit()
}