# Load dependencies -------------------------------------------------------
library(tidyverse)
library(rvest)
library(xml2)

get_cohort_data <- function(year) {
  # web parameters --------------------------------------------------------
  base_url <- "https://www.mccormick.northwestern.edu/analytics/people/alumni/class-of-"
  year <- year
  extension <- ".html"

  url <- paste0(base_url, year, extension)
  url <- url(url, "rb")
  on.exit(close(url)) # close any url connections when exiting the function

  # scrape -------------------------------------------------------------------
  # professional data are stored within <div> tags with class 'faculty info'
  cohort <- url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = "//div[@class='faculty-info']") %>%
    rvest::html_text()

  # clean -----------------------------------------------------------------
  cohort <- cohort %>%
    # insert commas for easy delimiting
    str_replace(pattern = paste0("MSiA Class of ", year),
                replacement = paste0(";", year, ";")) %>%
    # split into separate columns in a tibble
    read_csv2(col_names = c("Name", "Class", "Title")) %>%
    # split up title into separate role and company columns
    separate(Title, into = c("Role", "Company"), sep = " at ")

  cohort$Company <- cohort$Company %>%
    # combine different spellings of the same company
    str_replace(pattern = "Blue Cross and Blue Shield of Illinois",
                replacement = "Blue Cross and Blue Shield") %>%
    str_replace(pattern = " *\\(.*\\)",  # remove all items in parentheses, e.g. "JPL (...)"
                replacement = "") %>%
    str_replace(pattern = "^JPL",
                replacement = "NASA JPL") %>%
    str_replace(pattern = "KPMG US",
                replacement = "KPMG")

  cohort
}

get_all_alumni_data <- function(latest_available_year = 2018) {
  alumni <- tibble()

  for (year in seq(2013, latest_available_year)) {
    alumni <- bind_rows(alumni, get_cohort_data(year))
  }

  alumni
}
