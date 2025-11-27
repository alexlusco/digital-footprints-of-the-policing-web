library(RSelenium)
library(rvest)
library(tidyverse)

driver <- rsDriver(browser=c("firefox"), port= 4835L)
remote_driver <- driver[["client"]]
remote_driver$open()

url <- "https://cullencommission.ca/witnesses/"

remote_driver$navigate(url)

all_button <- remote_driver$findElement(using = "xpath", value = '//*[(@id = "ExpandableFunctionAll")]')

all_button$clickElement()

page_contents <- read_html(remote_driver$getPageSource()[[1]])

page_contents %>%
  html_nodes("strong") %>%
  html_text() %>%
  as_tibble() %>%
  rename(name = value) %>%
  write_csv("data/witnesses/witness_names.csv")

page_contents %>%
  html_nodes(".NamesOfWitnesses") %>%
  html_text() %>%
  as_tibble() %>%
  rename(title = value) %>%
  mutate(title = str_extract(title, "(?=\\()(.*?)\\)")) %>%
  write_csv("data/witnesses/witness_titles.csv")

page_contents %>%
  html_nodes(".WitnessHearingDate") %>%
  html_text() %>%
  as_tibble() %>%
  write_csv("data/witnesses/witness_hearing_dates.csv")

page_contents %>%
  html_nodes(".WitnessHearingDate , .NamesOfWitnesses") %>%
  html_text() %>%
  as_tibble() %>%
  write_csv("data/witnesses/witness_hearing_dates_with_names.csv")


