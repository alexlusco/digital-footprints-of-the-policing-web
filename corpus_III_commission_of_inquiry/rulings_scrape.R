library(rvest)
library(tidyverse)
library(heapsofpapers)

url <- "https://cullencommission.ca/rulings/"

pdf_urls <- url %>%
  read_html() %>%
  html_node("body") %>%
  html_nodes(".RulingTitle a") %>%
  html_attr("href") %>%
  paste0("https://cullencommission.ca", .) %>%
  as_tibble() %>%
  rename(urls = value) %>%
  mutate(pdf_names = str_extract(urls, "[^\\/]+$"))

ruling_dates <- url %>%
  read_html() %>%
  html_node("body") %>%
  html_nodes(".RulingSeparator+ p , .RulingTitle+ p") %>%
  html_text(trim = TRUE) %>%
  as_tibble() %>%
  rename(dates = value)

bind_cols(pdf_urls, ruling_dates) %>% write_csv("data/rulings/ruling_titles_dates.csv")

heapsofpapers::get_and_save(
  data = pdf_urls,
  links = "urls",
  save_names = "pdf_names",
  dir = "data/rulings",
  dupe_strategy = "ignore"
)
