library(rvest)
library(tidyverse)
library(heapsofpapers)

url <- "https://cullencommission.ca/transcripts/"

pdf_urls <- url %>%
  read_html() %>%
  html_node("body") %>%
  html_nodes("#Content > p a") %>%
  html_attr("href") %>%
  paste0("https://cullencommission.ca", .) %>%
  as_tibble()

pdf_urls <- pdf_urls %>%
  mutate(pdf_names = str_extract(value, "[^\\/]+$"),
         pdf_names = str_replace_all(pdf_names, "\\,|-", ""),
         pdf_names = str_squish(pdf_names),
         pdf_names = str_replace_all(pdf_names, " ", "_")) %>%
  rename(urls = value) %>%
  mutate(urls = str_replace_all(urls, " ", "%20"))

#setwd("/Users/alexluscombe/Dropbox/Git_Repos/cullen-commission")

heapsofpapers::get_and_save(
  data = pdf_urls,
  links = "urls",
  save_names = "pdf_names",
  dir = "data/transcripts"
)
