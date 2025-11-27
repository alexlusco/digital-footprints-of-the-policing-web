library(rvest)
library(tidyverse)
library(heapsofpapers)

url <- "https://cullencommission.ca/closing-submissions/"

pdf_urls <- url %>%
  read_html() %>%
  html_node("body") %>%
  html_nodes("li a") %>%
  html_attr("href") %>%
  paste0("https://cullencommission.ca", .) %>%
  as_tibble() %>%
  rename(urls = value) %>%
  mutate(pdf_names = str_extract(urls, "[^\\/]+$"))

submission_type <- url %>%
  read_html() %>%
  html_node("body") %>%
  html_nodes("li") %>%
  html_text(trim = TRUE) %>%
  as_tibble() %>%
  rename(type = value)

bind_cols(pdf_urls, submission_type) %>% write_csv("data/closing-submissions/submission_type_titles_urls.csv")

heapsofpapers::get_and_save(
  data = pdf_urls,
  links = "urls",
  save_names = "pdf_names",
  dir = "data/closing-submissions",
  dupe_strategy = "ignore"
)
