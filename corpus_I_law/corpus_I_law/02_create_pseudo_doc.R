pacman::p_load(readr, dplyr, stringr, tidytext, tidyr)

court_decisions <- read_csv("data/processed/all_court_decisions_pp.csv")

court_dec_tokens <- court_decisions %>%
  separate_rows(text, sep = "\n") %>%
  rename(paragraph = text)

pseudo_docs <- court_dec_tokens %>%
  mutate(paragraph2 = str_remove_all(paragraph, "[:punct:]"),
         paragraph2 = str_to_lower(paragraph2)) %>%
  mutate(policing = str_detect(paragraph2, "police|policier|sergeant|constable|detective|investigation|arrest|officer|officier|agent|gendarmerie|lieutenant|enquête|imet |upac |rcmp |opp |tps|sq |grc |jsot |integrated market enforcement|joint serious offences|unité permenante|sûreté du")) %>%
  mutate(word_count = str_count(paragraph, "\\w+"))

pseudo_docs_policing <- pseudo_docs %>%
  filter(policing == TRUE) %>%
  select(-paragraph2, -policing)

# save as csv
write_excel_csv(pseudo_docs_policing, "data/processed/pseudo_docs_policing.csv")

# output as txt for qualitative analysis with metadata
pseudo_docs_policing %>%
  select(doc_id, paragraph) %>%
  group_by(doc_id) %>%
  mutate(out = paste0(paragraph, collapse = "\n\n")) %>%
  distinct(doc_id, .keep_all = TRUE) %>%
  mutate(doc_id = paste("DOC_ID: ", doc_id)) %>%
  select(doc_id, out) %>%
  write_delim("pseudo_docs_policing.txt", col_names = FALSE, delim = "\n\n", eol = "\n\n", quote_escape = "none")
