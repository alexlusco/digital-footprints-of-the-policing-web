pacman::p_load(dplyr, readr, stringr)

df <- read_rds("data/pp/transcripts_names.rds")

df <- df %>%
  select(-text_paged) %>%
  distinct(text_all, .keep_all = TRUE)

df <- df %>%
  mutate(questions = str_extract_all(text_all, "(?<=Q  )(?s)(.*?)(?=\\?|A  )"))

write_rds(df, "data/pp/transcripts_names_questions.rds")
