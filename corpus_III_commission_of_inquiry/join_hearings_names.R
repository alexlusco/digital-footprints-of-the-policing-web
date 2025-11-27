pacman::p_load(readr, dplyr, stringr)

hearings_df <- read_rds("data/pp/transcript_df.rds")
names_df <- read_csv("data/pp/transcripts_df_anno.csv")

hearings_df <- hearings_df %>%
  mutate(doc_title = str_extract(file_dir, "[^\\/]+$"))

names_df <- names_df %>%
  mutate(doc_title = paste0(doc_title, ".pdf"))

merge_df <- hearings_df %>%
  left_join(names_df, by = "doc_title")

merge_df <- merge_df %>% filter(!is.na(police))

merge_df <- merge_df %>%
  select(-file_dir)

write_rds(merge_df, "data/pp/transcripts_names.rds")
