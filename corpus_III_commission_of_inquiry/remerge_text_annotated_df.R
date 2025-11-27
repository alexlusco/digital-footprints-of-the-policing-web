pacman::p_load(readr, dplyr)

annotations <- read_csv("data/pp/transcripts_for_annotating.csv")

annotations <- annotations %>%
  mutate(row_id = row_number())

transcripts <- read_csv("data/pp/transcripts_names_separated.csv")

transcripts <- transcripts %>%
  mutate(row_id = row_number()) %>%
  select(text_all, row_id)

merged_df <- annotations %>%
  left_join(transcripts, by = c("row_id")) %>%
  select(-witness_correction_needed, -separable, -separator) %>%
  group_by(witness_raw) %>%
  mutate(interview_id = cur_group_id()) %>%
  select(-row_id)

merged_df %>%
  arrange(interview_id)

write_csv(merged_df, "data/pp/transcripts_df_FINAL.csv")
