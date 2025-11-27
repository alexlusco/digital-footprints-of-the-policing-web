pacman::p_load(readr, dplyr, stringr, tidyr)

##### Parse transcripts where possible

hearings_df <- read_rds("data/pp/transcript_df.rds")

names_df <- read_csv("data/pp/transcripts_df_anno.csv")

hearings_df <- hearings_df %>%
  mutate(doc_title = str_extract(file_dir, "[^\\/]+$"))

names_df <- names_df %>%
  mutate(doc_title = paste0(doc_title, ".pdf"))

merge_df <- hearings_df %>%
  left_join(names_df, by = "doc_title")

merge_df <- merge_df %>% filter(!is.na(witness_raw))

merge_df <- merge_df %>% select(-file_dir)

merge_df <- merge_df %>%
  distinct(text_all, .keep_all = TRUE) %>%
  select(-text_paged, word_count_page)

to_sep <- merge_df %>%
  filter(separable == "yes") %>%
  mutate(separator = str_trim(separator))

#tom's more elegant solution:
#to_sep %>%
#  group_by(doc_title) %>% 
#  group_modify(~ separate_rows(.x, text_all, sep = .x$separator))

output <- list()

for(row in 1:nrow(to_sep)){
  
  sep_val <- as.character(to_sep[row, "separator"])
  
  text <- to_sep[row, "text_all"]
  
  text <- separate_rows(text, text_all, sep = sep_val)
  
  if(!is.na(to_sep[row, "separator2"])){
    
    sep_val2 <- as.character(to_sep[row, "separator2"])
    
    text <- separate_rows(text, text_all, sep = sep_val2)
  }
  
  df <- tibble(
    text,
    separator = sep_val
  )
  
  output[[row]] <- df
  
}

output_df <- bind_rows(output)

to_sep_temp <- to_sep %>%
  select(-text_all)

separated_df <- output_df %>%
  left_join(to_sep_temp, by = c("separator"))

no_sep <- merge_df %>%
  filter(separable == "no")

all_data <- bind_rows(no_sep, separated_df)

write_csv(all_data, "data/pp/transcripts_names_separated.csv")


