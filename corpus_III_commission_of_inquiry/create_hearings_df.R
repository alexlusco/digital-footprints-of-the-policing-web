pacman::p_load(readr, pdftools, dplyr, tibble, stringr, purrr)

file_paths <- dir("data/raw/transcripts", pattern = ".pdf$", full.names = TRUE)

#file_paths <- file_paths[1:5]

output <- list()

for(f in file_paths){
  
  doc_title <- str_extract(f, "([^\\/]+$)") 
  doc_title <- str_remove(doc_title, ".pdf")
  
  text <- pdf_text(f)
  text_paged <- str_trim(text)
  text_all <- paste0(text_paged, collapse = " ")
  
  word_count_page <- str_count(text_paged, "\\w+")
  word_count_all <- str_count(text_all, "\\w+")
  
  currdata <- tibble(
    doc_title = doc_title,
    text_paged = text_paged,
    text_all = text_all,
    file_dir = f,
    word_count_page = word_count_page,
    word_count_all = word_count_all
  ) %>%
    mutate(page_num = row_number(),
           total_pages = max(page_num))
  
  output[[f]] <- currdata
}

joined_df <- bind_rows(output)

write_rds(joined_df, "data/pp/transcript_df.rds")

#### create another df with only doc ids for annotating

joined_df_anno <- joined_df %>%
  select(doc_title) %>%
  distinct(doc_title) %>%
  write_excel_csv("data/pp/transcripts_df_anno.csv")




                      