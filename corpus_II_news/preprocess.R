# read in libraries
pacman::p_load(tm.plugin.factiva, tm, readr, dplyr, tidytext, textclean, stringr, glue, RNewsflow)

# read in file paths
file_paths <- list.files("data/raw/factiva_articles", pattern = ".html", full.names = TRUE)

# loop over file paths, reading in factiva source files, selecting desired columns, 
list_of_dfs <- list()

for (f in file_paths){
  
  print(glue("Reading {f}..."))
  
  corpus <- Corpus(FactivaSource(f), list(language=NA))
  
  td_corpus <- tidy(corpus)
  
  td_corpus <- td_corpus %>% select(author, datetimestamp, description, heading, text, id, language, origin, edition, section, page, wordcount, publisher)
  
  list_of_dfs[[f]] <- td_corpus
  
}

# join together dataframes in list
df <- list_of_dfs %>%
  bind_rows()

# save back up as csv
write_csv(df, "data/processed/news_article_corpus.csv")

# more pre-processing
unwanted_sections <- c("Arts", "Arts & Life", "AUTO SHOW 2009", "Autos", "Avenue", "Book", "Book Review", "Books", "Comment", "Entertainment",
                       "Entertainment | Toronto Star", "Entertainment News | Toronto Sta", "Facts & Arguments", "Film", "Focus", "Focus Column",
                       "Globe Style", "Globe Television", "Globe Megawheels", "Globe Life", "Have-Your-Say", "HAVE-YOUR-SAY", "Ideas", "Initiatives", 
                       "Issues & Ideas", "Letter", "Letters", "Life", "Life & Arts", "Life | Lifestyle and Living | To", "Movies", "Obituaries",
                       "Opinion", "Opinion | Editorial Coverage | T", "Opinion | Toronto Star", "Post Driving", "Post Homes Magazine", "Post Movies",
                       "Pursuits", "ROB Magazine", "Review", "Science", "Special Report: Post Books", "Sports", "Sports | Toronto Star", "Sports Business",
                       "Sports Column", "Summer Books", "TV", "Travel", "Vitals", "Weekend Post: Books", "Weekend Review", "Weekend Review Column",
                       "Wheels", "World", "World and Comment", "Toronto: Post TV", "Special Section8", "Report on Business: International", 
                       "Saturday Post: Books", "News Index", "International News", "From the Archives", "FP Comment", "Financial Post: Comment",
                       "Toronto: Post Movies", "Toronto: Must See", "Sponsored Sections", "Special Section", "Issues", "New in Homes", "The Globe Review",
                       "The Globe Review 7", "The Globe Review 7 Column", "The Globe Review Column")

df_pp <- df %>%
  # add doc_id variable
  mutate(doc_id = row_number()) %>%
  # remove some of the duplicates that Factiva did not remove (more removed below, using cosine similarity)
  distinct(text, .keep_all = TRUE) %>%
  # remove unwanted IDs from text column
  mutate(text = str_remove_all(text, id)) %>%
  # remove email addresses from text column
  mutate(text = replace_email(text)) %>%
  # remove unwanted leading and trailing white space from text column
  mutate(text = str_trim(text))

# remove additional duplicates using 
df_pp_similar <- df_pp %>%
  mutate(text_pp = str_remove_all(text, "[:punct:]"),
         text_pp = str_to_lower(text_pp),
         text_pp = str_squish(text_pp),
         text_pp = replace_symbol(text_pp)) %>%
  unnest_tokens(word, text_pp, token = "words", to_lower = FALSE) %>%
  group_by(doc_id) %>%
  add_count(word) %>%
  ungroup() %>%
  cast_dfm(doc_id, word, n)

df_pp_similar <-
  # use RNewsflow's delete_duplicates function to remove articles with 80% + cosine similarity from dfm
  delete_duplicates(df_pp_similar, measure = "cosine", similarity = .8, keep='first', tf_idf = TRUE)

df_pp_similar <- 
  # convert back to tidy format
  tidy(df_pp_similar)

df_pp_final <- df_pp_similar %>%
  # conduct inner join on original dataframe to remove duplicates (cosine similarity)
  distinct(document) %>%
  mutate(document = as.integer(document)) %>%
  inner_join(df_pp, by = c("document" = "doc_id"))
  
# remove unwanted sections
df_pp_final <- df_pp_final %>%
  filter(!section %in% unwanted_sections)
  
# remove other unwanted articles, manually identified
df_pp_final <- df_pp_final %>%
  mutate(author_temp = str_to_lower(author)) %>%
  filter(!str_detect(author_temp, "doyle")) %>%
  select(-author_temp)

# filter out long/short articles, keeping all between 10/90 quartile
df_pp_final <- df_pp_final %>%
  filter(between(wordcount, quantile(wordcount, .10), quantile(wordcount, .90)))

# save as csv
write_csv(df_pp_final, "data/processed/news_article_corpus_pp.csv")

