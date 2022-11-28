# load in libraries
pacman::p_load(readr, dplyr, textmineR, stringr, purrr, udpipe, ggdendro, ggplot2, ggdendroplot, mclust, tidytext, furrr)

# load data
pseudo_doc_policing <- read_csv("data/processed/pseudo_docs_policing.csv") %>% filter(jurisdiction != "Quebec")
pseudo_doc_policing_qc <- read_csv("data/out/pseudo_docs_policing_auto_translated.csv")
pseudo_doc_policing_qc2 <- read_csv("data/out/pseudo_docs_policing_auto_translated2.csv")

# merge english and french data
pseudo_doc_policing_qc <- pseudo_doc_policing_qc %>% bind_rows(pseudo_doc_policing_qc2)
pseudo_doc_policing <- full_join(pseudo_doc_policing, pseudo_doc_policing_qc)

#rm(pseudo_doc_policing_qc, pseudo_doc_policing_qc2)

pseudo_doc_policing <- pseudo_doc_policing %>%
  mutate(paragraph = case_when(
    !is.na(paragraph_translated) ~ as.character(paragraph_translated),
    TRUE ~ as.character(paragraph)
  )) %>%
  select(-paragraph_translated) %>%
  mutate(doc_id2 = row_number())

# merge rows index along doc_id
pseudo_doc_policing <- pseudo_doc_policing %>%
  group_by(case_title) %>%
  summarize(paragraph = paste0(paragraph, collapse = " "))

# POS tagging
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
#df <- udpipe_annotate(ud_model, x = pseudo_doc_policing$paragraph, doc_id = pseudo_doc_policing$case_title)
df <- udpipe_annotate(ud_model, x = pseudo_doc_policing$paragraph, doc_id = pseudo_doc_policing$doc_id2)
df <- as.data.frame(df)

# subset to nouns and verbs
dtf <- subset(df, upos %in% c("NOUN"))
dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")

# cast as dtm
dtm <- document_term_matrix(x = dtf)

# construct the matrix of term counts to get the IDF vector
tf_mat <- TermDocFreq(dtm)

# TF-IDF and cosine similarity
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)

# hyperparameter optimization - elbow method

multiple_totwiths <- sapply(seq(1, 30, 2),
                            function(k){kmeans(cdist, k, nstart = 25, iter.max = 25)$tot.withinss})

plot(seq(1, 30, 2), multiple_totwiths,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# hyperparameter optimization - bayesian inference criterion

d_clust <- Mclust(as.matrix(cdist), G=1:25,
                  modelNames = mclust.options("emModelNames"))

plot(d_clust)

# run k-means cluster analysis (multiple models for convenience / further comparison)
plan(multiprocess)

set.seed(1234)

multiple_models <- tibble(K = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)) %>%
  mutate(kmeans_model = future_map(K, ~kmeans(cdist, centers = ., nstart = 50, iter.max = 15)))

cluster <- multiple_models$kmeans_model[[11]]

# extract top-weighted words for each cluster
clustering <- cluster$cluster

p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]

  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]

  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})

# create a summary table of the top 5 words defining each cluster
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:7 ],
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)

cluster_summary

# visualize as word clouds
wordcloud::wordcloud(words = names(cluster_words[[ 1 ]]),
                     freq = cluster_words[[ 1 ]],
                     max.words = 50,
                     random.order = FALSE,
                     colors = c("#3B9AB2", "#E1AF00", "#EBCC2A"),
                     main = "Top words in cluster 1")

wordcloud::wordcloud(words = names(cluster_words[[ 2 ]]),
                           freq = cluster_words[[ 2 ]],
                           max.words = 50,
                           random.order = FALSE,
                           colors = c("#3B9AB2", "#E1AF00", "#EBCC2A"),
                           main = "Top words in cluster 2")

wordcloud::wordcloud(words = names(cluster_words[[ 3 ]]),
                           freq = cluster_words[[ 3 ]],
                           max.words = 50,
                           random.order = FALSE,
                           colors = c("#3B9AB2", "#E1AF00", "#EBCC2A"),
                           main = "Top words in cluster 3")

wordcloud::wordcloud(words = names(cluster_words[[ 4 ]]),
                           freq = cluster_words[[ 4 ]],
                           max.words = 50,
                           random.order = FALSE,
                           colors = c("#3B9AB2", "#E1AF00", "#EBCC2A"),
                           main = "Top words in cluster 4")

wordcloud::wordcloud(words = names(cluster_words[[ 5 ]]),
                     freq = cluster_words[[ 5 ]],
                     max.words = 50,
                     random.order = FALSE,
                     colors = c("#3B9AB2", "#E1AF00", "#EBCC2A"),
                     main = "Top words in cluster 5")



# rebind clusters with original dtm
og_df <- pseudo_doc_policing

new_df <- tidy(cluster$cluster) %>% rename(doc_id2 = names, cluster = x) %>% mutate(doc_id2 = as.integer(doc_id2))

binded_df <- inner_join(og_df, new_df)

# add words to df, joining on cluster
words <- as_tibble(cluster_summary) %>% rename(cluster_size = size)

binded_df <- left_join(binded_df, words)
