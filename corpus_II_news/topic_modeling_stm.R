#read in libraries
# devtools::install_github("mikajoh/tidystm", dependencies = TRUE)
pacman::p_load(here, readr, dplyr, stringr, tidytext, stm, topicmodels, ggplot2, textstem, tidystm, ggraph, igraph, tidygraph, textclean, wesanderson, forcats)

# read in data
news_corpus <- read_csv(here("data/processed/news_article_corpus_pp.csv"))

# a bit of preprocessing
news_corpus <- news_corpus %>%
  mutate(year = as.numeric(str_extract(datetimestamp, "\\d{4}")))

news_corpus <- news_corpus %>%
  mutate(text_pp = str_remove_all(text, "[:punct:]"),
         text_pp = str_to_lower(text_pp),
         text_pp = str_remove_all(text_pp, "[0-9]+"),
         text_pp = str_squish(text_pp),
         text_pp = replace_symbol(text_pp))

news_corpus <- news_corpus %>%
  mutate(text_pp = lemmatize_strings(text_pp))

# process and save the corpus
processed <- textProcessor(news_corpus$text_pp, 
                           metadata = news_corpus, 
                           customstopwords = c("say", "mr", "ms", "mrs", "day", "yesterday", "year",
                                               "canada", "canadian", "also", "get", "good", "go", "one", "like",
                                               "just", "make", "time", "people", "even", "see", "think", "can", "know",
                                               "now", "thing", "may", "do", "come", "story"),
                           lowercase = FALSE,
                           removenumbers = FALSE,
                           stem = FALSE,
                           removestopwords = TRUE,
                           removepunctuation = FALSE,
                           wordLengths = c(3, Inf),
                           verbose = FALSE)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, verbose = FALSE)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# fit to model with 12 topics
model11 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ origin + year,
               K = 11, data = out$meta, init.type = "Spectral", verbose = FALSE)
model12 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ origin + year,
               K = 12, data = out$meta, init.type = "Spectral", verbose = FALSE)
model15 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ origin + year,
               K = 15, data = out$meta, init.type = "Spectral", verbose = FALSE)
model10 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ origin + year,
               K = 10, data = out$meta, init.type = "Spectral", verbose = FALSE)

# plot quotes related to certain topics, taking n most representative documents per topic k
thoughts <- findThoughts(model11, texts=news_corpus$text, topics=7, n=5)$docs[[1]]
plotQuote(thoughts, width=155, maxwidth=500, text.cex=1, main="")

# topic prevalence
plot.STM(model11, "summary", custom.labels = "")

# topic keywords
labelTopics(model11, topics = c(1:11), n=7)
labelTopics(model12, topics = c(1:12), n=7)
labelTopics(model15, topics = c(1:15), n=7)
labelTopics(model10, topics = c(1:10), n=7)

plot.STM(model11, "labels", topics = c(1, 2, 3, 4, 5), label = "frex", n = 10, width = 55)

# regression modeling
prep <- estimateEffect(1:11 ~ origin + s(year), model11, metadata=out$meta, uncertainty="Global") #nsim is defaulted to 25, but on a small model a higher number lead to more consistent results
summary(prep, topics=c(1:11), nsim=1000)# summary of regression on topic 1-3

###########################################
# plot topics by topic terms and prevalence
###########################################

# 'tidy' the results
td_beta <- tidy(model11, matrix = "beta")
td_gamma <- tidy(model11, matrix = "gamma")

# top 10 terms
top_terms <- td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

# plot 20 most probable topics in corpus by mean gamma
gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma)) %>%
  mutate(topic_title = as.character(topic))

# label the topics
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 1"] <- "POLICE CORRUPTION & MISCONDUCT"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 2"] <- "FOREIGN BRIBERY & CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 3"] <- "DIPLOMACY & BILATERAL RELATIONS"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 4"] <- "MILITARY AFFAIRS & CONFLICT"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 5"] <- "PROVINCIAL POLITICAL CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 6"] <- "FEDERAL POLITICAL CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 7"] <- "MUNICIPAL OVERSIGHT & ACCOUNTABILITY"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 8"] <- "LAW & PROSECUTIONS"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 9"] <- "MONEY LAUNDERING & ORGANIZED CRIME"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 10"] <- "STATE-CORPORATE CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 11"] <- "SECURITIES FRAUD"

td_beta$topic[td_beta$topic == "1"] <- "POLICE CORRUPTION & MISCONDUCT"
td_beta$topic[td_beta$topic == "2"] <- "FOREIGN BRIBERY & CORRUPTION"
td_beta$topic[td_beta$topic == "3"] <- "DIPLOMACY & BILATERAL RELATIONS"
td_beta$topic[td_beta$topic == "4"] <- "MILITARY AFFAIRS & CONFLICT"
td_beta$topic[td_beta$topic == "5"] <- "PROVINCIAL POLITICAL CORRUPTION"
td_beta$topic[td_beta$topic == "6"] <- "FEDERAL POLITICAL CORRUPTION"
td_beta$topic[td_beta$topic == "7"] <- "MUNICIPAL OVERSIGHT & ACCOUNTABILITY"
td_beta$topic[td_beta$topic == "8"] <- "LAW & PROSECUTIONS"
td_beta$topic[td_beta$topic == "9"] <- "MONEY LAUNDERING & ORGANIZED CRIME"
td_beta$topic[td_beta$topic == "10"] <- "STATE-CORPORATE CORRUPTION"
td_beta$topic[td_beta$topic == "11"] <- "SECURITIES FRAUD"

#gamma_terms <- gamma_terms %>%
  #mutate(topic_title = recode(topic_title, 
                              #"DIPLOMACY & BILATERAL RELATIONS" = paste0("OTHER( ", bar(x)),
                              #"MUNICIPAL OVERSIGHT & ACCOUNTABILITY" = paste0("OTHER( ", bar(x)),
                              #"POLICE CORRUPTION & MISCONDUCT" = paste0("OTHER( ", bar(x)),
                              #"MILITARY AFFAIRS & CONFLICT" = paste0("OTHER( ", bar(x)))) %>%
  #(topic = recode(topic,
                        #"Topic 3" = "Topics 1,3,4,7",
                        #"Topic 1" = "Topics 1,3,4,7",
                        #"Topic 7" = "Topics 1,3,4,7",
                        #"Topic 4" = "Topics 1,3,4,7")) %>%
  #group_by(topic, topic_title) %>%
  #summarize(gamma = mean(gamma))

gamma_terms %>%
  #mutate(topic = factor(topic, levels = c("Topic 11", "Topic 2", "Topic 10", "Topic 8", "Topic 5", "Topic 6", "Topic 9", "Topics 1,3,4,7"))) %>%
  #mutate(topic = fct_rev(topic)) %>%
  mutate(topic_title = str_to_title(topic_title)) %>%
  ggplot(aes(topic, gamma, label = topic_title, fill = "red")) +
  geom_col(show.legend = FALSE, width = 0.1) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3.5, alpha = 1, color = "black") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, .14),
                     labels = scales::percent_format()) +
  theme_minimal() +
  scale_fill_manual(values = wes_palette(name = "Zissou1")) +
  theme(plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence")

ggsave("figures/topic_prevalence_whole_corpus.pdf", width = 7, height = 3)

###############################################
# plot individual topics by most probable words
###############################################

# plot one topic by most probable terms
td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 1") %>% #beta values for topic 1
  filter(beta > 0.003) %>% #only plot word probabilities higher than 0.003 for topic 1
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta),
       title = "Word probabilities for Topic 1")

td_beta %>%
  #mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic) |> 
  top_n(beta, n = 10) |> 
  ungroup() |> 
  ggplot(aes(term, beta, fill = "black")) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(x = "Terms", y = expression(beta),
       title = "Top 10 most weighted words for each topic") +
  facet_wrap(~topic, scales = "free", ncol = 3) +
  theme_minimal() +
  scale_fill_manual(values = wes_palette(name = "Zissou1")) +
  theme(plot.title = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm")) +
  theme(panel.grid.minor = element_blank())

ggsave("figures/top_10_weighted_words_every_topic.pdf", width = 12, height = 12)

###################################################
# plot topic prevalence (point estimates) over time
###################################################

tidyprep <- extract.estimateEffect(prep, "year", model = model11, method = "pointestimate")

# label the topics
tidyprep$label <- gsub("\\(.*", "", tidyprep$label)
tidyprep$label[tidyprep$label == "Topic 1"] <- "POLICE CORRUPTION & MISCONDUCT"
tidyprep$label[tidyprep$label == "Topic 2"] <- "FOREIGN BRIBERY & CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 3"] <- "DIPLOMACY & BILATERAL RELATIONS"
tidyprep$label[tidyprep$label == "Topic 4"] <- "MILITARY AFFAIRS & CONFLICT"
tidyprep$label[tidyprep$label == "Topic 5"] <- "PROVINCIAL POLITICAL CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 6"] <- "FEDERAL POLITICAL CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 7"] <- "MUNICIPAL OVERSIGHT & ACCOUNTABILITY"
tidyprep$label[tidyprep$label == "Topic 8"] <- "LAW & PROSECUTIONS"
tidyprep$label[tidyprep$label == "Topic 9"] <- "MONEY LAUNDERING & ORGANIZED CRIME"
tidyprep$label[tidyprep$label == "Topic 10"] <- "STATE-CORPORATE CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 11"] <- "SECURITIES FRAUD"

facet_line <- tidyprep %>%
  group_by(label) %>%
  mutate(facet_intercept = case_when(
    estimate == max(estimate) ~ covariate.value
  )) %>%
  mutate(facet_intercept = max(facet_intercept, na.rm = TRUE))

facet_line %>%
  filter(label %in% c("MONEY LAUNDERING & ORGANIZED CRIME", "STATE-CORPORATE CORRUPTION", "SECURITIES FRAUD")) %>%
  mutate(label = str_to_title(label)) %>%
  arrange(label) %>%
  ggplot(aes(x = covariate.value, y = estimate)) +
  geom_line(aes(colour = "variable"), size = 1, show.legend = FALSE) +
  geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper), linetype = 2, alpha = 0.1) +
  facet_wrap(~label, ncol = 3, labeller = label_wrap_gen()) +
  geom_vline(aes(xintercept = facet_intercept), size = 1, colour = "#F21A00", linetype = "dotted") +
  scale_x_continuous(breaks = seq(2001, 2021, by = 4)) +
  labs(y = "Expected topic proportion",
       x= "",
       title = "Topic prevalence over time, 2001-2021") + #covers period 2001-01-01 to 2021-07-11
  theme_minimal() +
  scale_colour_manual(values = wes_palette(name = "Zissou1")) +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm")) +
  theme(panel.grid.minor = element_blank())

ggsave(here::here("figures", "topics_prevalence_over_time_THREE.pdf"), width = 8, height = 3)

#####################################################
# plot topic prevalence (point estimates) by publisher
#####################################################

tidyprep <- extract.estimateEffect(prep, "origin", model = model11, method = "pointestimate")

summary(prep)

# label the topics
tidyprep$label <- gsub("\\(.*", "", tidyprep$label)
tidyprep$label[tidyprep$label == "Topic 1"] <- "POLICE CORRUPTION & MISCONDUCT"
tidyprep$label[tidyprep$label == "Topic 2"] <- "FOREIGN BRIBERY & CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 3"] <- "DIPLOMACY & BILATERAL RELATIONS"
tidyprep$label[tidyprep$label == "Topic 4"] <- "MILITARY AFFAIRS & CONFLICT"
tidyprep$label[tidyprep$label == "Topic 5"] <- "PROVINCIAL POLITICAL CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 6"] <- "FEDERAL POLITICAL CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 7"] <- "MUNICIPAL OVERSIGHT & ACCOUNTABILITY"
tidyprep$label[tidyprep$label == "Topic 8"] <- "LAW & PROSECUTIONS"
tidyprep$label[tidyprep$label == "Topic 9"] <- "MONEY LAUNDERING & ORGANIZED CRIME"
tidyprep$label[tidyprep$label == "Topic 10"] <- "STATE-CORPORATE CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 11"] <- "SECURITIES FRAUD"

tidyprep %>%
  filter(label %in% c("MONEY LAUNDERING & ORGANIZED CRIME", "STATE-CORPORATE CORRUPTION", "SECURITIES FRAUD")) %>%
  mutate(label = str_to_title(label)) %>%
  mutate(covariate.value = str_remove(covariate.value, "The ")) %>%
  ggplot(aes(x = covariate.value, y = estimate, ymin = ci.lower, ymax = ci.upper)) +
  geom_hline(yintercept = 0, colour = "#F21A00", lty = 2) +
  geom_linerange(aes(colour = "variable3"), size = 1, show.legend = FALSE) +
  geom_point(aes(colour = "variable2"), size = 3, show.legend = FALSE) +
  facet_wrap(~label, ncol = 3, labeller = label_wrap_gen()) +
  coord_flip() +
  theme_minimal() +
  scale_colour_manual(values = wes_palette(name = "Zissou1")) +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.grid.major.y = element_blank()) +
  labs(title = "Estimated effect of news source on topic prevalence", x = "", y = "Regression coefficient")

ggsave(here::here("figures", "news_source_coefficients_THREE.pdf"), width = 8, height = 3)


#############################
# plot topics as corr network
#############################

tidyprep <- extract.estimateEffect(prep, "year", model = model8, method = "pointestimate")

# label the topics
tidyprep$label <- gsub("\\(.*", "", tidyprep$label)
tidyprep$label[tidyprep$label == "Topic 1"] <- "POLICE CORRUPTION & MISCONDUCT"
tidyprep$label[tidyprep$label == "Topic 2"] <- "FOREIGN BRIBERY & CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 3"] <- "DIPLOMACY & BILATERAL RELATIONS"
tidyprep$label[tidyprep$label == "Topic 4"] <- "MILITARY AFFAIRS & CONFLICT"
tidyprep$label[tidyprep$label == "Topic 5"] <- "PROVINCIAL POLITICAL CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 6"] <- "FEDERAL POLITICAL CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 7"] <- "MUNICIPAL OVERSIGHT & ACCOUNTABILITY"
tidyprep$label[tidyprep$label == "Topic 8"] <- "LAW & PROSECUTIONS"
tidyprep$label[tidyprep$label == "Topic 9"] <- "MONEY LAUNDERING & ORGANIZED CRIME"
tidyprep$label[tidyprep$label == "Topic 10"] <- "PROCUREMENT CORRUPTION"
tidyprep$label[tidyprep$label == "Topic 11"] <- "SECURITIES FRAUD"

corr_network_labels <- tidyprep %>%
  select(label) %>%
  distinct(label)

stm_corrs <- stminsights::get_network(model = model11,
                                      method = 'simple',
                                      #labels = paste(1:20),
                                      labels = paste(corr_network_labels$label),
                                      cutoff = 0.05,
                                      cutiso = FALSE)

p1 <- stm_corrs %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_fast_greedy())) %>% 
  ggraph(layout = "fr") +
  geom_node_point(aes(size = props, colour = community), alpha =.3, show.legend = FALSE)  +
  geom_edge_link(aes(edge_width = weight, label = weight), edge_colour = 'grey', alpha = 0.75, show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = FALSE, size = 3.5, alpha = 1, fontface = "italic", check_overlap = TRUE, show.legend = FALSE) +
  scale_size(range = c(5, 25), labels = scales::percent) +
  theme_bw() +
  #labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') +
  scale_edge_width(range = c(1, 5)) +
  #theme_graph(base_family="sans") +
  labs(title = "Topic correlations")

p2 <- p1 + expand_limits(x = c(172, 178), y = c(169, 176)) + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.grid = element_blank()) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here::here("figures", "topics_correlation_network.pdf"), width = 12)

#####################################
# sample texts for abductive analysis
#####################################

td_gamma <- tidy(model11, matrix = "gamma")

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma)) %>%
  mutate(topic_title = as.character(topic)) %>%
  ungroup()

gamma_terms$topic_title[gamma_terms$topic_title == "Topic 1"] <- "POLICE CORRUPTION & MISCONDUCT"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 2"] <- "FOREIGN BRIBERY & CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 3"] <- "DIPLOMACY & BILATERAL RELATIONS"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 4"] <- "MILITARY AFFAIRS & CONFLICT"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 5"] <- "PROVINCIAL POLITICAL CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 6"] <- "FEDERAL POLITICAL CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 7"] <- "MUNICIPAL OVERSIGHT & ACCOUNTABILITY"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 8"] <- "LAW & PROSECUTIONS"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 9"] <- "MONEY LAUNDERING & ORGANIZED CRIME"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 10"] <- "PROCUREMENT CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 11"] <- "SECURITIES FRAUD"

article_sample <- gamma_terms %>%
  filter(topic_title %in% c("LAW & PROSECUTIONS", 
                            "MONEY LAUNDERING & ORGANIZED CRIME", 
                            "PROCUREMENT CORRUPTION", 
                            "SECURITIES FRAUD")) %>%
  group_by(topic) %>%
  arrange(desc(gamma)) %>%
  slice(1:75)

news_corpus <- news_corpus %>%
  mutate(document = row_number())

article_sample <- article_sample %>%
  # fix document variable before joining
  inner_join(news_corpus, by = "document")

setwd(here::here("data/out/article_sample_July28_2021"))

article_sample %>%
  ungroup() %>%
  mutate(datetimestamp = as.Date(datetimestamp)) %>%
  mutate(origin = str_to_upper(origin)) %>%
  mutate(heading = str_to_upper(heading)) %>%
  mutate(topic_title = str_to_upper(topic_title)) %>%
  select(document, topic_title, origin, datetimestamp, author, heading, text) %>%
  group_by(heading) %>%
  do(write_delim(., paste0(.$datetimestamp, "-", .$topic_title, "-", .$document, ".txt"), col_names = FALSE, delim = "\n", quote_escape = FALSE))

write_csv(article_sample, "article_sample_df.csv")

##################################################################
# Export corpus with per-term-per-document-probailities and labels
##################################################################

td_gamma <- tidy(model11, matrix = "gamma")

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma)) %>%
  mutate(topic_title = as.character(topic)) %>%
  ungroup()

gamma_terms$topic_title[gamma_terms$topic_title == "Topic 1"] <- "POLICE CORRUPTION & MISCONDUCT"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 2"] <- "FOREIGN BRIBERY & CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 3"] <- "DIPLOMACY & BILATERAL RELATIONS"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 4"] <- "MILITARY AFFAIRS & CONFLICT"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 5"] <- "PROVINCIAL POLITICAL CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 6"] <- "FEDERAL POLITICAL CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 7"] <- "MUNICIPAL OVERSIGHT & ACCOUNTABILITY"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 8"] <- "LAW & PROSECUTIONS"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 9"] <- "MONEY LAUNDERING & ORGANIZED CRIME"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 10"] <- "STATE-CORPORATE CORRUPTION"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 11"] <- "SECURITIES FRAUD"

news_corpus <- news_corpus %>%
  mutate(document = row_number())

news_corpus_stm <- news_corpus %>%
  left_join(gamma_terms, by = "document")

write_csv(news_corpus_stm, here::here("data/processed/news_corpus_stm.csv"))

