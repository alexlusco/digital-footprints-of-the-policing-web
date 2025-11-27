pacman::p_load(readr, dplyr, stringr, tidyr, textnets, htmlwidgets, tibble, textstem, broom)

#transcripts <- read_rds("data/pp/transcripts_names_questions.rds")
transcripts <- read_csv("data/pp/transcripts_df_FINAL.csv")

#transcripts <- transcripts %>%
#  mutate(witness_surname = str_extract(witness_raw, "^(.+?)(?=,)")) #grabs only first surname in combined interviews

transcripts <- transcripts %>%
  mutate(interview_id = row_number()) |> 
  mutate(interview_id = ifelse(police == "yes", paste0(interview_id, "_police"), interview_id))

transcripts_nouns <- PrepText(transcripts, 
                              groupvar = "interview_id",
                              textvar = "text_all",
                              node_type = "groups",
                              #node_type = "words",
                              tokenizer = "words",
                              pos = "nouns",
                              remove_stop_words = TRUE,
                              compound_nouns = TRUE
                              )

names_to_drop <- transcripts |> 
  mutate(names = str_extract(witness_raw, "^[^(]*")) |> 
  separate(names, into = c("name1", "name2", "name3")) |> 
  select(name1:name3) |> 
  pivot_longer(name1:name3) |> 
  select(-name) |> 
  distinct(value) |> 
  filter(!str_count(value) < 1) |> 
  mutate(value = str_to_lower(value)) |> 
  filter(!value %in% c("deputy", "supt", "sgt"))

names_to_drop <- as.character(names_to_drop$value)

# Combine the names into a regex pattern with word boundary anchors
names_to_drop <- paste("\\b", paste(names_to_drop, collapse = "\\b|\\b"), "\\b", sep = "")

top_words_modularity_classes_1 <- top_words_modularity_classes

transcripts_nouns_filtered <- transcripts_nouns |> 
  filter(!str_detect(lemma, names_to_drop)) |> 
  anti_join(top_words_modularity_classes_1, by = "lemma") |> 
  filter(!lemma %in% c("christopher mullin", "ezekiel chhoa", "joel hussey", "leuprecht", "nettleton", "spiro", "tweel", "butler", "kirkland morris", "robinson", "pratte", "gile dixon", "helena wood", "steven", "jb", "jp", "brown", "carlos", "gardner")) |> 
  filter(!lemma %in% c("carol prest", "joshua gordon", "arthur cockfield", "johanne", "jaffe", "bruce wallace", "j milloy")) |> 
  filter(!lemma %in% c("christopher elgar", "bekkering", "ferris", "martland", "freda")) |> 
  filter(!lemma %in% c("rideout", "gibbon", "carter", "aaron gilke", "hussey", "alexon bell", "stewart", "lisa liu", "laporte"))

transcripts_network <- CreateTextnet(transcripts_nouns)
#transcripts_network <- CreateTextnet(transcripts_nouns_filter)

###############

VisTextNet(transcripts_network, label_degree_cut = 0, betweenness = FALSE)

###############

set.seed(123)

transcripts_communities <- TextCommunities(transcripts_network) |> as_tibble()

transcripts_communities |> 
  as_tibble() |> 
  mutate(police = ifelse(str_detect(group, "police"), 1, 0)) |> 
  janitor::tabyl(modularity_class, police) |> 
  janitor::adorn_totals("col") |> 
  janitor::adorn_percentages() |> 
  janitor::adorn_pct_formatting(1) |> 
  janitor::adorn_ns()

top_words_modularity_classes <- InterpretText(transcripts_network, transcripts_nouns_filtered)
#top_words_modularity_classes <- InterpretText(transcripts_network, transcripts_nouns_filter)

text_centrality <- TextCentrality(transcripts_network) |> rownames_to_column(var = "group") |> as_tibble()

transcript_metrics <- transcripts_communities |> 
  left_join(text_centrality, by = "group") 

transcripts_all <- transcripts |> 
  left_join(transcript_metrics, by = c("interview_id" = "group"))

transcripts_all |> 
  select(interview_id, witness_raw, betweenness_centrality) |> 
  arrange(-betweenness_centrality) |> 
  mutate(quartile = ntile(betweenness_centrality, 4)) |> 
  group_by(quartile) |> 
  mutate(total_cfpo = sum(str_count(interview_id, "police")),
         total_interviews = n(),
         total_non_cfpo = total_interviews - total_cfpo,
         prop_cfpo = total_cfpo/total_interviews*100,
         prop_non_cfpo = total_non_cfpo/total_interviews*100) |> 
  distinct(prop_cfpo, prop_non_cfpo)
  
transcripts_all |> 
  arrange(-betweenness_centrality) |> 
  mutate(police = ifelse(str_detect(interview_id, "police"), "CFPO", "Non-CFPO")) |> 
  print(n = 20) |> 
  filter(str_detect(interview_id, "police")) |> 
  select(witness_raw, betweenness_centrality)

################
top10_word_lists <- top_words_modularity_classes |> 
  filter(!lemma %in% c("liu", "chhoa", "peddle", "cameron", "nash", "morhart", "chewka", "fiod", "wong", "mistry", "baron")) |> 
  group_by(modularity_class) |> 
  distinct(lemma, .keep_all = TRUE) |> 
  slice_max(n = 10, order_by = tf_idf) |> 
  ungroup()

list_of_words <- list()

for(m in unique(top10_word_lists$modularity_class)) {
  
  df <- filter(top10_word_lists, modularity_class == m)
  
  list_m <- as.character(df$lemma)
  
  list_of_words[[m]] <- list_m
  
}

list_of_words

###############

# augment with count of 'resource talk'
resource_word_list <- c("resource", "resources", "resourced", "resourcing", "staff", "staffing", "staffed", "support", "supporting", "supported", "supports", "budget", "budgeted", "budgeting", "allocation", "allocated", "allocating", "understaff", "under-staff", "under staff", "understaffed", "under-staffed", "under staffed", "short-staffed", "short staffed", "insufficient", "inadequate", "lack of resources", "lack-of-resources", "resource constraints", "resource-constraints", "staff shortages", "staff-shortages", "under-resourced", "under resourced", "budgetary constraints", "budgetary-constraints", "inadequate funding", "overworked", "overburdened", "strained resources", "strained-resources", "resource limitations", "resource-limitations", "resource scarcity", "resource-scarcity", "resource allocation", "resource-allocation", "limited manpower", "manpower", "inefficient staffing", "inefficient-staffing", "resource-intensive", "resource intensive", "insufficient budget", "insufficient-budget", "utilization", "utilizations", "utilize", "utilizes", "utilizing", "utilized", "sourcing", "sourced", "sources", "expansion", "expanded", "expanding", "sustainability")

regex_string <- paste(resource_word_list, collapse = "|")

transcripts_all <- transcripts_all |> 
  mutate(resource_talk = str_count(str_to_lower(text_all), regex_string),
         word_count = str_count(str_to_lower(text_all), "\\w+"))

transcripts_all |> 
  mutate(resource_prop = resource_talk/word_count*100) |> 
  summarize(resource_prop_ave = round(mean(resource_prop), 2),
            .by = modularity_class)

transcripts_all |> 
  mutate(police = ifelse(str_detect(interview_id, "police"), 1, 0)) |> 
  summarize(ave_betweenness = mean(betweenness_centrality),
            ave_centrality = mean(closness_centrality),
            .by = police)

transcripts_all |> 
  summarize(ave_betweenness = mean(betweenness_centrality),
            median_betweenness = median(betweenness_centrality),
            .by = police)

summary(lm(betweenness_centrality ~ as.factor(police), data = transcripts_all))

##############
# regression analysis
###############

# factorize police v$ariable
transcripts_all <- transcripts_all |> 
  mutate(police = as.factor(ifelse(police == "yes", 1, 0)))

# rename modularity_class to community and factorize
transcripts_all <- transcripts_all |> 
  #rename(community = modularity_class) |> 
  mutate(community = as.factor(community)) |> 
  mutate(community = factor(community, levels = c("1", "2", "3", "4", "5", "6", "7", "8"))) |> 
  mutate(community = relevel(community, ref = "1"))

# rename police to cfpo
transcripts_all <- transcripts_all |> 
  rename(CFPO = police)

#fit1 <- lm(resource_talk ~ CFPO, data = transcripts_all)

fit1 <- lm(resource_talk ~ CFPO + word_count, data = transcripts_all)

fit2 <- lm(resource_talk ~ CFPO + word_count + betweenness_centrality, data = transcripts_all)

fit3 <- lm(resource_talk ~ CFPO + word_count + betweenness_centrality + community, data = transcripts_all)

jtools::export_summs(fit1, fit2, fit3, fit4, scale = TRUE, error_format = "[{conf.low}, {conf.high}]")

jtools::plot_summs(fit1, fit2, fit3)

stargazer::stargazer(fit1, fit2, fit3)

###############
# community composition analysis
###############

community_comp_tbl <- transcripts_all |> 
  janitor::tabyl(modularity_class, police) |> 
  janitor::adorn_totals("col") |> 
  janitor::adorn_percentages() |> 
  janitor::adorn_pct_formatting(1) |> 
  janitor::adorn_ns()

community_comp_fisher <- transcripts_all |> 
  janitor::tabyl(modularity_class, police)

janitor::fisher.test(community_comp_fisher, simulate.p.value = TRUE)

###############

# explore texts with resource vs no resource talk using tf-idf

#lemmatize_resource_words <- unique(sapply(resource_word_list, lemmatize_strings))

#text_centrality |> 
#  rownames_to_column(var = "word") |> 
#  as_tibble() |> 
#  arrange(-betweenness_centrality) |>
#  filter(betweenness_centrality > 1) |> 
#  mutate(word = forcats::fct_reorder(word, betweenness_centrality)) |> 
#  ggplot(aes(x = word, y = betweenness_centrality, label = ifelse(word %in% lemmatize_resource_words, as.character(word), ""), alpha = 0.1)) + 
#  geom_col(show.legend = FALSE) +
#  ggrepel::geom_text_repel(max.overlaps = Inf, alpha = 1) +
#  scale_y_log10() +
#  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
#  labs(title = "Betweenness centrality scores for every word appearing in oral hearing transcripts")
#filter(word %in% lemmatize_resource_words)

# develop 'resource talk' dictionary
#transcript_tokens <- transcript_communities |>
#  separate_longer_delim(text_all, delim = "A  ") |> 
#  mutate(resource_flag = str_detect(str_to_lower(text_all), regex_string)) |> 
#  tidytext::unnest_tokens(word, text_all, token = "words", to_lower = TRUE) |> 
#  filter(!str_detect(word, "[0-9]+")) |> 
#  filter(!word %in% tidytext::stop_words$word)

#paragraph_words <- transcript_tokens |> 
#  count(resource_flag, word, sort = TRUE)

#total_words <- paragraph_words |>  
#  group_by(resource_flag) |>  
#  summarize(total = sum(n)) |> 
#  ungroup()

#paragraph_words <- left_join(paragraph_words, total_words)

#paragraph_words |> 
#  tidytext::bind_tf_idf(word, resource_flag, n) |> 
#  select(resource_flag, word, tf_idf) |> 
#  group_by(resource_flag) |> 
#  slice_max(n = 200, order_by = tf_idf) |> 
#  ungroup() |> 
#  filter(resource_flag == TRUE) |> 
#  view()
