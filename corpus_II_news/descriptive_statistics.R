# load libraries
pacman::p_load(ggplot2, dplyr, stringr, readr, wesanderson, cowplot)

# read in data - original and preprocessed
news_corpus <- read_csv("data/processed/news_article_corpus.csv")
news_corpus_pp <- read_csv("data/processed/news_article_corpus_pp.csv")
news_article_sample <- read_csv("data/out/article_sample_July28_2021/article_sample_df.csv")

# count number of articles by news source
news_corpus_pp %>%
  group_by(origin) %>%
  count()

# plot number of articles over time, counting total per year
news_corpus_pp %>%
  mutate(year = as.numeric(str_extract(datetimestamp, "\\d{4}"))) %>%
  group_by(origin, year) %>%
  count() %>%
  ggplot(aes(x = year, y = n, fill = origin)) +
  geom_col(position = "stack") +
  #geom_smooth() + 
  scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values = wes_palette(name = "Zissou1")) +
  labs(title = "Total news articles in corpus by year, 2001-2021",
       #subtitle = "2001-01-01 to 2021-07-11",
       x = "", y = "Number of documents",
       fill = "")

ggsave("figures/number_of_news_articles_over_time.pdf", width = 8, height = 3)

# calculate min / max and mean article length
quantile(news_corpus_pp$wordcount)
mean(news_corpus_pp$wordcount)

# density plot distributions of word count
p1 <- news_corpus_pp %>%
  ggplot(aes(x = wordcount, fill = "variable")) +
  geom_histogram(show.legend = FALSE) +
  theme_minimal() +
  scale_fill_manual(values = wes_palette(name = "Zissou1")) +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.grid = element_blank()) +
  labs(title = "",
       subtitle = "After normalization",
       y = "",
       x = "Word count")

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

p2 <- news_corpus %>%
  distinct(text, .keep_all = TRUE) %>%
  filter(!section %in% unwanted_sections) %>%
  ggplot(aes(x = wordcount, fill = "variable")) +
  geom_histogram(show.legend = FALSE) +
  theme_minimal() +
  scale_fill_manual(values = wes_palette(name = "Zissou1")) +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.grid = element_blank()) +
  labs(title = "Article word count distributions",
       subtitle = "Before normalization",
       y = "Frequency",
       x = "Word count")

cowplot::plot_grid(p2, p1, labels = "AUTO")

ggsave("figures/word_count_news_articles.pdf", width = 12)

# calculate total number of words (before removing stop words, numbers)
news_corpus_pp %>% summarize(sum(wordcount))

# plot descriptive characteristics of article sub-sample used in qualitative analysis
news_article_sample %>%
  mutate(year = as.numeric(str_extract(datetimestamp, "\\d{4}"))) %>%
  group_by(origin, year) %>%
  count() %>%
  ggplot(aes(x = year, y = n, fill = origin)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values = wes_palette(name = "Zissou1")) +
  labs(title = "Distribution of news articles over time in 300 article subset",
       x = "", y = "Number of documents",
       fill = "")

ggsave("figures/number_of_sampled_news_articles_over_time.pdf", width = 8)

