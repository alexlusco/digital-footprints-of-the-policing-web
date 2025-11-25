library(openxlsx)
library(tidyverse)
library(wesanderson)

total_auths <- openxlsx::read.xlsx("data/raw/electronic_surveillance_auths_2001_2021.xlsx", sheet = "Sheet2")

total_auths <- total_auths %>%
  pivot_longer(`2001`:`2019`, names_to = "year", values_to = "total_authorizations")

p1 <- total_auths %>%
  #filter(type_of_application_made %in% c("Audio S.185 C.C.", "Emergency audio S.188 C.C.")) %>%
  mutate(type_of_application_made = case_when(
    str_detect(type_of_application_made, "Audio|audio") ~ "audio",
    str_detect(type_of_application_made, "Video|video") ~ "video",
    TRUE ~ "renewal"
  )) %>%
  filter(type_of_application_made %in% c("audio")) %>%
  group_by(year) %>%
  summarize(total_authorizations = sum(total_authorizations)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = total_authorizations, fill = type_of_application_made)) +
  #geom_smooth(se = FALSE, colour = "#3B9AB2") +
  geom_col(fill = "#3B9AB2") +
  #geom_col() +
  #scale_fill_manual(values = c("#78B7C5", "#F21A00")) +
  scale_x_continuous(breaks = c(2001, 2005, 2009, 2013, 2017, 2019)) +
  scale_y_continuous(n.breaks = 8) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.grid = element_blank()) +
  #theme(legend.position = "top") +
  labs(x = "", y = "Number of applications",
       title = "Total applications for authorization to use audio surveillance, 2001-2019",
       fill = "")

statute_refs <- openxlsx::read.xlsx("data/raw/electronic_surveillance_auths_2001_2021.xlsx", sheet = "Sheet1")

statute_refs %>% as_tibble() %>% distinct(type_of_offence)

p2 <- statute_refs %>%
  filter(statute == "Criminal Code") %>%
  filter(type_of_offence %in% c("Bribery s.120",
                                "Fraud s.380",
                                "Laundering proceeds of crime s.462.31",
                                "Fraudulent manipulation of stock exchange transactions s.382")) %>%
  ggplot(aes(x = year, y = number_of_authorizations, fill = type_of_offence)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = c(2001, 2005, 2009, 2013, 2017, 2019)) +
  scale_y_continuous(n.breaks = 6) +
  scale_fill_manual(values = c("#EBCC2A", "#78B7C5", "#F21A00", "#3B9AB2"), labels = function(x) str_wrap(x, width = 35)) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(plot.title = element_text(face = "bold")) +
  theme(panel.grid = element_blank()) +
  labs(fill = "",
       x = "", y = "Number of authorizations",
       title = "Select criminal offences specified in eletronic surveillance authorizations, 2001-2019")

cowplot::plot_grid(p1, p2, labels = "AUTO", ncol = 1)

ggsave("figures/electronic_surveillance_auths.pdf", width = 8, height = 5)

statute_refs %>%
  filter(statute == "Criminal Code") %>%
  filter(type_of_offence %in% c("Bribery s.120",
                                "Fraud s.380",
                                "Laundering proceeds of crime s.462.31",
                                "Fraudulent manipulation of stock exchange transactions s.382")) %>%
  group_by(type_of_offence) %>%
  summarize(total = sum(number_of_authorizations))

