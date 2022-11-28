# LOAD IN LIBRARIES
pacman::p_load(here, readtext, dplyr, stringr, readr)

# READ IN RAW DATA
ml_court_decisions <- readtext(paste0("data/raw/laundering-FINAL/", "*.docx"),
                               docvarsfrom = "filenames",
                               docvarnames = c("case_title", "jurisdiction", "year"),
                               dvsep = "_") %>%
  mutate(year = as.integer(year))

sf_court_decisions <- readtext(paste0("data/raw/capital-markets-FINAL/", "*.DOCX"),
                               docvarsfrom = "filenames",
                               docvarnames = c("case_title", "jurisdiction", "year"),
                               dvsep = "_")

cb_court_decisions <- readtext(paste0("data/raw/corruption-FINAL/", "*.docx"),
                              docvarsfrom = "filenames",
                              docvarnames = c("case_title", "jurisdiction", "year"),
                              dvsep = "_")

all_court_decisions <- ml_court_decisions %>% bind_rows(sf_court_decisions, cb_court_decisions)

# DATA PARSING/EXTRACTION
all_court_decisions_pp <- all_court_decisions

all_court_decisions_pp <- all_court_decisions_pp %>% mutate(text = str_trim(text))

# SAVE RESULTS
write_csv(all_court_decisions_pp, "data/processed/all_court_decisions_pp.csv")
