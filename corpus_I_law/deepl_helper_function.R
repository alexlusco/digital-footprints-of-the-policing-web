pacman::p_load(readr, dplyr, RSelenium, rvest)

driver <- rsDriver(browser=c("firefox"), port= 4833L)
remote_driver <- driver[["client"]]
remote_driver$open()

url <- "https://www.deepl.com/translator#fr/en/"

output_list <- list()

for(p in df$paragraph){

  remote_driver$navigate(url)

  Sys.sleep(sample(2:4, 1))

  input_box <- remote_driver$findElement(using = "css selector", value = ".lmt__source_textarea")

  input_box$sendKeysToElement(list(p))

  Sys.sleep(sample(2:4, 1))

  output <- read_html(remote_driver$getPageSource()[[1]])

  output <- output %>%
    html_node("#target-dummydiv") %>%
    html_text()

  currdata <- tibble::tibble(
    paragraph = unlist(p),
    paragraph_translated = output
  )

  output_list[[p]] <- currdata

  Sys.sleep(sample(10:20, 2))
}
