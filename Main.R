library(tidyverse)
library(rvest)

main <- read_html('https://www.azlyrics.com/lyrics/hozier/movement.html')



rawST <- main %>% 
  html_node(xpath = "//html//body//div[5]") %>%  #//div//div[2]//div[5]//text()[1]")
  html_text()

rawST <- str_replace_all(rawST, "[\r\n]", " ")


rawST