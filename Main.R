## This code is writen for practice use only

library(tidyverse)
library(rvest)

hits <- 1
for(i in 1:hits) {
  raw <- read_html('https://www.azlyrics.com/lyrics/shakeygraves/rollthebones.html')
}





# ##
# Scrape all artists from main page, store
# loop through list and scrape all album and song pairings, store
#   will store as accessable album song pair?
# loop through list and pull lyrics for all songs, store
# all words and letters will be parsed and tallied
# create visuals for data
# !Set limiter on this chunk before running, for good measure
# ##

rawST <- raw %>% 
  html_node(xpath = "//html//body//div[5]") %>%  #//div//div[2]//div[5]//text()[1]")
  html_text()

rawST <- str_replace_all(rawST, "[\r\n]", " ")


main <- as.data.frame(rawST) %>% 
  mutate(rawST = strsplit(as.character(rawST), " ")) %>% 
  unnest(rawST)

main$sorted <- main$rawST %>% 
  na.omit() %>% 
  #(-rawST) %>% 
  str_remove_all("([(),])")


rawST