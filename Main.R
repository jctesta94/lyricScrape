## !!This code is writen for practice use only!! ##

# ##
# Scrape all artists from mainSS page, store.
# loop through list and scrape all album and song pairings, store.
#   will store as accessable album song pair?
# loop through list and pull lyrics for all songs, store.
# all words and letters will be parsed and tallied
# create visuals for data
# !Set limiter on this chunk before running, for good measure
# ##

#loops through all URLs and checks if error out, if error record it and at end pring out precentage of success v failures


library(tidyverse)
library(rvest)

#Input: Desired Artist
#Output: DF of URLs for all songs
#######################################################################################
hits <- 1
for(i in 1:hits) {
  raw <- read_html('https://www.azlyrics.com/h/hobojohnson.html')
}

rawSongList <- raw %>% 
  html_nodes("#listAlbum") %>%
  html_text() %>% 
  strsplit(split = "\n") %>% 
  unlist()

rawSongText <- rawSongList[which(substr(rawSongList, 0, 3) == "{s:")]

rawSongText <- rawSongText %>% 
  substr(0, nchar(rawSongText) - 15)

rawSongTextDF <- data.frame(rawSongText)

rawURL <- rawSongTextDF %>%
  separate(rawSongText, into = c(NA, "url"), sep = "h:")

rawURL$url <- rawURL$url %>% 
  substr(4,nchar(rawURL$url))

cleanURL <- paste0("https://www.azlyrics.com", rawURL$url)

cleanURL[length(cleanURL)] <- substr(cleanURL[length(cleanURL)], 0, nchar(cleanURL[length(cleanURL)]) - 2) 

cleanURL


#Input: DF of clean URLs
#Output: ?? of scraped text
###################################################################################
rawScrape <- data.frame()


for(i in 1: nrow(cleanURL)) {
  rawScrape[i] <- read_ho
}



## Single Song grab
####################################################################################
rawST <- raw %>% 
  html_node(xpath = "//html//body//div[5]") %>%  #//div//div[2]//div[5]//text()[1]")
  html_text()

rawST <- substr(rawST, 5, nchar(rawST))

rawST <- str_replace_all(rawST, "[\r\n]", " ")


mainSS <- as.data.frame(rawST) %>% 
  mutate(rawST = strsplit(as.character(rawST), " ")) %>% 
  unnest(rawST)

mainSS$sorted <- mainSS$rawST %>% 
  na.omit() %>% 
  #(-rawST) %>% 
  str_remove_all("([(),])") %>% 
  str_to_lower()

mainSS %>% 
  select(-rawST) #%>% str_remove_all()

##################################################################################################

