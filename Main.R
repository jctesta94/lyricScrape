
## !!This code is writen for practice use only!! ##


# ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ##
# Scrape all artists from mainSS page, store.
# loop through list and scrape all album and song pairings, store.
#   will store as accessable album song pair?
# loop through list and pull lyrics for all songs, store.
# all words and letters will be parsed and tallied
# create visuals for data
# ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ##

#loops through all URLs and checks if error out, if error record it and at end pring out precentage of success v failures
#possibly

## Main ##
#######################################################################################
library(tidyverse)
library(rvest)

masterCountList <- data.frame(NA, NA, NA)
names(masterCountList) <- c("Word", "Count", "Instances")

wlPass <- song_grab('https://www.azlyrics.com/lyrics/drake/tuscanleather.html')
dupPass <- get_dupes(wlPass)

#######################################################################################



#Input: Desired Artist
#Output: DF of URLs for all songs
#######################################################################################
hits <- 1


raw <- read_html('https://www.azlyrics.com/d/drake.html')

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


#Input: DF of clean URLs
#Output: ?? of scraped text
###################################################################################
#rawScrape <- c()



#for(i in 1: ) {
#print(read_html(cleanURL[1])[1])



#append(rawScrape, read_html(cleanURL[i]), after = 3) 
#}

#rawScrape

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

## Single Song grab
####################################################################################

song_grab <- function(url)
{
  raw <- read_html(url)

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
  str_remove_all("'") %>%
  str_to_lower()

mainSS <- mainSS %>% 
  select(-rawST)

return(mainSS)
}


## Word Count
##################################################################################################
#Could also look for how many times words are duplicated across songs

#For now this function will work for a single song pass.
# When connecting all sections, this willl need to become a dictonary that is built and added to from every song. 

get_dupes <- function(wordList)
{
  cleanWords <- as.vector(wordList[,1])

  holdList <- data.frame(count(wordList, vars = cleanWords))
  holdList["TEMP"] <- 1
  names(holdList) <- c("Word", "Count", "Instances")
  dupes <- data.frame(inner_join(holdList, masterCountList, by = "Word"))
  
  return(dupes)
}


new_ML <- function(dupesP) {
  for(i in 1:nrow(dupesP)) {
    dupesP$total <- dupesP[i,2] + dupesP[i,4]
  }
    
}