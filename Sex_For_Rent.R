################
#Load libraries#
################
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(textclean)
library(httr)
library(readr)
library(tidyverse)
library(tidytext)
library(ggmap) #for map
library(ggrepel) #for map

#############################################
#Create list of results urls from query list#
#############################################

craigslist_start_url <-
  'https://london.craigslist.co.uk/search/hhh?query='
craigslist_end_url <-
  '&sort=rel&searchNearby=2&nearbyArea=494&nearbyArea=72&nearbyArea=398&nearbyArea=117&nearbyArea=312&nearbyArea=116&nearbyArea=495&nearbyArea=496&nearbyArea=399&nearbyArea=402&nearbyArea=400&nearbyArea=497&nearbyArea=403&nearbyArea=493&nearbyArea=71&nearbyArea=163&nearbyArea=492&nearbyArea=211&nearbyArea=401&availabilityMode=0&sale_date=all+dates'
query <-
  c('sex',
    'female',
    'free',
    'fwb',
    'fun',
    'intimacy',
    'benefits',
    'erotic') #List of querys

querylist <-
  vector("list", length = length(query)) #Creates empty list

for (i in seq_along(query)) {
  query_url <-
    paste(craigslist_start_url, query[i], craigslist_end_url, sep = "")
  querylist[i] <- query_url
} #Iterates along the query list to create url list called querylist

#################################################
#Create list of paginated results for each query#
#################################################

pagenum <- "&s="  #Adds in the separator
pagelist <- list()  #Creates empty list
#iterate along list and add extra pages when they exist
for (i in seq_along(querylist)) {
  resultspage <- 0
  repeat {
    url <- paste(querylist[i], pagenum, resultspage, sep = "")
    nomore <-
      read_html(url) %>%
      html_nodes(xpath = '//*[@id="searchform"]/div[3]/div[3]/span[2]/span') %>% #this element only equals "no results" when there are no results
      html_text()
    
    resultspage = resultspage + 120 #each page has 120 max results
    if (nomore == "no results")
      break #ends repeat loop when there are no results
    pagelist <- c(pagelist, url) #adds url to empty list
  }
}
pagelist

#this was helpful https://towardsdatascience.com/web-scraping-tutorial-in-r-5e71fd107f32
#set url to check below works for individual page
#url <- "https://london.craigslist.co.uk/search/hhh?query=female&sort=rel&searchNearby=2&nearbyArea=494&nearbyArea=72&nearbyArea=398&nearbyArea=117&nearbyArea=312&nearbyArea=116&nearbyArea=495&nearbyArea=496&nearbyArea=399&nearbyArea=402&nearbyArea=400&nearbyArea=497&nearbyArea=403&nearbyArea=493&nearbyArea=71&nearbyArea=163&nearbyArea=492&nearbyArea=211&nearbyArea=401&availabilityMode=0&sale_date=all+dates"


######################################################
#Iterate through result page URLS and scrape each one#
######################################################

#apply scrape function to each url
FinalTable <- lapply(pagelist, function(url) {
  #get list of result blocks for each url
  results <- read_html(url) %>% html_nodes("li.result-row")
  
  #create empty list
  records <- vector("list", length = length(results))
  
  #get different elements from within for each result block
  #if the element doesn't exist, put NA to keep the lines even
  
  for (i in seq_along(results)) {
    if (length(results[i] %>% html_nodes("time.result-date") %>% html_attr("datetime")) <= 0) {
      date <-
        NA
    } else {
      date <-
        results[i] %>% html_nodes("time.result-date") %>% html_attr("datetime") %>% as.Date(format = "%Y-%m-%d %H:%M")
    }
    if (length(results[i] %>% html_nodes("a.result-title") %>% html_attr("href")) <= 0) {
      link <-
        NA
    } else {
      link <-
        results[i] %>% html_nodes("a.result-title") %>% html_attr("href")
    }
    if (length(results[i] %>% html_nodes("a.result-title") %>% html_text()) <= 0) {
      title <-
        NA
    } else {
      title <-
        results[i] %>% html_nodes("a.result-title") %>% html_text() %>% gsub("<.*>", "", .) %>% iconv(., "latin1", "ASCII", sub =
                                                                                                        "")
    } #gsub and iconv removes special characters and emoji
    
    #here I nest some ifs to put the nearby and location elements in same column
    
    if (length(results[i] %>% html_nodes("span.result-hood") %>% html_text(trim = TRUE)) <= 0) {
      if (length(results[i] %>% html_nodes("span.nearby") %>% html_text(trim = TRUE)) <= 0) {
        location <- NA
      } else {
        location <-
          results[i] %>% html_nodes("span.nearby") %>% html_text(trim = TRUE)
      }
    } else {
      location <-
        results[i] %>% html_nodes("span.result-hood") %>% html_text(trim = TRUE)
    }
    
    
    if (length(results[i] %>% html_nodes("span.result-price") %>% html_text(trim = TRUE)) <= 0) {
      price <-
        NA
    } else {
      price <-
        results[i] %>% html_nodes("span.result-price") %>% html_text(trim = TRUE)
    }
    Sys.sleep(3) #just to not overload craigslist
    
    #create data frame for each row
    
    records[[i]] <-
      unique(data_frame(
        date = date,
        link = link,
        title = title,
        location = location,
        price = price
      ))
  }
  #end for loop
  dfresults <- bind_rows(records) #put all the rows into a df
  glimpse(dfresults) #take a look to see if its working
})
#end lapply function
FinalTable = as.data.frame(data.table::rbindlist(FinalTable)) #pull data out of all the enveloped lists and into one data frame
##########
#Cleaning#
##########
CleanTable <- FinalTable
CleanTable$price <-
  as.numeric(as.character(str_sub(CleanTable$price, 2))) #removes pound sign from price
CleanTable$location <-
  str_sub(CleanTable$location, 2, -2) # removes beginning and end brackets from location
CleanTable$location <-
  gsub(".* > ", "", CleanTable$location) #remove characters before " > " on location
CleanTable <-
  filter(CleanTable, is.na(price) |
           price <= 75) #Filter results to those charging less than £75 (the highest I saw a genuine s-f-r ad for was 70) & keep na
CleanTable <- unique(CleanTable) #Remove any duplicate Rows
CleanTable <-
  distinct(CleanTable, title, location, .keep_all = TRUE) #remove more duplicates which have different urls for same ad
CleanTable <-
  filter(
    CleanTable,!grepl("looking for", title) |
      !grepl("looking to", title) |
      !grepl("seeks", title)
  ) #Remove rows where people are "looking for" properties
CleanTable <-
  filter(CleanTable,!grepl("france", location) |
           !grepl("france", title)) #remove any rows with france as we are only looking at britain


##########################################
#Scrape individual pages for descriptions#
##########################################

#this bit was the most troublesome and slow as it sends a lot of requests to craigslist, which is why i cleaned first

urls <-
  as.vector(CleanTable[["link"]]) #takes the list of urls from the data frame we just made
desc.df <-
  data.frame(link = urls) #makes a new temporary data frame desc.df from the list of urls
#now we are applying our function to the list with lapply again and storing it in desc_text
bind_rows(lapply(urls, function(url) {
  Sys.sleep(runif(1, 24, 46)) # sets pause between 24-46 secs so craigslist don't get upset
  if (length(read_html(url[1]) %>%
             html_nodes(xpath = '//*[@id="postingbody"]') %>% html_text()) <= 0) {
    description <- NA
  }
  else{
    description <- read_html(url[1]) %>%
      html_nodes(xpath = '//*[@id="postingbody"]') %>% html_text()
  }
  print(description)
  data.frame(link = url,
             description = description)
  
})) -> desc_text

desc.df <-
  desc_text %>% left_join(desc.df) #merge descriptions with url list data frame
ActualFinalTable <-
  desc.df %>% left_join(CleanTable) #merge descritpions + url list with original data frame
#################
#Cleaning Part 2#
#################
#1 - trim descriptions
ActualFinalTable$description <- ActualFinalTable$description %>%
  gsub("\n|\r", " ", .) %>% #remove line break
  str_sub(., 64) %>% # trim beginning characters
  ActualFinalTable$description  <-
  gsub("<[^>]+>", "", ActualFinalTable$description) #remove special characters

#2 - remove irrelevant descriptions
ActualFinalTable <-
  filter(
    ActualFinalTable,!grepl("looking for", description) |
      !grepl("looking to", description) |
      !grepl("moving", description)
  ) #Remove rows where people are "looking for" properties
ActualFinalTable <-
  filter(
    ActualFinalTable,!grepl("free parking", description) |
      !grepl("free wifi", description)
  ) #common errors
#It is clear that location data is not uniform, so may have to export and manually go through it *sigh*
#write.csv(ActualFinalTable, "ActualFinalTable.csv")
#ActualFinalTable <- read_csv("ActualFinalTable.csv")
#created a column called location2 with corrected locations

############################
#Analysis and Vizualisation#
############################

#average posts a day
ActualFinalTable %>% count(date)
#=33
#67 / 33 = 2.03r


#1 Which city has the most / what is the location breakdown?

LocationTotals <-
  ActualFinalTable %>% group_by(location2) %>% tally(sort = TRUE) %>% data.frame() #creates data frame of location totals
LocationTotals %>% mutate(percentage = (n / 67) * 100) #adds percentage column (67 is number of rows)

#lets geocode that#

LocationTotals$location2 <-
  paste(LocationTotals$location2, ", UK", sep = "") #adds ", UK" at the end of location column just incase cities have same name abroad

for (i in 1:nrow(LocationTotals))
  #for loop that runs through adressed and fetches geocode
{
  result <-
    geocode(LocationTotals$location2[i],
            output = "latlona",
            source = "google") #uses ggmaps api
  LocationTotals$lon[i] <- as.numeric(result[1])
  LocationTotals$lat[i] <- as.numeric(result[2])
  LocationTotals$geoAddress[i] <- as.character(result[3])
  Sys.sleep(4) #stop rate limit
}

#lets map that [this was helpful= https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2/]
UK <-
  map_data("world") %>% filter(region == "UK") # fetches uk polygon
#GGPLOT TIME#
BasicMap <- ggplot() +
  geom_polygon(
    data = UK,
    aes(x = long, y = lat, group = group),
    fill = "grey",
    alpha = 0.3 
  ) +
  geom_point(data = LocationTotals,
             aes(
               x = lon,
               y = lat,
               size = n * 30,
               color = n
             ),
             alpha = 0.9) +
  scale_color_continuous() +
  scale_size(range = c(3, 10)) +
  theme_void() + ylim(50, 59) + coord_map()

ggsave(
  file = "basicmap.svg",
  plot = BasicMap,
  width = 10,
  height = 8
) #saves svg to edit and add labels in adobe illustrator

#####NOT WORTH IT AS TOO FEW DATA ################################
#2 Can We compare to average house price? [data from UK House Price Index]
#  Average_prices_2018_01 <- read_csv("Average-prices-2018-01.csv")
# LocationTotals$location2 <- str_sub(LocationTotals$location2, 1, -5) # remves the , UK bit
#  colnames(Average_prices_2018_01)[colnames(Average_prices_2018_01)=="Region_Name"] <- "location2"
#  LocationandHousePrice <- merge(LocationTotals, Average_prices_2018_01,by="location2")
#
# BasicPlot <- ggplot(LocationandHousePrice, aes(x=n, y=Average_Price)) +
#    geom_point(size=2, shape=23) #That was a fail#
##################################################################


############################################################
#TEXT ANALYSIS- what are the most frequent bigrams?        #
#This was Helpful https://uc-r.github.io/word_relationships#
############################################################

alladsBigrams <- tibble()  #create tibble called alladsBigrams
for (i in 1:nrow(ActualFinalTable)) #for each row in the table we made earlier
{
  clean <-
    tibble(text = ActualFinalTable$description[[i]]) %>% #take the description text
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% #grab the bigrams
    mutate(description = ActualFinalTable$description[[i]]) %>% #add description column
    select(description, everything()) 
  alladsBigrams <-
    rbind(alladsBigrams, clean) #bind together out bigrams into the tibble
}

top15bigrams <- alladsBigrams   %>% #show top 15 bigrams
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word |
           !word2 %in% stop_words$word) %>% #filters out common words but not totally (it's one word or the other)
  count(word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  top_n(15)

alladsWords <- tibble()   #same as before but with words
for (i in 1:nrow(ActualFinalTable)) {
  clean <- tibble(text = ActualFinalTable$description[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(description = ActualFinalTable$description[[i]]) %>%
    select(description, everything())
  alladsWords <- rbind(alladsWords, clean)
}

top15words <- alladsWords   %>% #show top 15 words
  anti_join(stop_words) %>% #remove banality with stop_words funtion again
  count(word, sort = TRUE) %>% #count the words
  top_n(15)

##############
#Plot Bigrams#
##############
#words
topwordsplot <- ggplot(top15words, aes(reorder(word, n), n)) +
  geom_bar(stat = "identity", fill = "#18BC9C") +
  labs(x = NULL, y = "Frequency") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="none")

ggsave(
  file = "topwords.svg",
  plot = topwordsplot,
  width = 10,
  height = 8
) #saves svg to edit and add labels in adobe illustrator

#bigrams
topbigramsplot <- ggplot(top15bigrams, aes(reorder(bigram, n), n)) +
  geom_bar(stat = "identity", fill = "#18BC9C") +
  labs(x = NULL, y = "Frequency") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="none")

ggsave(
  file = "topbigrams.svg",
  plot = topbigramsplot,
  width = 10,
  height = 8
) #saves svg to edit and add labels in adobe illustrator
