library(tidyverse)
library(RCurl)
library(magrittr)

#list of low and very low quality sources
iffy_df<-read.csv("link-lists/iffy-ratings.csv") %>% 
  filter(MBFC1 %in% c("low", "very-low"))

# append https:// to beginning to match tweet link format
iffy_df$Domain.name <- sapply(iffy_df$Domain.name, function(x) paste0("https://", x))

acad_urls<-readRDS("link-lists/journal_links.rds")
  
# function to get URL destination via https://stackoverflow.com/posts/34383991/revisions
unshorten_url <- function(uri){
  require(RCurl)

  # listCurlOptions()
  opts <- list(
    followlocation = TRUE,  # resolve redirects
    ssl.verifyhost = FALSE, # suppress certain SSL errors
    ssl.verifypeer = FALSE, 
    nobody = TRUE, # perform HEAD request
    verbose = FALSE,
    timeout = 3, 
    maxredirs = 4
  );
  tryCatch({
    curlhandle = getCurlHandle(.opts = opts)
    getURL(uri, curl = curlhandle, .opts = opts)
    info <- getCurlInfo(curlhandle)
    rm(curlhandle)  # release the curlhandle!
    info$effective.url},
    error=function(e){"[unknown link]"}
  )
}


# function to extend hyperlinks in tweets
# inputs: Tweet, user Id, and whether or not to check source type (default to false)
# returns: tweet with hyperlinks extended if possible (if not checking source type)
# if checking source type, also returns the user Id, number of links in tweet
# number of links checked, number of links that were low quality ("iffy"), 
# and number of links that were academic


replace_tweet_url<-function(tweet, Id, check=FALSE){
  
  #identify whether there are hyperlinks
  has_links<-str_detect(tweet, "https://t.co")
  
  # if not, return original tweet
  if(!has_links){
    if(check==FALSE){
      return(tweet)
    }
    else{
      return(list(tweet=tweet, Id=Id, url_num=0, url_checked=0, url_iffy=0, url_acad=0))
    }
  }
  
  
  else{
    #if there are hyperlinks, identify and extend them
    words<-strsplit(tweet, split = " ")[[1]] 
    urls<-words[startsWith(words, "https://t.co")]
    url_longer<-c()
    
    for(i in 1:length(urls)){
      url_longer[i]<-unshorten_url(urls[i])
    }
    
    # replace twitter links with "[twitter link]", do not check source type
    twitter_links<-startsWith(url_longer, "https://twitter.com/")
    
    if(sum(twitter_links)>0){
      url_longer[twitter_links]<-"[twitter link]"
    }
    
    #check source type if request
    if(check==TRUE){  
      library(stringr)
      
      url_num<-length(url_longer)-sum(twitter_links)
      url_checked<-sum(startsWith(url_longer, "https://"))
      
      url_iffy<-0
      url_acad<-0 
      
      #loop through all extended hyperlinks
      for(url in url_longer[startsWith(url_longer, "https://")]){
        
        # string matching to identify and count low quality sources
        if(startsWith(url, iffy_df$Domain.name) %>% any()){
          url_iffy<-url_iffy+1
        }
        
        # string matching to identify and count academic sources
        if(startsWith(url, acad_urls) %>% any()){
          url_acad<-url_acad+1
        }
      }
    }
    
    
    # insert extended hyperlink back in text
    words[startsWith(words, "https://t.co")]<-unlist(url_longer)
    tweet<-paste(words, collapse=" ")
    
    if(check==FALSE){
      return(tweet)
    }
    
    else{
      return(list(Id=Id, url_num=url_num, url_checked=url_checked, url_iffy=url_iffy, url_acad=url_acad))
    }
  }
}

