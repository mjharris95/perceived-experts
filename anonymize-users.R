# Anonymizes tweets and stores a private dictionary (init_id) to map between 
# Twitter's random IDs and the anonymous ones we assigned. 

library(tidyverse)
library(magrittr)
library(plyr)
library(data.table)

# Initialize ID dictionary, which connects user numerical IDs assigned by Twitter (init_id)
# to the random IDs we assign to users (rand_id). ID dictionary is stored privately to maintain
# anonymity of users in study. 

id_dict <- data.frame(init_id=integer(),
                     rand_id=integer())

# Will be a directed graph where each edge connects a user who was retweeted (target)
# the user who retweeted them (source), used to construct coengagement network
graph <- data.frame()

# For each day in April, read in tweet data from two collections: vaccines and coronavirus
for(i in 1:30){
  # Workflow for vaccines collection, read in tweets
  df <- fread(paste0("../../ddd/dat/vaccines_2021-04-", sprintf("%02d", i), ".csv.gz"))
  
  # To construct a coengagement network based on retweets, we collect all retweets in the collection
  df %<>% select('id', 'created_at', 'tweet', 'user_id', 'user_screen_name', 'user_name', 'user_description',
                 'retweeted_status_id', 'retweeted_status_user_id','retweeted_status_user_screen_name',
                 'retweeted_status_user_name', 'retweeted_status_user_description', "retweeted_status_retweet_count",
                 "retweeted_status_favorite_count") %>%
    #filters to retweets
    filter(!is.na(retweeted_status_user_id)) %>%
    mutate(user_id = as.character(user_id),
           id = as.character(id),
           retweeted_status_user_id = as.character(retweeted_status_user_id))
  
  # Identify any users who tweeted during the time period who have not already
  # been designated an anonymous identifier
  new_ids <- unique(c(df$user_id, df$retweeted_status_user_id))
  unique_new_ids<-setdiff(new_ids, id_dict$init_id)
  sample<-sample(seq(from=length(id_dict$init_id)+1, length=length(unique_new_ids)),
           size=length(unique_new_ids), replace=FALSE)
  id_dict %<>% rbind(data.frame(init_id=unique_new_ids,
                                rand_id=sample))
  
  saveRDS(id_dict, "../../ddd/dat/id_dict.rds")
  
  # Separate user information from tweets and store
  user_df<-select(df, ends_with("user_id") | ends_with("name") | ends_with("description")) %>%
    rename_all(~(str_replace(., "user_", "user-"))) %>%
    pivot_longer(everything(),
                 names_sep="-",
                 names_to = c("type",".value")) %>%
    distinct()
  
  fwrite(user_df, paste0("../../ddd/dat/user_df_vaccines_2021-04-", sprintf("%02d", i), ".csv.gz"))

  # Separate tweets from user information and store
  tweet_df <- select(df, c(retweeted_status_id, tweet, created_at, user_id, retweeted_status_user_id,
                           retweeted_status_retweet_count, retweeted_status_favorite_count))
  
  # Store ids of tweets from vaccines collection to prevent duplicates when adding coronavirus collection
  these_tweets<-df$id
  
  rm(df)
  
  # Switch user ids to our random IDs
  tweet_df %<>% mutate(user_id = mapvalues(user_id,from=id_dict$init_id, to=id_dict$rand_id, warn_missing=FALSE),
                       retweeted_status_user_id = mapvalues(retweeted_status_user_id,from=id_dict$init_id, to=id_dict$rand_id, warn_missing=FALSE))
  
  fwrite(tweet_df, paste0("../../ddd/dat/tweet_df_vaccines_2021-04-", sprintf("%02d", i), ".csv.gz"))
  
  # Add to directed graph of retweets (used to construct coengagement network)
  graph <- tweet_df %>% 
    select(c(user_id, retweeted_status_user_id)) %>%
    dplyr::rename(source=user_id, target=retweeted_status_user_id) %>%
    rbind(graph)

  rm(tweet_df)
  
  #Identical workflow for coronavirus collection, read in tweets
  df <- fread("../../ddd/dat/coronavirus_2021-04-01.csv.gz")
  
  # To construct a coengagement network based on retweets, we collect all retweets in the collection
  df %<>% select('id', 'created_at', 'tweet', 'user_id', 'user_screen_name', 'user_name', 'user_description',
                 'retweeted_status_id', 'retweeted_status_user_id','retweeted_status_user_screen_name',
                 'retweeted_status_user_name', 'retweeted_status_user_description', 
                 "retweeted_status_retweet_count","retweeted_status_favorite_count") %>%
    filter(!is.na(retweeted_status_user_id)) %>%
    # prevent duplication of tweets between the two collections
    filter(! id %in% these_tweets) %>%
    mutate(user_id = as.character(user_id),
           id = as.character(id),
           retweeted_status_user_id = as.character(retweeted_status_user_id))
  
  # Identify any users who tweeted during the time period who have not already
  # been designated an anonymous identifier
  new_ids <- unique(c(df$user_id, df$retweeted_status_user_id))
  unique_new_ids<-setdiff(new_ids, id_dict$init_id)
  sample<-sample(seq(from=length(id_dict$init_id)+1, length=length(unique_new_ids)),
                 size=length(unique_new_ids), replace=FALSE)
  id_dict %<>% rbind(data.frame(init_id=unique_new_ids,
                                rand_id=sample))

  saveRDS(id_dict, "../../ddd/dat/id_dict.rds")
  
  # Separate user information from tweets and store
  user_df<-select(df, ends_with("user_id") | ends_with("name") | ends_with("description")) %>%
    rename_all(~(str_replace(., "user_", "user-"))) %>%
    pivot_longer(everything(),
                 names_sep="-",
                 names_to = c("type",".value")) %>%
    distinct()
 
   fwrite(user_df, paste0("../../ddd/dat/user_df_coronavirus_2021-04-", sprintf("%02d", i), ".csv.gz"))
   rm(user_df)
  
  # Separate tweets from user information and store
  tweet_df <- select(df, c(retweeted_status_id, tweet, created_at, user_id, retweeted_status_user_id,
                           retweeted_status_favorite_count, retweeted_status_retweet_count))
  
  rm(df)
  
  # Switch user ids to our random IDs
  tweet_df %<>% mutate(user_id = mapvalues(user_id,from=id_dict$init_id, to=id_dict$rand_id, warn_missing=FALSE),
                       retweeted_status_user_id = mapvalues(retweeted_status_user_id,from=id_dict$init_id, to=id_dict$rand_id, warn_missing=FALSE))
  
  fwrite(tweet_df, paste0("../../ddd/dat/tweet_df_coronavirus_2021-04-", sprintf("%02d", i), ".csv.gz"))
  
  # Add to directed graph of retweets (used to construct coengagement network)
  graph <- tweet_df %>% 
    select(c(user_id, retweeted_status_user_id)) %>%
    dplyr::rename(source=user_id, target=retweeted_status_user_id) %>%
    rbind(graph)
  
  fwrite(graph, "network/fordocker.csv")
  rm(tweet_df)
}