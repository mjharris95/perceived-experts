library(data.table)
library(tidyverse)
library(magrittr)

# grab tweets from users in the two largest communities (pro- and anti- vaccine) across all coengagement networks
# note that Ids here are random Ids assigned by researchers for anonymization purposes
df <- read.csv("user-chars.csv")
df1 <- read.csv("user-charsn10s2.csv")
df2 <- read.csv("user-charsn5s5.csv")

df %<>% filter(!is.na(is_PE) & !is.na(Community)) %>% select(Id)
df1 %<>% filter(!is.na(is_PE) & !is.na(Community) & in_orig==FALSE) %>% select(Id)
df2 %<>% filter(!is.na(is_PE) & !is.na(Community) & in_orig==FALSE) %>% select(Id)

need_link_ids <- rbind(df, df1, df2) %>%
  distinct() %>%
  select(Id) %>%
  rename(rand_id = Id) 

# grab tweets from users in the two largest communities (pro- and anti- vaccine)
# note that Ids here are random Ids assigned by researchers for anonymization purposes
clustered_user_df <- readRDS('../../ddd/dat/id_dict.rds') %>%
  right_join(need_link_ids) %>%
  select(-rand_id) %>%
  rename(Id = init_id) 

clustered_user_df <- readRDS('../../ddd/dat/id_dict.rds') %>%
  filter(as.numeric(init_id) %in% clustered_user_df$Id) %>%
  select(-rand_id) %>%
  rename(Id = init_id)  %>%
  select(Id)

# helps avoid issues with numeric compression
#options(scipen=30)
# function to retrieve a specified a specific subset of tweets
# inputs: users whose tweets should be retrieved
#         day in April 2021 to retrieve tweets from
#         df_name collection to retrieve tweets from (coronavirus or vaccines)
# output: the tweet text, tweet id, user id, 
get_tweets_help<-function(users, day, df_name){
  df<-fread(paste0("../../ddd/dat/",df_name,"_2021-04-", sprintf("%02d", day), ".csv.gz"),
            colClasses=c("id"="character","user_id"="character")) %>%
    select('user_id', 'id', 'tweet', 'retweeted_status_user_id') %>%
    dplyr::filter(user_id %in% users & is.na(retweeted_status_user_id)) %>%
    select(-"retweeted_status_user_id") %>%
    #replace any tagged user name with "@X" to further anonymize
    mutate(tweet = str_replace_all(tweet, regex("\\B\\@\\w+"), "@X")) %>%
    rename("Id"="user_id",
           "tweet_id"="id")
  return(df)
}



# looping through all days in April and both collections, retrieve all retweeted tweets
# by users in the large anti- and pro-vaccine communities
# for each tweet, retrieve the highest retweet count observed during the study period
full_df<-lapply(1:30, function(day) 
  lapply(c("vaccines", "coronavirus"), function(df_name) get_tweets_help(clustered_user_df$Id, day, df_name))) %>%
  do.call(rbind, .) %>%
  do.call(rbind, .) %>% 
  group_by(tweet_id) %>%
  slice_sample(n=1) %>%
  ungroup() %>%
  select(tweet, Id) %>%
  rename(init_id = Id) 


# handles numerical precision issues to get back to original user id
full_df2 <- left_join(readRDS('../../ddd/dat/id_dict.rds') %>%
                        mutate(unprecise_id = as.numeric(init_id)) %>%
                        filter(rand_id %in% need_link_ids$rand_id) %>%
                        select(-init_id),
                      readRDS('../../ddd/dat/id_dict.rds') %>%
                        mutate(unprecise_id = as.numeric(init_id)) %>%
                        select(-rand_id)) %>%
  right_join(full_df) %>%
  select(-init_id) %>%
  rename(Id = rand_id)

full_df2 %<>% select(Id, tweet)

# csv of all tweets in main communities (anti and pro-vaccine) 
write.csv(full_df2, "tweets_by_users_in_network-bigmods.csv", row.names=FALSE)