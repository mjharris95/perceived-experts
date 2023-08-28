library(data.table)
library(tidyverse)
library(magrittr)

# grab tweets from users in the two largest communities (pro- and anti- vaccine)
# note that Ids here are random Ids assigned by researchers for anonymization purposes
clusters<-c("1", "2")

clustered_user_df<-read.csv("user-chars.csv") %>%
  select(Id, path1) %>%
  dplyr::rename(cluster=path1,
                users = Id) %>%
  filter(cluster %in% clusters)

# function to retrieve a specified a specific subset of tweets
# inputs: users whose tweets should be retrieved
#         day in April 2021 to retrieve tweets from
#         df_name collection to retrieve tweets from (coronavirus or vaccines)
# output: the tweet text, tweet id, user id, 
get_tweets_help<-function(users, day, df_name){
  df<-fread(paste0("../../../../ddd/dat/tweet_df_",df_name,"_2021-04-", sprintf("%02d", day), ".csv.gz")) %>%
    select(-"created_at")  %>%
    dplyr::filter(retweeted_status_user_id %in% users) %>% 
    group_by(tweet, retweeted_status_user_id, retweeted_status_id) %>%
    #for each tweets, retrieve the highest retweet count observed on a given day
    dplyr::summarize(retweet_count = max(retweeted_status_retweet_count)) %>%
    #replace any tagged user name with "@X" to further anonymize
    mutate(tweet = str_replace_all(tweet, regex("\\B\\@\\w+"), "@X")) %>%
    left_join(clustered_user_df, by=c("retweeted_status_user_id"="users")) 
  
  return(df)
}



# looping through all days in April and both collections, retrieve all retweeted tweets
# by users in the large anti- and pro-vaccine communities
# for each tweet, retrieve the highest retweet count observed during the study period
full_df<-lapply(1:30, function(day) 
  lapply(c("vaccines", "coronavirus"), function(df_name) get_tweets_help(clustered_user_df$users, day, df_name))) %>%
  do.call(rbind, .) %>%
  do.call(rbind, .) %>% 
  group_by(retweeted_status_id) %>%
  slice_max(retweet_count, n=1, with_ties=FALSE) %>%
  select(tweet, retweeted_status_id, retweeted_status_user_id, retweet_count) %>%
  rename(rand_id = retweeted_status_user_id) 

full_df <- readRDS('../../../../ddd/dat/id_dict.rds') %>%
  right_join(full_df) %>%
  select(-rand_id) %>%
  rename(Id = init_id)

# csv of all tweets in main communities (anti and pro-vaccine) 
write.csv(full_df, "tweets_by_users_in_network-bigmods.csv")
