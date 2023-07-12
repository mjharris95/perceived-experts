library(igraph)
library(rgexf)
library(tidyverse)
library(networktools)
library(RCurl)
library(magrittr)

df <- read.csv("user-chars.csv")

#10 top PE and non-PE individuals in both main communities
cent_df <- df %>%  
  filter(!is.na(is_PE) & !is.na(Community)) %>%
  group_by(is_PE, Community) %>%
  slice_max(Degree, n=10, with_ties=FALSE)  %>%
  arrange(Community, is_PE) 

#write.csv(cent_df, "central-users.csv")

id_dict <- readRDS('../../ddd/dat/id_dict.rds') 

#grab their 10 top tweets
top_tweets<-read.csv("tweets_by_users_in_network-bigmods.csv") %>%
  rename(init_id = Id) %>%
  mutate(init_id = as.character(init_id)) %>%
  left_join(id_dict) %>%
  select(-init_id) %>%
  rename(Id = rand_id) %>%
  filter(Id %in% cent_df$Id) %>%
  group_by(Id) %>%
  slice_max(retweet_count, n=10, with_ties=FALSE) %>%
  arrange(factor(Id, levels = cent_df$Id))

#unshorten urls
source("link-analysis.R")

for(i in 1:nrow(top_tweets)){
  this_tweet<-top_tweets[i,]
  top_tweets$tweet[i] <- replace_tweet_url(as.character(this_tweet$tweet), this_tweet$Id, check=FALSE)
}

# Randomize order
top_tweets$order<-sample(dim(top_tweets)[1])

top_tweets %<>% arrange(desc(order)) %>%
  select(tweet, Id, order)

# This file is excluded from Github to maintain anonymity
write.csv(top_tweets, "tweet-lab/top_tweets.csv")

# Sample of 10 to paraphrase per category (commented to avoid overwriting paraphrased tweets)

# user_df <- read.csv("user-chars.csv")
# 
# labeled_tweets <- read.csv("tweet_stance.csv")
# 
# labeled_tweets %>%
#   left_join(user_df) %>%
#   group_by(Community, is_PE) %>%
#   slice_sample(n=10) %>%
#   ungroup() %>%
#   select(tweet, stance_1, stance_2, stance_3) %>%
#   write.csv("tweet-lab/paraphrased-tweets.csv")

