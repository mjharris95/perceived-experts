library(foreach)
library(doParallel)
library(tidyverse)
library(magrittr)

# commands to run in parallel on cluster and detect number of cores available
if(Sys.getenv('SLURM_JOB_ID') != ""){
  registerDoParallel(cores = (Sys.getenv("SLURM_NTASKS_PER_NODE")))
}else{
  registerDoParallel(cores = 1)  
}

# input set of tweets (in this case, all tweets from the two largest communities)
tweet_df<-read.csv("tweets_by_users_in_network-bigmods.csv") %>%
            rename(init_id = Id) %>%
            mutate(init_id = as.character(init_id))

tweet_df<-readRDS('../../ddd/dat/id_dict.rds') %>%
  right_join(tweet_df) %>%
  select(-init_id) %>%
  rename(Id = rand_id)

# in parallel, run through all rows of a dataframe containing tweets with user Ids for person who posted them
links_check<-foreach(this_tweet=iter(tweet_df, by='row'), .combine=rbind) %dopar%{
  source("link-analysis.R")
  replace_tweet_url(as.character(this_tweet$tweet), this_tweet$Id, check=TRUE)
}

# save output as a dataframe
links_check %<>% 
  data.frame() %>%
  unnest() %>%
  arrange(Id)
  
saveRDS(links_check, "link-counts-bigmods", compress=TRUE)

