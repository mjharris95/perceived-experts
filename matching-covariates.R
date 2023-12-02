library(igraph)
library(rgexf)
library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)
library(lubridate)

# Only included individual, English profiles from the two main communities
# change this line out to run for user-chars_n5s5 or user-chars n10s2
user_df <- read.csv("user-chars.csv") %>%
  filter(!is.na(is_PE) & !is.na(Community))

# convert back to Twitter numeric Ids, create vector of users to retrieve info for 
clustered_user_df <- readRDS('../../../../../ddd/dat/id_dict.rds') %>%
  right_join(user_df, by=c("rand_id"="Id")) %>%
  select(-rand_id) %>%
  rename(Id = init_id) 

these_ids <- readRDS('../../../../../ddd/dat/id_dict.rds') %>%
  filter(as.numeric(init_id) %in% clustered_user_df$Id) %>%
  select(-rand_id) 

# return to the raw Twitter data to collect additional covariates
# this is more efficient than retrieving additional covariates for all users at beginning
# inputs: vector of Twitter numeric Ids for users included in analysis (my_ids)
# and month and day of dataset to collect stats from
get_user_stats_byday<-function(month, day, my_ids){
  #because April 31st doesn't exist...
  if(month==4 & day==31){
    user_df_today<-data.frame()
  }
  
  else{
    # read in vaccines and coronavirus collections for the specified day
    # read in integer64 variables as characters to avoid numeric compression
    df1 <- fread(paste0("../../../../../ddd/dat/vaccines_2021-", sprintf("%02d", month), "-", sprintf("%02d", day), ".csv.gz"),
                 integer64="character") 
    
    df2<-fread(paste0("../../../../../ddd/dat/coronavirus_2021-", sprintf("%02d", month), "-", sprintf("%02d", day), ".csv.gz"),
               integer64="character")
    
    df<-rbind(df1, df2) 
    
    df %<>% mutate(user_id = as.numeric(user_id),
                   id = as.numeric(id),
                   retweeted_status_user_id = as.numeric(retweeted_status_user_id),
                   quoted_status_user_id = as.numeric(quoted_status_user_id))
    
    # generate dataframe of stats for specified users on the given day
    user_df_today<-select(df, ends_with(c("id", "favorite_count", "retweet_count",
                                          "followers_count",  "created_at", "verified"))) %>%
      select(!starts_with("in_reply_to")) %>%
      # will use to count the frequency of retweets from each user
      mutate(is_retweet = retweeted_status_id != "" & !is.na(retweeted_status_id)) %>%
      # rename variable to clarify difference between post and account creation time
      rename("post_created_at"="created_at") %>%
      # rename some variables to prepare for pivoting
      rename("orig-id"="id") %>%
      mutate(orig_user=user_id) %>%
      rename_at(vars(starts_with('user')), ~(paste0('orig-', .))) %>%
      rename_all(~(str_replace(., "quoted_status_", "quoted_status-"))) %>%
      rename_all(~(str_replace(., "retweeted_status_", "retweeted_status-"))) %>%
      rename("user_verified"="orig-user_verified", "user_created_at"="orig-user_created_at") %>%
      # pivot since observations of an original tweet due to retweets and quote tweets
      # give updated like and retweet counts for the original tweet
      pivot_longer(cols=-c("post_created_at", "is_retweet", "orig_user",
                           "user_created_at", "user_verified"),
                   names_sep="-",
                   names_to = c("type",".value")) %>%
      # filter just to users in our dataset
      filter(user_id %in% my_ids) %>%
      distinct() %>%
      arrange(post_created_at) 
    
    # do not include any tweets originally posted in May
    # (only retrieve quote tweets and retweets from May)
    if(month==5){
      user_df_today<-filter(user_df_today, type != "orig")
    }
  }  
  
  # update user on status of script by printing day function was run for
  print(paste(month, day, sep="-"))
  return(user_df_today)
}


# retrieve user stats by looping through all days in April and May
full_df <-lapply(4:5, function(month) 
  lapply(1:31, function(day) get_user_stats_byday(month, day, these_ids$init_id)) %>%
    do.call(rbind,.)) %>%
  do.call(rbind,.) 

#write.csv(full_df, '../../../../ddd/dat/tweets.csv', row.names=FALSE)
#full_df <- read.csv('../../../../ddd/dat/tweets.csv')

# go back to anonymous random identifier for users
full_df <- left_join(readRDS('../../../../../ddd/dat/id_dict.rds') %>%
                       mutate(unprecise_id = as.numeric(init_id)) %>%
                       filter(rand_id %in% user_df$Id) %>%
                       select(-init_id),
                     readRDS('../../../../../ddd/dat/id_dict.rds') %>%
                       mutate(unprecise_id = as.numeric(init_id)) %>%
                       select(-rand_id)) %>%
  distinct() %>%
  left_join(full_df %>% mutate(user_id=as.character(user_id)), by=c("init_id"="user_id")) %>%
  select(-c("init_id", "unprecise_id")) %>%
  rename(user_id = rand_id) %>%
  mutate(post_created_at = as_datetime(post_created_at))


# create a separate dataframe that is only original tweets
orig_df <- full_df %>% 
  filter(type=="orig") %>%
  select("post_created_at", "id", "user_id") %>%
  rename(orig_post_created_at = post_created_at) %>%
  distinct()



# only keep observations related to tweets originating in April
full_df <- full_df %>%
  filter(id %in% orig_df$id) 


# initialize user_stats, create variable for their initial follower count
user_stats <- full_df %>%
  group_by(user_id) %>%
  summarize(followers = first(na.omit(user_followers_count))) %>%
  right_join(user_df, by=c("user_id"="Id")) 

# for each user, find when their account was created
# whether they were ever verified or not verified during the time period
# total number of posts, total number of retweets
user_stats <- full_df %>%
  filter(type=="orig") %>%
  group_by(user_id) %>%
  summarize(user_created = head(user_created_at,1),
            ever_verified =  any(user_verified),
            ever_unverified = any(!user_verified),
            post_count = n(),
            retweet_count = sum(is_retweet, na.rm=TRUE)) %>%
  #did any users' verification status change during the time period?
  mutate(verif_change = ever_verified + ever_unverified) %>%
  right_join(user_stats)

# inititalize posting time variables for time of day that users post content
user_stats <- full_df %>% 
  filter(type=="orig") %>%
  # convert to Eastern time
  mutate(post_created_at = as_datetime(post_created_at) - hours(5)) %>%
  # retrieve hour when post was created
  mutate(post_created_at = hour(post_created_at)) %>%
  # for each user, find the number of posts created during a given time window
  group_by(user_id) %>%
  summarize(morning_count = sum(post_created_at >= 6 & post_created_at <12),
            afternoon_count = sum(post_created_at >= 12 & post_created_at <18),
            evening_count = sum(post_created_at >= 18 & post_created_at <=24),
            night_count = sum(post_created_at >= 0 & post_created_at <6)) %>%
  right_join(user_stats) %>%
  # if at least one third of posts were in a given time window, assign true 
  mutate(morning_poster = ifelse(morning_count > post_count/3, T, F),
         afternoon_poster = ifelse(afternoon_count > post_count/3, T, F),
         evening_poster = ifelse(evening_count > post_count/3, T, F),
         night_poster = ifelse(night_count > post_count/3, T, F)) 


#function to determine whether post days are uniformally distributed
uniform_days<-function(post_days){
  obs_counts <- table(factor(post_days, levels=1:30)) %>%
    as.numeric()
  
  exp<-rep(1/30, length.out=30)
  
  return(chisq.test(x=obs_counts, p=exp)$p.value)
}

# if p-value for uniform distribution of post days is less than .1, assign is_unif as true
user_stats <- orig_df %>% 
  mutate(post_day = day(orig_post_created_at)) %>%
  group_by(user_id) %>%
  summarize(post_unif = uniform_days(post_day)) %>%
  mutate(is_unif = ifelse(post_unif <=.1, FALSE, TRUE)) %>%
  right_join(user_stats, by="user_id")

orig_df %<>% 
  select(-"user_id")

#engagement counts
#create h-index formula
hindex <- function(x) {
  tx <- sort(x, decreasing = T)
  return(sum(tx >= seq_along(tx)))
}

# get original post time to subset to engagements (retweets, quote tweets) within 31 days of original post time)
eng_df <- full_df %>%
  left_join(orig_df, by="id") %>%
  filter(!is.na(orig_post_created_at)) %>%
  # difference in when engagement happened and when post was created
  mutate(time_diff = as.numeric(post_created_at-orig_post_created_at)) %>%
  # convert to days
  mutate(time_diff = floor(time_diff/(24*60*60))) %>%
  filter(time_diff <= 31) 

# get maximum favorite count for all posts
eng_df2 <- eng_df %>%
  select(user_id, id, favorite_count) %>%
  group_by(user_id, id) %>%
  slice_max(favorite_count, with_ties=FALSE, na_rm=TRUE)

# get maximum retweet count for all posts
eng_df2 <- eng_df %>%
  select(user_id, id, retweet_count) %>%
  group_by(user_id, id) %>%
  slice_max(retweet_count, with_ties=FALSE, na_rm=TRUE) %>%
  full_join(eng_df2)

rm(orig_df)
rm(eng_df)


user_stats <- eng_df2 %>%
  # replace NAs from full join with zeroes
  mutate(favorite_count = coalesce(favorite_count, 0),
         retweet_count = coalesce(retweet_count, 0)) %>%
  group_by(user_id) %>%
  # calculate median and h-index of likes and retweets across all posts by a given user
  summarize(med_likes = median(favorite_count,na.rm=TRUE),
            hind_likes = hindex(favorite_count),
            med_rts = median(retweet_count,na.rm=TRUE),
            hind_rts = hindex(retweet_count)) %>%
  right_join(user_stats, by="user_id")

#rm(full_df)

# append other user variables
user_stats <- user_df %>%
  right_join(user_stats %>% rename(Id=user_id))

# create additional variable from link analyses: number of posts with a URL
user_stats<-readRDS("link-counts-bigmods") %>%
  right_join(user_stats, by="Id") 


write.csv(user_stats, "users_formatch.csv", row.names=FALSE)

