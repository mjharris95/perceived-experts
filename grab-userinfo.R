library(data.table)
library(tidyverse)
library(magrittr)

# Creates a spreadsheet of user name, handle, and descriptions for all users
# included in the coengagement network (users_to_label.csv)

# Get all unique users who are included in the coengagement network
network_df <- read.csv("network/users-n2s10.csv", colClasses=c(rep("character",2)))
nw_users <- unique(c(network_df$Source, network_df$Target))  

# Use ID dictionary to map back to numerical IDs assigned by Twitter (init_id)
users <- readRDS('../../ddd/dat/id_dict.rds') %>%
  filter(rand_id %in% nw_users) %>%
  select(init_id) 

# solve numerical precision error by also taking user_ids for original posts, which may be rounded
users <- readRDS('../../ddd/dat/id_dict.rds') %>%
  filter(as.numeric(init_id) %in% users$init_id) %>%
  select(init_id) 

# Loop through user dataframes to collect relevant profile information
users_to_label <- lapply(1:30, function(day) 
              lapply(c("vaccines", "coronavirus"), function(df_name) 
                fread(paste0("../../ddd/dat/user_df_",df_name,"_2021-04-", sprintf("%02d", day), ".csv.gz")) %>%
                       filter(id %in% users$init_id) %>%
                       select(-type))) %>%
              do.call(rbind, .) %>%
      do.call(rbind, .) %>%
      mutate(id = as.numeric(id)) %>%
      distinct() %>%
      group_by(screen_name, name, description) %>%
      slice_max(id, n=1, with_ties=FALSE)
     

# User profiles in spreadsheet should be randomly ordered
order_df <- data.frame(id = unique(users_to_label$id))
set.seed(0113)
order_df$order <- sample(nrow(order_df))  
users_to_label %<>% left_join(order_df)
users_to_label$id <- sapply(users_to_label$id, function(x) toString(x))
write.csv(users_to_label, "user-lab/users_to_label2.csv", row.names=FALSE)


#grab select 500 random users to validate
sample_users <- sort(sample(length(unique(users_to_label$order)), 500, replace=FALSE))
df<-filter(users_to_label, order %in% sample_users) %>%
  arrange(order)

write.csv(df, "user-lab/validate-users.csv", row.names=FALSE)
