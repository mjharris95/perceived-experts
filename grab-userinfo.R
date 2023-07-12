library(data.table)
library(tidyverse)

# Creates a spreadsheet of user name, handle, and descriptions for all users
# included in the coengagement network (users_to_label.csv)

# Get all unique users who are included in the coengagement network
network_df <- read.csv("network/users-n10s2.csv", colClasses=c(rep("character",2)))
users <- unique(c(network_df$Source, network_df$Target))  

# Use ID dictionary to map back to numerical IDs assigned by Twitter (init_id)
users <- readRDS('../../ddd/dat/id_dict.rds') %>%
  filter(rand_id %in% users) %>%
  select(init_id) 

# Loop through user dataframes to collect relevant profile information
users_to_label <- lapply(1:30, function(day) 
              lapply(c("vaccines", "coronavirus"), function(df_name) 
                fread(paste0("../../ddd/dat/user_df_",df_name,"_2021-04-", sprintf("%02d", day), ".csv.gz")) %>%
                       filter(id %in% users$init_id) %>%
                       select(-type))) %>%
              do.call(rbind, .) %>%
      do.call(rbind, .) %>%
      distinct() 

# User profiles in spreadsheet should be randomly ordered
order_df <- users_to_label %>% select(id) %>% distinct()
set.seed(0113)
order_df$order <- sample(nrow(order_df))  
users_to_label %<>% left_join(order_df)
users_to_label$id <- sapply(users_to_label$id, function(x) toString(x))
write.csv(users_to_label, "user-lab/users_to_label.csv", row.names=FALSE)


#grab select 500 random users to validate
set.seed(0125)
sample_users <- sort(sample(length(unique(users_to_label$order)), 500, replace=FALSE))

df<-filter(users_to_label, order %in% sample_users) %>%
  arrange(order)

write.csv(df, "user-lab/validate-users.csv", row.names=FALSE)