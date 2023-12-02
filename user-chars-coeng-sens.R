library(rgexf)
library(tidyverse)
library(igraph)
library(data.table)
library(magrittr)

all_missing <- data.frame()

for(file_name in c("n5s5", "n10s2")){
  # read in rgexf file
  nw <- read.gexf(paste0(file_name, ".gexf")) %>%
    gexf.to.igraph()
  
  # append tags from prior labeling
  prior_labels <- read.csv("user-chars.csv",
                           colClasses=c("Id"="character")) 
  
  df <- data.frame("Id" = names(nw[1])) %>%
    left_join(prior_labels) %>%
    rename("old_path1"=path1) %>%
    mutate("in_orig"=ifelse(is.na(old_path1), FALSE, TRUE)) %>%
    select(Id, is_PE, old_path1, in_orig)
  
  write.csv(df, paste0("user-chars_", file_name, ".csv"))
  
  # identify ids that don't have tags - length
  missing_lab <- df %>% filter(in_orig==FALSE) %>% select(Id)

  all_missing <- rbind(all_missing, missing_lab)
  
  write.csv("coeng-sens-users.csv")
  
  # format to run informap
  infomap_submit <- get.data.frame(nw)[,c(1,2)]
  colnames(infomap_submit)<-c("source", "target")
  write_delim(infomap_submit, paste0("infomap-", file_name, ".csv"), delim=" ")
}

# get community assignment from infomap in browser and then append infomap community
for(file_name in file_names){    
  infomap_output <- read.table(paste("infomap", file_name, "output.txt", sep="-"), 
                               sep=" ", header=TRUE)
  
  user_df <- read.csv(paste0("user-chars_", file_name, ".csv"))
  
  user_df <- infomap_output %>% 
    mutate(path1 = str_split(path, ":")[[1]][1]) %>% 
    select(node_id, path1) %>%
    rename(Id = node_id, 
           path1 = module) %>%
    right_join(user_df, by="Id") %>%
    select(Id, path1, is_PE, old_path1, in_orig)
  
  # append new PE labels
  label_df <- read.csv("user-lab/coeng-sens-user-labels.csv", colClasses=c("id"="character"))
  
  # add community assignment in main text 
  user_df 
  mutate(old_comm = ifelse(in_orig == TRUE, "micro", NA)) %>%
    mutate(old_comm = ifelse(old_path1 == 1, "Anti-vaccine", old_comm)) %>%
    mutate(old_comm = ifelse(old_path1 == 2, "Pro-vaccine", old_comm)) -> user_chars
  
  for(id in label_df$id){
    if(id %in% user_chars$Id){
      user_chars$is_PE[user_chars$Id==id] <- label_df$is_PE[label_df$id==id]
    }
  }
  
  write.csv(user_chars, paste0("user-chars_", file_name, ".csv"), row.names=FALSE)
  
}

