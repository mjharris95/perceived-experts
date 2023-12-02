library(tidyverse)
library(igraph)
library(rgexf)
library(magrittr)

nw <- read.gexf("network/test-n10s2.gexf") %>%
  gexf.to.igraph()

# Infomap communities
df <- read.delim("network/infomap-output.txt") %>%
  rename(Id = node_id) %>%
  rowwise() %>%
  mutate(Id = toString(Id),
         path1 = strsplit(path, split=":")[[1]][1],
         path2 = strsplit(path, split=":")[[1]][2] %>% as.numeric(),
         path3 = strsplit(path, split=":")[[1]][3] %>% as.numeric()) %>%
  mutate(submod = paste(path1, path2),
         Community = ifelse(path1 == 1, "Anti-vaccine", NA)) %>%
  mutate(Community = ifelse(path1 == 2, "Pro-vaccine", Community)) %>%
  ungroup()

# Calculate bridge centrality - function
adj<-get.adjacency(nw)

bridge_count<-function(node_num){
  neighbors<-names(which(adj[,node_num]==1))
  
  counts<-filter(df, Id %in% neighbors) %>%
    filter(path1 %in% c("1","2")) %>%
    group_by(path1) %>%
    summarize(frq=n()) %>%
    select(frq)  %>%
    unlist() %>%
    as.numeric()
  
  bridge_score<-ifelse(length(counts)<2, 0, min(counts))
  return(bridge_score)
}


# Calculate centrality metrics
df <- data.frame(Id = names(nw[1]), 
                 Degree=as.numeric(degree(nw)), 
                 Betweenness = as.numeric(betweenness(nw)),
                 PageRank = as.numeric(page_rank(nw)$vector),
                 Bridge = sapply(1:length(names(nw[1])), function(x) bridge_count(x))) %>%
  rowwise() %>%
  right_join(df)

#Read in user labels, append using ID dictionary to connect back to profile labels
users <- readRDS('../../ddd/dat/id_dict.rds')

df<-read.csv("user-lab/user-labels-processed.csv", colClasses="character") %>% 
  distinct() %>%
  select(id, label, is_PE) %>%
  left_join(users, by=c("id" = "init_id")) %>%
  select(-id) %>%
  rename(Id = rand_id) %>%
  rowwise() %>%
  mutate(Id = toString (Id)) %>%
  # to organize key for supplementary visualization
  mutate(label = factor(label, 
                        levels= c("changed",
                                  "non-English",
                                  "non-individual",
                                  "individual, non-P.E.",
                                  "Description", 
                                  "Name, NOT \nDescription",
                                  "Name",
                                  "Both"
                        ))) %>%
  distinct() %>%
  right_join(df)


# stars to indicate the top X users by different centrality/bridging metrics
df_stars <- df %>%
  filter(!is.na(is_PE)) %>%
  # count perceived experts in top n ranked by metric
  arrange(desc(Bridge)) %>%
  select(Id, Bridge)

df_stars$Bridge_stars <- ""
df_stars$Bridge_stars[1:500]<-"*"
df_stars$Bridge_stars[1:50]<-"**"
df_stars$Bridge_stars[1:10]<-"***"
df %<>% left_join(df_stars)

for(my_metric in c("Degree", "Betweenness", "PageRank")){
  df_stars<-data.frame()
  for(my_comm in c("Anti-vaccine", "Pro-vaccine")){
    # split for anti and pro
    this_df_stars <- df %>%
      filter(!is.na(is_PE) & Community == my_comm) %>%
      arrange(desc(get(my_metric))) 
    
    this_df_stars[[paste0(my_metric, "_stars")]] <- ""
    this_df_stars[1:500, paste0(my_metric, "_stars")]<-"*"
    this_df_stars[1:50, paste0(my_metric, "_stars")]<-"**"
    df_stars<-rbind(this_df_stars, df_stars)
  }
  df %<>% left_join(df_stars)
}

df %<>%
  mutate(Bridge_stars = ifelse(is.na(Bridge_stars), "", Bridge_stars),
         Degree_stars = ifelse(is.na(Degree_stars), "", Degree_stars),
         Betweenness_stars = ifelse(is.na(Betweenness_stars), "", Betweenness_stars),
         PageRank_stars = ifelse(is.na(PageRank_stars), "", PageRank_stars))

write.csv(df, "user-chars.csv", row.names=FALSE)