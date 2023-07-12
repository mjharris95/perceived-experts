library(irr)
library(tidyverse)
library(magrittr)

# Takes in raw user codes and processes them to separate into categories
process_df <- function(file_path){
  df<-read.csv(file_path)
  
  df %<>%
    filter(name_label != "") %>%
    mutate(name_label=substr(name_label, 1, 1),
           descrip_label=substr(descrip_label, 1, 1)) %>%
    mutate(combo = paste0(name_label, descrip_label)) %>%
    add_column(label="") 
  

  
  df$label <- ifelse(df$name_label=="f" | df$descrip_label=="f", "non-english", df$label)
  df$label <- ifelse(df$label=="" & (df$name_label=="g" | df$descrip_label=="g"), "non-individual", df$label)
  df$label <- ifelse(df$label=="" & (df$name_label=="c" | df$descrip_label=="c"), "changed", df$label)
  df$label <- ifelse(df$label=="" & (df$name_label=="o" & df$descrip_label=="o"), "individual, non-P.E.", df$label)
  df$label <- ifelse(df$label=="" & (df$name_label=="p" & df$descrip_label=="p"), "Both", df$label)
  df$label <- ifelse(df$label=="" & (df$name_label=="o" & df$descrip_label=="p"), "Description", df$label)
  df$label[df$combo=="pu"] <- "Name"
  df$label[df$combo=="pn"] <- "Name, NOT \nDescription"
  df$is_PE <- ifelse(df$label == "individual, non-P.E.", FALSE, NA)
  df$is_PE <- ifelse(df$label %in% c("Both", "Description", "Name", "Name, NOT \nDescription"), TRUE, df$is_PE)  

  return(df)
}

#process user labels from three coders
process_df("user-lab/user-labels.csv") %>% 
  write.csv("user-lab/user-labels-processed.csv", row.names = FALSE)

process_df("user-lab/validate-users2.csv") %>%
  write.csv("user-lab/validate-users2-processed.csv", row.names = FALSE)

process_df("user-lab/validate-users3.csv") %>%
  write.csv("user-lab/validate-users3-processed.csv", row.names = FALSE)

# modify user label columns for comparison
df3<-read.csv("user-lab/validate-users3-processed.csv") %>%
  rename(name_label3 = name_label,
         descrip_label3 = descrip_label,
         is_PE3 = is_PE) %>%
  select(order, is_PE3) %>%
  distinct() 

df2<-read.csv("user-lab/validate-users2-processed.csv") %>%
  rename(name_label2 = name_label,
         descrip_label2 = descrip_label,
         is_PE2 = is_PE) %>%
  select(order, is_PE2) %>%
  distinct() 

df<-read.csv("user-lab/user-labels-processed.csv") %>%
  filter(order %in% df2$order) %>%
  select(is_PE, order) %>%
  distinct()

# create a combined dataframe with tags for all three coders
# column names are numbered to correspond to each
# column names are the specific category (differentiates between different types of excluded users)
# is_indiv is a binary variable for whether the user is included in analysis
# cat is either "1" for perceived experts, "2" for perceived non-experts, and "3" is users excluded from analysis

combo_df <- left_join(df3, df2, by="order") %>%
  left_join(df, by="order")

write.csv(combo_df, "user-lab/validate-users-all.csv", row.names = FALSE)

# calculates Fleiss' kappa based on the "cat" columns
combo_df %>%
  select(is_PE, is_PE2, is_PE3) %>%
  rowwise() %>%
  mutate(is_PE = toString(is_PE),
          is_PE2 = toString(is_PE2),
          is_PE3 = toString(is_PE3)) %>%
  kappam.fleiss()

