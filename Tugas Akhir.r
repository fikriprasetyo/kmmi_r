library(tidyverse)
library(tidytext)
library(textclean)
library(igraph)
library(ggraph)
library(rgexf)

# Dataset
  df1 <- read_csv("~/KMMI_R/kmmi_r/covid19_tweet.csv")
  df1 = df1 %>%
  select(screen_name, text)

# Filter Dataset
  # Sub topik 'vaksin'
    df11 <- dplyr::filter(df1,stringr::str_detect(text,"vaksin"))
    
  # Sub topik 'Pemerintah'
    df12 <- dplyr::filter(df1,stringr::str_detect(text,"Pemerintah"))
    
  # Sub topik 'Pandemi'
    df13 <- dplyr::filter(df1,stringr::str_detect(text,"Pandemi"))
    
# pre-processing
  # Sub topik 'vaksin'
    mentioned1 <- str_extract_all(string = df11$text,
                                 pattern = "(@[[:alnum:]_]*)", 
                                 simplify = TRUE)
    
    mentioned1 <- data.frame(mentioned1)
    mentioned1 <- mentioned1 %>%
      unite("mention", sep = " ")
    
    df11 = bind_cols(df11, mentioned1)
    df11$mention = replace_white(df11$mention)
    df11$mention = str_trim(string = df11$mention, side = "both") 
    glimpse(df11)
    
  # Sub topik 'Pemerintah'
    mentioned2 <- str_extract_all(string = df12$text,
                               pattern = "(@[[:alnum:]_]*)", 
                               simplify = TRUE)
 
    mentioned2 <- data.frame(mentioned2)
    mentioned2 <- mentioned2 %>%
      unite("mention", sep = " ")
 
    df12 = bind_cols(df12, mentioned2)
    df12$mention = replace_white(df12$mention)
    df12$mention = str_trim(string = df12$mention, side = "both")
    glimpse(df12)
  
  # Sub topik 'Pandemi'
    mentioned3 <- str_extract_all(string = df13$text,
                                  pattern = "(@[[:alnum:]_]*)", 
                                  simplify = TRUE)
    
    mentioned3 <- data.frame(mentioned3)
    mentioned3 <- mentioned3 %>%
      unite("mention", sep = " ")
    
    df13 = bind_cols(df13, mentioned3)
    df13$mention = replace_white(df13$mention)
    df13$mention = str_trim(string = df13$mention, side = "both")
    glimpse(df13)

# Tokenisasi target
  # Sub topik 'Vaksin'
    df2 = df11 %>%
      unnest_tokens(mentioned, mention, token = "words",
                    to_lower = FALSE, drop = FALSE)
    df3 = df2 %>%
      select("sumber" = screen_name, "target" = mentioned)
  
  # Sub topik 'Pemerintah'
    df22 = df12 %>%
      unnest_tokens(mentioned, mention, token = "words",
                    to_lower = FALSE, drop = FALSE)
    df33 = df22 %>%
      select("sumber" = screen_name, "target" = mentioned)
  
  # Sub topik 'Pandemi'
    df222 = df13 %>%
      unnest_tokens(mentioned, mention, token = "words",
                    to_lower = FALSE, drop = FALSE)
    df333 = df222 %>%
      select("sumber" = screen_name, "target" = mentioned)
  
# Membuat data network
  df_net1 = graph_from_data_frame(d = df3, directed = FALSE)
  df_net2 = graph_from_data_frame(d = df33, directed = FALSE)
  df_net3 = graph_from_data_frame(d = df333, directed = FALSE)

# Export File
  # Create a dataframe nodes : 1st column - node ID, 2nd column - node name
    # Sub topik 'vaksin'
      nodes_df1 <- data.frame(ID = c(1:vcount(df_net1)), NAME = V(df_net1)$name)
      vcount(df_net1)
      V(df_net1)$name
      
    # Sub topik 'Pemerintah'  
      nodes_df2 <- data.frame(ID = c(1:vcount(df_net2)), NAME = V(df_net2)$name)
      vcount(df_net2)
      V(df_net2)$name
      
    # Sub topik 'Pandemi'
      nodes_df3 <- data.frame(ID = c(1:vcount(df_net3)), NAME = V(df_net3)$name)
      vcount(df_net3)
      V(df_net3)$name
      
  # Create a dataframe edges : 1st column - source node ID
  # 2nd column - target node ID
    # Sub topik 'vaksin'
      ?get.edges
      edges_df1 <- as.data.frame(get.edges(df_net1, c(1:ecount(df_net1))))
      
    # Sub topik 'Pemerintah'
      ?get.edges
      edges_df2 <- as.data.frame(get.edges(df_net2, c(1:ecount(df_net2))))
      
    # Sub topik 'Pandemi'
      ?get.edges
      edges_df3 <- as.data.frame(get.edges(df_net3, c(1:ecount(df_net3))))
  
  # Simpan dengan nama nodes
    # Sub topik 'vaksin'
      write.gexf(nodes = nodes_df1, edges = edges_df1, defaultedgetype = "derected",
                 output = "Vaksin.gexf")
    # Sub topik 'Pemerintah'
      write.gexf(nodes = nodes_df2, edges = edges_df2, defaultedgetype = "derected",
                 output = "Pemerintah.gexf")
    # Sub topik 'Pandemi'
      write.gexf(nodes = nodes_df3, edges = edges_df3, defaultedgetype = "derected",
                 output = "Pandemi.gexf")
    


